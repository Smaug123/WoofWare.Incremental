namespace WoofWare.Incremental.Test

open WoofWare.Incremental

      module Expert = Expert
      module E = Expert

      module _ = struct
        (* This tests add_dependency/remove_dependency, invalidity (in particular a node
             becomes invalid before being replaced by a valid one). *)
        include Join (struct
            let join : type a. a t t -> a t =
              fun t ->
              let prev_rhs = ref None in
              let join =
                E.Node.create (fun () -> E.Dependency.value (Option.value_exn !prev_rhs))
              in
              let lhs_change =
                map t ~f:(fun rhs ->
                  let rhs_dep = E.Dependency.create rhs in
                  E.Node.add_dependency join rhs_dep;
                  Option.iter !prev_rhs ~f:(fun v -> E.Node.remove_dependency join v);
                  prev_rhs := Some rhs_dep)
              in
              E.Node.add_dependency join (E.Dependency.create lhs_change);
              E.Node.watch join
            ;;
          end)

        let%expect_test _ =
          (* plugging an already invalid incremental node make
               the expert node invalid *)
          let t = E.Node.create ignore in
          E.Node.add_dependency t (E.Dependency.create invalid);
          assert (is_invalid (E.Node.watch t))
        ;;

        let%expect_test _ =
          (* [invalidate] does invalidate *)
          let var = Var.create `Valid in
          let result = E.Node.create ignore in
          let result_incr = E.Node.watch result in
          let lhs_change =
            map (Var.watch var) ~f:(function
              | `Valid -> ()
              | `Invalid -> E.Node.invalidate result)
          in
          E.Node.add_dependency result (E.Dependency.create lhs_change);
          let o = observe result_incr in
          stabilize_ [%here];
          Observer.disallow_future_use o;
          assert (is_valid result_incr);
          Var.set var `Invalid;
          assert (is_invalid result_incr)
        ;;

        (* This tests
             - whether we can actually write such a thing with the intf of incremental
             - add_dependency/remove_dependency with an actually variable set of
               children, unlike join
             - incremental doesn't needlessly schedule certain nodes, destroying the good
               complexities we're trying to get.
             - behavior when observability if turned off and on
        *)
        let map_filter_mapi
          : type k v1 v2.
            on_event:
              ([ `Left of k
               | `Lhs_change
               | `Main
               | `On_change of k * v2 option
               | `Per_key of k
               | `Right of k
               | `Unequal of k
               ]
               -> unit)
            -> (k, v1, 'comparator) Map.t t
            -> f:(k -> v1 t -> v2 option t)
            -> (k, v2, 'comparator) Map.t t
          =
          fun ~on_event lhs ~f ->
          let prev_map = ref None in
          let prev_nodes = ref None in
          let acc = ref None in
          let result =
            E.Node.create (fun () ->
              on_event `Main;
              Option.value_exn !acc)
          in
          let rec lhs_change =
            lazy
              (map lhs ~f:(fun map ->
                 on_event `Lhs_change;
                 let empty_map =
                   Map.Using_comparator.empty ~comparator:(Map.comparator map)
                 in
                 if Option.is_none !acc then acc := Some empty_map;
                 let symmetric_diff =
                   Map.symmetric_diff
                     ~data_equal:phys_equal
                     (Option.value !prev_map ~default:empty_map)
                     map
                 in
                 let new_nodes =
                   Sequence.fold
                     symmetric_diff
                     ~init:(Option.value !prev_nodes ~default:empty_map)
                     ~f:(fun nodes (key, changed) ->
                       match changed with
                       | `Unequal _ ->
                         on_event (`Unequal key);
                         let node, _dep = Map.find_exn nodes key in
                         E.Node.make_stale node;
                         nodes
                       | `Left _ ->
                         on_event (`Left key);
                         let node, dep = Map.find_exn nodes key in
                         let nodes = Map.remove nodes key in
                         E.Node.remove_dependency result dep;
                         acc := Some (Map.remove (Option.value_exn !acc) key);
                         E.Node.invalidate node;
                         nodes
                       | `Right _ ->
                         on_event (`Right key);
                         let node =
                           E.Node.create (fun () ->
                             on_event (`Per_key key);
                             Map.find_exn (Option.value_exn !prev_map) key)
                         in
                         E.Node.add_dependency
                           node
                           (E.Dependency.create (force lhs_change));
                         let user_function_dep =
                           E.Dependency.create
                             (f key (E.Node.watch node))
                             ~on_change:(fun opt ->
                               on_event (`On_change (key, opt));
                               let old = Option.value_exn !acc in
                               acc
                               := Some
                                    (match opt with
                                     | None ->
                                       if Map.mem old key then Map.remove old key else old
                                     | Some v -> Map.set old ~key ~data:v))
                         in
                         E.Node.add_dependency result user_function_dep;
                         Map.set nodes ~key ~data:(node, user_function_dep))
                 in
                 prev_nodes := Some new_nodes;
                 prev_map := Some map))
          in
          E.Node.add_dependency result (E.Dependency.create (force lhs_change));
          E.Node.watch result
        ;;

        let%expect_test _ =
          let module M =
            On_update_queue (struct
              type 'a t =
                [ `Left of string
                | `Lhs_change
                | `Main
                | `On_change of string * int option
                | `Per_key of string
                | `Right of string
                | `Unequal of string
                ]
              [@@deriving compare, sexp_of]
            end)
          in
          let push, check = M.on_update_queue () in
          let var = Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2; "c", 3 ]) in
          let increment = Var.create 1 in
          let assert_incremental_computation_is_correct observer =
            let from_scratch =
              let map = Var.latest_value var in
              let j = Var.latest_value increment in
              Map.filter_mapi map ~f:(fun ~key:_ ~data:i ->
                if (i + j) mod 10 = 0 then None else Some (i + j))
            in
            [%test_result: int String.Map.t] (value observer) ~expect:from_scratch
          in
          let result =
            map_filter_mapi ~on_event:push (Var.watch var) ~f:(fun _key value_incr ->
              map2 value_incr (Var.watch increment) ~f:(fun i j ->
                if (i + j) mod 10 = 0 then None else Some (i + j)))
          in
          let o = observe result in
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change
            ; `Right "a"
            ; `Right "b"
            ; `Right "c"
            ; `Per_key "c"
            ; `Per_key "b"
            ; `Per_key "a"
            ; `On_change ("a", Some 2)
            ; `On_change ("b", Some 3)
            ; `On_change ("c", Some 4)
            ; `Main
            ];
          let update f = Var.set var (f (Var.value var)) in
          update (fun map -> Map.set map ~key:"b2" ~data:12);
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change; `Right "b2"; `Per_key "b2"; `On_change ("b2", Some 13); `Main ];
          update (fun map -> Map.set map ~key:"b2" ~data:18);
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change
            ; `Unequal "b2"
            ; `Per_key "b2"
            ; `On_change ("b2", Some 19)
            ; `Main
            ];
          update (fun map -> Map.set map ~key:"b2" ~data:19);
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change; `Unequal "b2"; `Per_key "b2"; `On_change ("b2", None); `Main ];
          update (fun map -> Map.set map ~key:"b2" ~data:18);
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change
            ; `Unequal "b2"
            ; `Per_key "b2"
            ; `On_change ("b2", Some 19)
            ; `Main
            ];
          update (fun map -> Map.remove map "b2");
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check [ `Lhs_change; `Left "b2"; `Main ];
          Var.set increment 9;
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `On_change ("a", None)
            ; `On_change ("c", Some 12)
            ; `On_change ("b", Some 11)
            ; `Main
            ];
          update (fun map -> Map.remove map "a");
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check [ `Lhs_change; `Left "a"; `Main ];
          update (fun map ->
            Map.set (Map.remove (Map.remove map "b") "c") ~key:"a" ~data:2);
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change
            ; `Right "a"
            ; `Left "b"
            ; `Left "c"
            ; `Per_key "a"
            ; `On_change ("a", Some 11)
            ; `Main
            ];
          disallow_future_use o;
          stabilize_ [%here];
          check [];
          update (fun map -> Map.set map ~key:"a" ~data:3);
          stabilize_ [%here];
          check [];
          let o = observe result in
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Lhs_change; `Unequal "a"; `Per_key "a"; `On_change ("a", Some 12); `Main ]
        ;;

        (* This one checks
             - expressivity of the interface, again
             - on_observability_change callback
        *)
        let staged_eq
          : type a.
            on_event:
              ([ `Scheduling
               | `Is_eq of a
               | `Add_reverse_dep of a
               | `Remove_reverse_dep of a
               ]
               -> unit)
            -> a Hashtbl.Hashable.t
            -> a t
            -> (a -> bool t) Staged.t
          =
          fun ~on_event hashable incr ->
          let last = ref None in
          let reverse_dependencies = Hashtbl.Using_hashable.create ~hashable () in
          let scheduling_node =
            I.map incr ~f:(fun v ->
              on_event `Scheduling;
              Option.iter !last ~f:(fun old_v ->
                Option.iter (Hashtbl.find reverse_dependencies old_v) ~f:E.Node.make_stale);
              Option.iter (Hashtbl.find reverse_dependencies v) ~f:E.Node.make_stale;
              last := Some v)
          in
          Staged.stage (fun a ->
            let rec result =
              lazy
                (E.Node.create
                   (fun () ->
                     let v = Option.value_exn !last in
                     on_event (`Is_eq a);
                     hashable.compare a v = 0)
                   ~on_observability_change:(fun ~is_now_observable ->
                     if is_now_observable
                     then (
                       on_event (`Add_reverse_dep a);
                       Hashtbl.add_exn reverse_dependencies ~key:a ~data:(force result))
                     else (
                       on_event (`Remove_reverse_dep a);
                       Hashtbl.remove reverse_dependencies a)))
            in
            let dep = E.Dependency.create scheduling_node in
            E.Node.add_dependency (force result) dep;
            E.Node.watch (force result))
        ;;

        let%expect_test _ =
          let module M =
            On_update_queue (struct
              type 'a t =
                [ `Scheduling
                | `Is_eq of int
                | `Add_reverse_dep of int
                | `Remove_reverse_dep of int
                ]
              [@@deriving compare, sexp_of]
            end)
          in
          let push, check = M.on_update_queue () in
          let var = Var.create 2 in
          let switch = Var.create true in
          let input = if_ (Var.watch switch) ~then_:(Var.watch var) ~else_:invalid in
          let is_focused = Staged.unstage (staged_eq Int.hashable ~on_event:push input) in
          let all = all (List.init 5 ~f:is_focused) in
          let assert_incremental_computation_is_correct observer =
            let from_scratch = List.init 5 ~f:(( = ) (Var.latest_value var)) in
            [%test_result: bool list] (value observer) ~expect:from_scratch
          in
          let o = observe all in
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Add_reverse_dep 4
            ; `Add_reverse_dep 3
            ; `Add_reverse_dep 2
            ; `Add_reverse_dep 1
            ; `Add_reverse_dep 0
            ; `Scheduling
            ; `Is_eq 0
            ; `Is_eq 1
            ; `Is_eq 2
            ; `Is_eq 3
            ; `Is_eq 4
            ];
          Var.set var 0;
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check [ `Scheduling; `Is_eq 0; `Is_eq 2 ];
          disallow_future_use o;
          stabilize_ [%here];
          check
            [ `Remove_reverse_dep 4
            ; `Remove_reverse_dep 3
            ; `Remove_reverse_dep 2
            ; `Remove_reverse_dep 1
            ; `Remove_reverse_dep 0
            ];
          Var.set var 1;
          stabilize_ [%here];
          check [];
          let o = observe all in
          stabilize_ [%here];
          assert_incremental_computation_is_correct o;
          check
            [ `Add_reverse_dep 4
            ; `Add_reverse_dep 3
            ; `Add_reverse_dep 2
            ; `Add_reverse_dep 1
            ; `Add_reverse_dep 0
            ; `Scheduling
            ; `Is_eq 1
            ; `Is_eq 0
            ];
          Var.set switch false;
          stabilize_ [%here];
          assert (skip_invalidity_check || not (is_valid all));
          disallow_future_use o
        ;;

        (* Ensure we can make changes to an unnecessary expert node from a necessary
             child. *)
        let%expect_test _ =
          let weird_unzip
            : type a b c. a list t -> (a t -> (b * c) t) -> b list t * c list t
            =
            (* This function doesn't do anything really interesting, the point is more
                 that the caller can make it call add_dependency or remove_dependency as
                 desired. And we can check the correctness of the results. *)
            fun t f ->
            let f_result = ref None in
            let fs1 = ref None in
            let fs2 = ref None in
            let parent1 = E.Node.create (fun () -> Option.value_exn !fs1 ()) in
            let parent2 = E.Node.create (fun () -> Option.value_exn !fs2 ()) in
            let lhs_change =
              map t ~f:(fun l ->
                (match !f_result with
                 | Some (len, (deps1, deps2)) when len <> List.length l ->
                   (* remove_dependency does something different for the last
                        child. So iterate in different orders so we cover both cases. *)
                   List.iter deps1 ~f:(E.Node.remove_dependency parent1);
                   List.iter (List.rev deps2) ~f:(E.Node.remove_dependency parent2);
                   f_result := None
                 | _ -> ());
                match !f_result with
                | Some _ -> ()
                | None ->
                  let deps, new_refs =
                    List.unzip
                      (List.init (List.length l) ~f:(fun i ->
                         let incr = f (map t ~f:(fun l -> List.nth_exn l i)) in
                         let r1 = ref None in
                         let dep1 =
                           E.Dependency.create incr ~on_change:(fun x ->
                             r1 := Some (fst x))
                         in
                         let r2 = ref None in
                         let dep2 =
                           E.Dependency.create incr ~on_change:(fun x ->
                             r2 := Some (snd x))
                         in
                         E.Node.add_dependency parent1 dep1;
                         E.Node.add_dependency parent2 dep2;
                         (dep1, dep2), (r1, r2)))
                  in
                  let compute_result l f () =
                    List.map l ~f:(fun r -> Option.value_exn !(f r))
                  in
                  fs1 := Some (compute_result new_refs fst);
                  fs2 := Some (compute_result new_refs snd);
                  f_result := Some (List.length l, List.unzip deps))
            in
            E.Node.add_dependency parent1 (E.Dependency.create lhs_change);
            E.Node.add_dependency parent2 (E.Dependency.create lhs_change);
            E.Node.watch parent1, E.Node.watch parent2
          in
          Ref.set_temporarily sexp_style To_string_hum ~f:(fun () ->
            let v1 = Var.create [ 2 ] in
            let n1, n2 =
              weird_unzip (Var.watch v1) (fun t -> map t ~f:(fun x -> x, -x))
            in
            (* We add/remove dependencies with only n2 necessary, both necessary
                 and only n1 necessary.  *)
            let o2 = observe n2 in
            stabilize_ ();
            print_s [%sexp (value o2 : int list)];
            [%expect "(-2)"];
            Var.set v1 [ 3 ];
            stabilize_ ();
            print_s [%sexp (value o2 : int list)];
            [%expect "(-3)"];
            Var.set v1 [ 3; 4; 5 ];
            stabilize_ ();
            print_s [%sexp (value o2 : int list)];
            [%expect "(-3 -4 -5)"];
            Var.set v1 [];
            stabilize_ ();
            print_s [%sexp (value o2 : int list)];
            [%expect "()"];
            Var.set v1 [ 3; 4; 5 ];
            stabilize_ ();
            let o1 = observe n1 in
            stabilize_ ();
            print_s [%sexp (value o1 : int list), (value o2 : int list)];
            [%expect "((3 4 5) (-3 -4 -5))"];
            Var.set v1 [ 6 ];
            stabilize_ ();
            print_s [%sexp (value o1 : int list), (value o2 : int list)];
            [%expect "((6) (-6))"];
            disallow_future_use o2;
            Var.set v1 [ 7; 8; 9 ];
            stabilize_ ();
            print_s [%sexp (value o1 : int list)];
            [%expect "(7 8 9)"];
            Var.set v1 [];
            stabilize_ ();
            disallow_future_use o1)
        ;;
      end
