namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestState =

      module State = struct
        open State

        type nonrec t = t [@@deriving sexp_of]

        let invariant = invariant
        let t = t
        let%expect_test _ = invariant t
        let max_height_allowed = max_height_allowed

        (* the default *)
        let%test _ = max_height_allowed t = 128
        let max_height_seen = max_height_seen

        (* because of [let invalid] above *)
        let%test _ = max_height_seen t = 3
        let set_max_height_allowed = set_max_height_allowed

        let%expect_test _ =
          List.iter [ -1; 2 ] ~f:(fun height ->
            assert (does_raise (fun () -> set_max_height_allowed t height)))
        ;;

        let%expect_test _ = set_max_height_allowed t 10
        let%test _ = max_height_allowed t = 10
        let%expect_test _ = set_max_height_allowed t 128

        let%expect_test _ =
          set_max_height_allowed t 256;
          let rec loop n = if n = 0 then return 0 else loop (n - 1) >>| fun i -> i + 1 in
          let o = observe (loop (max_height_allowed t)) in
          stabilize_ [%here];
          assert (Observer.value_exn o = max_height_allowed t)
        ;;

        let%test _ = max_height_allowed t = max_height_seen t
        let%expect_test _ = invariant t
        let num_active_observers = num_active_observers

        let%expect_test _ =
          Gc.full_major ();
          stabilize_ [%here];
          let n = num_active_observers t in
          let o = observe (const 0) in
          disallow_future_use o;
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let%expect_test _ =
          Gc.full_major ();
          stabilize_ [%here];
          let n = num_active_observers t in
          let o = observe (const 0) in
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:(n + 1);
          disallow_future_use o;
          [%test_result: int] (num_active_observers t) ~expect:n;
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let%expect_test _ =
          (* [observe ~should_finalize:true] *)
          Gc.full_major ();
          stabilize_ [%here];
          let _o = observe (const 13) ~should_finalize:true in
          stabilize_ [%here];
          let n = num_active_observers t in
          Gc.full_major ();
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:(n - 1)
        ;;

        let%expect_test _ =
          (* [observe ~should_finalize:false] *)
          Gc.full_major ();
          stabilize_ [%here];
          let _o = observe (const 13) ~should_finalize:false in
          stabilize_ [%here];
          let n = num_active_observers t in
          Gc.full_major ();
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let keep_node_creation_backtrace = keep_node_creation_backtrace
        let set_keep_node_creation_backtrace = set_keep_node_creation_backtrace
        let num_nodes_became_necessary = num_nodes_became_necessary
        let num_nodes_became_unnecessary = num_nodes_became_unnecessary
        let num_nodes_changed = num_nodes_changed
        let num_nodes_created = num_nodes_created
        let num_nodes_invalidated = num_nodes_invalidated
        let num_nodes_recomputed = num_nodes_recomputed
        let num_stabilizes = num_stabilizes
        let num_var_sets = num_var_sets

        let num_nodes_recomputed_directly_because_min_height =
          num_nodes_recomputed_directly_because_min_height
        ;;

        let%expect_test _ =
          let var = Var.create 1 in
          let o =
            observe
              (map2
                 (map2 (Var.watch var) (const 1) ~f:( + ))
                 (map2 (const 2) (const 3) ~f:( + ))
                 ~f:( + ))
          in
          stabilize_ [%here];
          let stat1 = num_nodes_recomputed_directly_because_min_height t in
          Var.set var 2;
          stabilize_ [%here];
          let stat2 = num_nodes_recomputed_directly_because_min_height t in
          [%test_eq: int] (stat2 - stat1) 2;
          disallow_future_use o
        ;;

        let num_nodes_recomputed_directly_because_one_child =
          num_nodes_recomputed_directly_because_one_child
        ;;

        let%expect_test _ =
          (* We can't use the same variable twice otherwise the optimization is not
               applied. *)
          let var1 = Var.create 1 in
          let var2 = Var.create 1 in
          let o var = observe (map (map (Var.watch var) ~f:Fn.id) ~f:Fn.id) in
          let o1 = o var1 in
          let o2 = o var2 in
          stabilize_ [%here];
          let stat1 = num_nodes_recomputed_directly_because_one_child t in
          Var.set var1 2;
          Var.set var2 2;
          stabilize_ [%here];
          let stat2 = num_nodes_recomputed_directly_because_one_child t in
          [%test_result: int] (stat2 - stat1) ~expect:4;
          disallow_future_use o1;
          disallow_future_use o2
        ;;

        module Stats = Stats

        let stats = stats
      end

