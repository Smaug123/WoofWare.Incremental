namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestObserver =

      module Observer = struct
        open Observer

        type nonrec 'a t = 'a t [@@deriving sexp_of]

        let%expect_test "[sexp_of_t]" =
          let t = observe (watch (Var.create 13)) in
          let show_t () = print_s [%sexp (t : int t)] in
          show_t ();
          [%expect {| <unstabilized> |}];
          stabilize_ [%here];
          show_t ();
          [%expect {| 13 |}];
          disallow_future_use t;
          show_t ();
          [%expect {| <disallowed> |}]
        ;;

        let invariant = invariant
        let observing = observing

        let%expect_test _ =
          let x = Var.create_ [%here] 0 in
          let o = observe (watch x) in
          assert (phys_same (observing o) (watch x))
        ;;

        let use_is_allowed = use_is_allowed

        let%expect_test _ =
          let o = observe (watch (Var.create_ [%here] 0)) in
          assert (use_is_allowed o);
          disallow_future_use o;
          assert (not (use_is_allowed o))
        ;;

        let disallow_future_use = disallow_future_use
        let value = value
        let value_exn = value_exn

        let%expect_test _ =
          (* calling [value] before stabilizing returns error. *)
          let x = Var.create_ [%here] 0 in
          let o = observe (watch x) in
          assert (is_error (value o));
          assert (does_raise (fun () -> value_exn o))
        ;;

        let%expect_test _ =
          (* calling [value] on a just-created observer of an already computed
               incremental before stabilizing returns error. *)
          let x = Var.create_ [%here] 13 in
          let o = observe (watch x) in
          stabilize_ [%here];
          disallow_future_use o;
          Var.set x 14;
          stabilize_ [%here];
          Var.set x 15;
          let o = observe (watch x) in
          assert (is_error (value o));
          assert (does_raise (fun () -> value_exn o))
        ;;

        let%expect_test _ =
          (* calling [value] after [disallow_future_use] returns error. *)
          let x = Var.create_ [%here] 0 in
          let o = observe (watch x) in
          stabilize_ [%here];
          disallow_future_use o;
          assert (is_error (value o));
          assert (does_raise (fun () -> value_exn o))
        ;;

        let%expect_test _ =
          (* [disallow_future_use] disables on-update handlers. *)
          let x = Var.create_ [%here] 13 in
          let o = observe (Var.watch x) in
          let r = ref 0 in
          Observer.on_update_exn o ~f:(fun _ -> incr r);
          stabilize_ [%here];
          assert (!r = 1);
          disallow_future_use o;
          Var.set x 14;
          stabilize_ [%here];
          assert (!r = 1)
        ;;

        let%expect_test _ =
          (* finalizers work *)
          Gc.full_major ();
          stabilize_ [%here];
          (* clean up pre-existing finalizers *)
          let before = State.(num_active_observers t) in
          let x = Var.create_ [%here] 13 in
          let o = observe (Var.watch x) in
          assert (State.(num_active_observers t) = before + 1);
          stabilize_ [%here];
          assert (value_exn o = 13);
          Gc.full_major ();
          assert (State.(num_active_observers t) = before + 1);
          stabilize_ [%here];
          assert (State.(num_active_observers t) = before)
        ;;

        let%expect_test _ =
          (* finalizers don't disable on-update handlers *)
          let x = Var.create_ [%here] 13 in
          let o = observe (Var.watch x) in
          let r = ref 0 in
          Observer.on_update_exn o ~f:(fun _ -> incr r);
          stabilize_ [%here];
          assert (!r = 1);
          Gc.full_major ();
          Var.set x 14;
          stabilize_ [%here];
          assert (!r = 2)
        ;;

        let%expect_test _ =
          (* finalizers cause an [Unnecessary] update to be sent *)
          let x = Var.create 13 in
          let o = observe (watch x) in
          let push, check = on_update_queue () in
          on_update (watch x) ~f:push;
          stabilize_ [%here];
          check [ Necessary 13 ];
          Gc.keep_alive o;
          Gc.full_major ();
          stabilize_ [%here];
          check [ Unnecessary ]
        ;;

        let%expect_test _ =
          (* [disallow_future_use] and finalize in the same stabilization. *)
          let x = Var.create_ [%here] 1 in
          let o = observe (Var.watch x) in
          stabilize_ [%here];
          disallow_future_use o;
          Gc.full_major ();
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* finalize after disallow_future_use *)
          let x = Var.create_ [%here] 1 in
          let o = observe (Var.watch x) in
          stabilize_ [%here];
          disallow_future_use o;
          stabilize_ [%here];
          (* This [full_major] + [stabilize] causes the finalizer for [o] to run and
               makes sure that it doesn't do anything wrong, given that
               [disallow_future_use o] has already been called. *)
          Gc.full_major ();
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* after user resurrection of an observer, it is still disallowed *)
          let x = Var.create_ [%here] 13 in
          let o = observe (Var.watch x) in
          stabilize_ [%here];
          Gc.keep_alive o;
          let r = ref None in
          Gc.Expert.add_finalizer_ignore o (fun o -> r := Some o);
          Gc.full_major ();
          stabilize_ [%here];
          let o = Option.value_exn !r in
          assert (not (use_is_allowed o))
        ;;

        let%expect_test _ =
          (* lots of observers on the same node isn't quadratic. *)
          (* We can't run this test with debugging, because it's too slow. *)
          if not debug
          then (
            let t = const 13 in
            let observers = List.init 100_000 ~f:(fun _ -> observe t) in
            let cpu_used () =
              let module R = Unix.Resource_usage in
              let { R.utime; stime; _ } = R.get `Self in
              Time_ns.Span.of_sec (utime +. stime)
            in
            let before = cpu_used () in
            (* Don't use [stabilize_], which runs the invariant, which is too slow
                 here. *)
            stabilize ();
            List.iter observers ~f:Observer.disallow_future_use;
            stabilize ();
            let consumed = Time_ns.Span.( - ) (cpu_used ()) before in
            assert (Time_ns.Span.( < ) consumed (sec 1.)))
        ;;

        module Update = Update

        let on_update_exn = on_update_exn

        let%expect_test _ =
          let x = Var.create_ [%here] 13 in
          let parent = map (watch x) ~f:(fun x -> x + 1) in
          let parent_o = observe parent in
          let num_calls = ref 0 in
          let r = ref 0 in
          on_update_exn parent_o ~f:(function
            | Initialized i | Changed (_, i) ->
              num_calls := !num_calls + 1;
              r := i
            | Invalidated -> assert false);
          stabilize_ [%here];
          assert (!num_calls = 1);
          Var.set x 15;
          stabilize_ [%here];
          assert (!num_calls = 2);
          assert (!r = 16);
          disallow_future_use parent_o;
          Var.set x 17;
          stabilize_ [%here];
          assert (!num_calls = 2);
          assert (!r = 16)
        ;;

        let%expect_test _ =
          (* [on_update_exn] of an invalid node, not during a stabilization *)
          let o = observe invalid in
          on_update_exn o ~f:(fun _ -> assert false);
          disallow_future_use o
        ;;

        let%expect_test _ =
          (* [on_update_exn] of an invalid node *)
          let o = observe invalid in
          let is_ok = ref false in
          on_update_exn o ~f:(function
            | Invalidated -> is_ok := true
            | _ -> assert (skip_invalidity_check || false));
          stabilize_ [%here];
          assert (skip_invalidity_check || !is_ok)
        ;;

        let%expect_test _ =
          (* stabilizing with an on-update handler of a node that is invalidated *)
          let x = Var.create 0 in
          let r = ref None in
          let o1 =
            observe
              (bind (watch x) ~f:(fun i ->
                 let t = const i in
                 r := Some t;
                 t))
          in
          stabilize_ [%here];
          let o2 = observe (Option.value_exn !r) in
          let invalidated = ref false in
          Observer.on_update_exn o2 ~f:(function
            | Invalidated -> invalidated := true
            | _ -> ());
          stabilize_ [%here];
          assert (not !invalidated);
          Var.set x 1;
          stabilize_ [%here];
          assert (skip_invalidity_check || !invalidated);
          disallow_future_use o1;
          disallow_future_use o2
        ;;

        let%expect_test _ =
          (* [on_update_exn] of a disallowed observer *)
          let o = observe (const 5) in
          disallow_future_use o;
          assert (does_raise (fun () -> on_update_exn o ~f:(fun _ -> assert false)))
        ;;

        let%expect_test _ =
          (* [disallow_future_use] before first stabilization *)
          let o = observe (const 5) in
          disallow_future_use o;
          stabilize_ [%here];
          disallow_future_use o
        ;;

        let%expect_test _ =
          (* [disallow_future_use] during an on-update handler *)
          let x = Var.create_ [%here] 13 in
          let o = observe (watch x) in
          on_update_exn o ~f:(fun _ -> disallow_future_use o);
          stabilize_ [%here];
          assert (is_error (value o))
        ;;

        let%expect_test _ =
          (* disallowing other on-update handlers in an on-update handler *)
          let x = Var.create_ [%here] 13 in
          let o = observe (watch x) in
          for _ = 1 to 2 do
            on_update_exn o ~f:(fun _ ->
              assert (use_is_allowed o);
              disallow_future_use o)
          done;
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* disallowing other observers of the same node an on-update handler *)
          let x = Var.create_ [%here] 13 in
          let o1 = observe (watch x) in
          let o2 = observe (watch x) in
          let o3 = observe (watch x) in
          List.iter [ o1; o3 ] ~f:(fun o ->
            on_update_exn o ~f:(fun _ -> assert (use_is_allowed o)));
          on_update_exn o2 ~f:(fun _ ->
            disallow_future_use o1;
            disallow_future_use o3);
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* disallowing observers of other nodes in an on-update handler *)
          let o () = observe (watch (Var.create_ [%here] 13)) in
          let o1 = o () in
          let o2 = o () in
          let o3 = o () in
          List.iter [ o1; o3 ] ~f:(fun o ->
            on_update_exn o ~f:(fun _ -> assert (use_is_allowed o)));
          on_update_exn o2 ~f:(fun _ ->
            disallow_future_use o1;
            disallow_future_use o3);
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* adding an on-update-handler to an already stable node *)
          let x = watch (Var.create 13) in
          let o = observe x in
          stabilize_ [%here];
          let did_run = ref false in
          on_update_exn (observe x) ~f:(fun _ -> did_run := true);
          assert (not !did_run);
          stabilize_ [%here];
          assert !did_run;
          disallow_future_use o
        ;;

        let%expect_test _ =
          (* adding an on-update handler after a change *)
          let x = Var.create 13 in
          let o = observe (watch x) in
          let push1, check1 = on_observer_update_queue () in
          Observer.on_update_exn o ~f:push1;
          stabilize_ [%here];
          check1 [ Initialized 13 ];
          Var.set x 14;
          stabilize_ [%here];
          check1 [ Changed (13, 14) ];
          let push2, check2 = on_observer_update_queue () in
          Observer.on_update_exn o ~f:push2;
          stabilize_ [%here];
          check2 [ Initialized 14 ];
          Var.set x 15;
          stabilize_ [%here];
          check1 [ Changed (14, 15) ];
          check2 [ Changed (14, 15) ]
        ;;

        let%expect_test _ =
          (* adding an on-update handler in an on-update handler. *)
          let x = Var.create 13 in
          let o = observe (watch x) in
          let did_run = ref false in
          on_update_exn o ~f:(fun _ ->
            on_update_exn (observe (watch x)) ~f:(fun _ -> did_run := true));
          stabilize_ [%here];
          assert (not !did_run);
          Var.set x 14;
          stabilize_ [%here];
          assert !did_run;
          Gc.full_major ();
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* adding an on-update-handler to an invalid node in an on-update
               handler. *)
          let module I = Make () in
          let open I in
          let o = observe (watch (Var.create 13)) in
          let is_ok = ref false in
          Observer.on_update_exn o ~f:(fun _ ->
            Observer.on_update_exn (observe invalid) ~f:(function
              | Invalidated -> is_ok := true
              | _ -> assert (skip_invalidity_check || false)));
          stabilize_ [%here];
          assert (not !is_ok);
          stabilize_ [%here];
          assert (skip_invalidity_check || !is_ok)
        ;;

        let%expect_test _ =
          (* on-update-handlers added during the firing of other on-update-handlers
               should not fire now but instead after the next stabilization *)
          List.iter
            [ const 1; invalid ]
            ~f:(fun node ->
              let o1 = observe (const 1) in
              let o2 = observe node in
              let ran = ref 0 in
              Observer.on_update_exn o1 ~f:(fun _ ->
                Observer.on_update_exn o2 ~f:(fun _ -> incr ran));
              Observer.on_update_exn o2 ~f:(fun _ -> ());
              assert (!ran = 0);
              stabilize_ [%here];
              assert (!ran = 0);
              stabilize_ [%here];
              assert (!ran = 1);
              stabilize_ [%here];
              assert (!ran = 1);
              disallow_future_use o1;
              disallow_future_use o2)
        ;;

        let%expect_test _ =
          (* on-update handler set up during stabilization fires after the
               stabilization *)
          let called = ref false in
          let unit = const () in
          let o_unit = observe unit in
          let o =
            observe
              (map unit ~f:(fun () -> on_update_exn o_unit ~f:(fun _ -> called := true)))
          in
          assert (not !called);
          stabilize_ [%here];
          assert !called;
          disallow_future_use o;
          disallow_future_use o_unit
        ;;

        let%expect_test _ =
          (* on-update handlers are initialized once *)
          let v = Var.create (const 0) in
          let i = Var.watch v in
          let o = observe i in
          let old_val_is_none_once () =
            let is_first_call = ref true in
            function
            | Observer.Update.Initialized _ ->
              assert !is_first_call;
              is_first_call := false
            | Changed _ -> assert (not !is_first_call)
            | Invalidated -> assert false
          in
          Observer.on_update_exn o ~f:(old_val_is_none_once ());
          stabilize ();
          Observer.on_update_exn o ~f:(old_val_is_none_once ());
          stabilize ()
        ;;

        let%expect_test _ =
          (* creating an observer during stabilization *)
          let x = Var.create 13 in
          let r = ref None in
          let o1 =
            observe
              (Var.watch x
               >>| fun _ ->
               let o2 = observe (Var.watch x) in
               assert (use_is_allowed o2);
               assert (is_error (value o2));
               r := Some o2;
               0)
          in
          stabilize_ [%here];
          let o2 = Option.value_exn !r in
          assert (use_is_allowed o2);
          assert (is_error (value o2));
          stabilize_ [%here];
          assert (value_exn o2 = 13);
          disallow_future_use o1;
          disallow_future_use o2;
          stabilize_ [%here];
          assert (is_error (value o2))
        ;;

        let%expect_test _ =
          (* creating an observer and adding on_update handler during
               stabilization *)
          let v = Var.create 0 in
          let push, check = on_observer_update_queue () in
          let inner_obs = ref None in
          let o =
            observe
              (Var.watch v
               >>| fun i ->
               let observer = observe (Var.watch v) in
               inner_obs := Some observer;
               on_update_exn observer ~f:push;
               i)
          in
          check [];
          stabilize_ [%here];
          check [];
          stabilize_ [%here];
          check [ Initialized 0 ];
          disallow_future_use o;
          disallow_future_use (Option.value_exn !inner_obs)
        ;;

        let%expect_test _ =
          (* disallow_future_use during stabilization *)
          let x = Var.create 13 in
          let handler_ran = ref false in
          let o1 = observe (Var.watch x) in
          let o2 =
            observe
              (Var.watch x
               >>| fun i ->
               on_update_exn o1 ~f:(fun _ -> handler_ran := true);
               disallow_future_use o1;
               assert (not (use_is_allowed o1));
               i)
          in
          assert (use_is_allowed o1);
          assert (not !handler_ran);
          stabilize_ [%here];
          assert (not (use_is_allowed o1));
          assert (not !handler_ran);
          disallow_future_use o2
        ;;

        let%expect_test _ =
          (* creating an observer and disallowing use during stabilization *)
          let x = Var.create 13 in
          let r = ref None in
          let o1 =
            observe
              (Var.watch x
               >>| fun _ ->
               let o2 = observe (Var.watch x) in
               r := Some o2;
               disallow_future_use o2;
               0)
          in
          stabilize_ [%here];
          let o2 = Option.value_exn !r in
          assert (not (use_is_allowed o2));
          disallow_future_use o1;
          stabilize_ [%here]
        ;;

        let%expect_test _ =
          (* creating an observer and finalizing it during stabilization *)
          let x = Var.create 13 in
          let o =
            observe
              (Var.watch x
               >>| fun _ ->
               Fn.ignore (observe (Var.watch x) : _ Observer.t);
               Gc.full_major ();
               0)
          in
          stabilize_ [%here];
          stabilize_ [%here];
          disallow_future_use o
        ;;
      end

