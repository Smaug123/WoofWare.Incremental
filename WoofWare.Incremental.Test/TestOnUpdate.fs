namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

(*
[<TestFixture>]
module TestOnUpdate =

      let on_update = on_update

      let%expect_test _ =
        let v = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        let o = observe (watch v) in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        stabilize_ [%here];
        check [];
        Var.set v 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        disallow_future_use o;
        Var.set v 15;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* on-change handlers of a node that changes but is not necessary at the end
             of a stabilization *)
        let v = Var.create_ [%here] 0 in
        let n = Var.watch v in
        let push, check = on_update_queue () in
        on_update n ~f:push;
        let o = observe n in
        stabilize_ [%here];
        check [ Necessary 0 ];
        disallow_future_use o;
        Var.set v 1;
        let o = observe (freeze n) in
        stabilize_ [%here];
        check [ Unnecessary ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* value changing with different observers *)
        let v = Var.create_ [%here] 13 in
        let o = observe (watch v) in
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        let o = observe (watch v) in
        stabilize_ [%here];
        disallow_future_use o;
        check [ Necessary 14 ]
      ;;

      let%expect_test _ =
        (* call at next stabilization *)
        let v = Var.create_ [%here] 13 in
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        let r = ref 0 in
        on_update (Var.watch v) ~f:(fun _ -> incr r);
        stabilize_ [%here];
        assert (!r = 1);
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* called at next stabilization with [Unnecessary] update *)
        let v = Var.create_ [%here] 13 in
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* transition from unnecessary to necessary and back *)
        let x = Var.create 13 in
        let push, check = on_update_queue () in
        on_update (watch x) ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe (watch x) in
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set x 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* an indirectly necessary node *)
        let x = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        on_update (Var.watch x) ~f:push;
        let t = Var.watch x >>| fun i -> i + 1 in
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe t in
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe t in
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* [on_update] doesn't make a node necessary *)
        let v = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        stabilize_ [%here];
        check [];
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        check [ Necessary 14 ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* invalid from the start *)
        let push, check = on_update_queue () in
        on_update invalid ~f:push;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ]
      ;;

      let%expect_test _ =
        (* invalidation of an unnecessary node *)
        let v = Var.create_ [%here] 13 in
        let r = ref None in
        let o =
          observe
            (bind (watch v) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* invalidation of a necessary node *)
        let v = Var.create_ [%here] 13 in
        let r = ref None in
        let o1 =
          observe
            (bind (watch v) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let o2 = observe i in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set v 14;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* invalidation of a necessary node after a change *)
        let v = Var.create_ [%here] 13 in
        let w = Var.create_ [%here] 14 in
        let r = ref None in
        let o1 =
          observe
            (bind (watch v) ~f:(fun _ ->
               r := Some (watch w >>| Fn.id);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let o2 = observe i in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Necessary 14 ];
        Var.set w 15;
        stabilize_ [%here];
        check [ Changed (14, 15) ];
        Var.set v 16;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* making a node necessary from an on-update handler *)
        let x = Var.create_ [%here] 13 in
        let y = Var.create_ [%here] 14 in
        let r = ref None in
        let push_x, check_x = on_update_queue () in
        on_update (watch x) ~f:push_x;
        let o = observe (watch y) in
        let push_o, check_o = on_observer_update_queue () in
        Observer.on_update_exn o ~f:(fun u ->
          push_o u;
          r := Some (observe (watch x)));
        stabilize_ [%here];
        check_x [ Unnecessary ];
        check_o [ Initialized 14 ];
        let ox = Option.value_exn !r in
        Var.set x 15;
        stabilize_ [%here];
        check_x [ Necessary 15 ];
        check_o [];
        disallow_future_use o;
        disallow_future_use ox
      ;;

      let%expect_test _ =
        (* calling [advance_clock] in an on-update handler *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = Clock.after clock (sec 1.) in
        let o = observe i in
        let num_fires = ref 0 in
        on_update i ~f:(fun _ ->
          incr num_fires;
          Clock.advance_clock_by clock (sec 2.));
        assert (!num_fires = 0);
        stabilize_ [%here];
        assert (!num_fires = 1);
        stabilize_ [%here];
        assert (!num_fires = 2);
        disallow_future_use o
      ;;

*)
