namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestClock =

      module Clock = Clock

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let w = observe (Clock.watch_now clock) in
        stabilize_ [%here];
        let before_advance = Clock.now clock in
        assert (Time_ns.equal before_advance (value w));
        let to_ = Time_ns.add before_advance (sec 1.) in
        Clock.advance_clock clock ~to_;
        assert (Time_ns.equal (Clock.now clock) to_);
        assert (Time_ns.equal (value w) before_advance);
        (* we didn't yet stabilize *)
        stabilize_ [%here];
        assert (Time_ns.equal (value w) to_)
      ;;

      let%expect_test "[advance_clock] backwards" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o = observe (Clock.watch_now clock) in
        let show_now () =
          stabilize_ [%here];
          print_s [%sexp (Clock.now clock : Time_ns.t), (value o : Time_ns.t)]
        in
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:00.000000000-05:00)
           (1969-12-31 19:00:00.000000000-05:00))
          |}];
        Clock.advance_clock clock ~to_:(Time_ns.add Time_ns.epoch (sec 1.));
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:01.000000000-05:00)
           (1969-12-31 19:00:01.000000000-05:00))
          |}];
        Clock.advance_clock clock ~to_:Time_ns.epoch;
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:01.000000000-05:00)
           (1969-12-31 19:00:01.000000000-05:00))
          |}]
      ;;

      let is observer v = Poly.equal (value observer) v

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o = observe (Clock.after clock (sec 1.)) in
        let show () =
          stabilize_ [%here];
          print_s [%sexp (value o : Before_or_after.t)]
        in
        show ();
        [%expect {| Before |}];
        Clock.advance_clock_by clock (sec 1.);
        show ();
        [%expect {| After |}];
        Clock.advance_clock_by clock (Clock.alarm_precision clock);
        show ();
        [%expect {| After |}]
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ ->
          Clock.at clock (Time_ns.add (Clock.now clock) (sec 1.)))
      ;;

      let%test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        is_invalidated_on_bind_rhs (fun _ ->
          Clock.at clock (Time_ns.add (Clock.now clock) (sec (-1.))))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.after clock (sec 1.))
      ;;

      let%test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.after clock (sec (-1.)))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let now = Clock.now clock in
        let at span = observe (Clock.at clock (Time_ns.add now span)) in
        let i1 = at (sec (-1.)) in
        let i2 = at (sec (-0.1)) in
        let i3 = at (sec 1.) in
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 Before);
        Clock.advance_clock_by clock (sec 0.5);
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 Before);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 After)
      ;;

      let%expect_test _ =
        (* advancing the clock in the same stabilization cycle as creation *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = observe (Clock.after clock (sec 1.)) in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (is i After)
      ;;

      let%expect_test _ =
        (* firing an unnecessary [after] and then observing it *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let i = Clock.after clock (sec (-1.)) in
        stabilize_ [%here];
        let o = observe i in
        stabilize_ [%here];
        assert (is o After);
        let r = ref 0 in
        let i =
          Clock.after clock (sec 1.)
          >>| fun z ->
          incr r;
          z
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (!r = 0);
        stabilize_ [%here];
        let o = observe i in
        stabilize_ [%here];
        assert (!r = 1);
        assert (is o After)
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        does_raise (fun () -> Clock.at_intervals clock (sec (-1.)))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        does_raise (fun () -> Clock.at_intervals clock (sec 0.))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.at_intervals clock (sec 1.))
      ;;

      let%expect_test _ =
        (* advancing the clock does nothing by itself *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref 0 in
        let i = Clock.at_intervals clock (sec 1.) >>| fun () -> incr r in
        let o = observe i in
        assert (!r = 0);
        Clock.advance_clock_by clock (sec 2.);
        assert (!r = 0);
        disallow_future_use o
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref (-1) in
        let i = Clock.at_intervals clock (sec 1.) >>| fun () -> incr r in
        let o = observe i in
        stabilize_ [%here];
        let show_r () = print_s [%sexp (!r : int)] in
        show_r ();
        [%expect {| 0 |}];
        Clock.advance_clock_by clock (sec 0.5);
        stabilize_ [%here];
        show_r ();
        [%expect {| 0 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        stabilize_ [%here];
        show_r ();
        [%expect {| 2 |}];
        Clock.advance_clock_by clock (sec 10.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 3 |}];
        disallow_future_use o;
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 3 |}];
        let o = observe i in
        stabilize_ [%here];
        show_r ();
        [%expect {| 4 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* advancing exactly to intervals doesn't skip any *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref (-1) in
        let o = observe (Clock.at_intervals clock (sec 1.) >>| fun () -> incr r) in
        stabilize_ [%here];
        [%test_result: int] !r ~expect:0;
        let base = Clock.now clock in
        let curr = ref base in
        for i = 1 to 20 do
          curr := Time_ns.next_multiple ~base ~after:!curr ~interval:(sec 1.) ();
          Clock.advance_clock clock ~to_:!curr;
          stabilize_ [%here];
          [%test_result: int] !r ~expect:i
        done;
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* [interval < alarm precision] raises *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        assert (does_raise (fun () -> Clock.at_intervals clock (sec 0.0005)))
      ;;

      let%expect_test _ =
        (* [at] in the past *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        assert (
          is_error
            (Clock.snapshot
               clock
               (const 14)
               ~at:(Time_ns.sub (Clock.now clock) (sec 1.))
               ~before:13))
      ;;

      let%expect_test _ =
        (* [at] in the future *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o =
          observe
            (ok_exn
               (Clock.snapshot
                  clock
                  (const 14)
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:13))
        in
        stabilize_ [%here];
        assert (value o = 13);
        stabilize_ [%here];
        Clock.advance_clock_by clock (sec 2.);
        assert (value o = 13);
        stabilize_ [%here];
        assert (value o = 14)
      ;;

      let%expect_test _ =
        (* [at] in the future, unobserved *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create_ [%here] 13 in
        let i =
          ok_exn
            (Clock.snapshot
               clock
               (Var.watch x)
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:15)
        in
        stabilize_ [%here];
        Var.set x 17;
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        Var.set x 19;
        let o = observe i in
        stabilize_ [%here];
        assert (value o = 17)
      ;;

      let%expect_test _ =
        (* [advance_clock] past [at] prior to stabilization. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o =
          observe
            (ok_exn
               (Clock.snapshot
                  clock
                  (const 15)
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:13))
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (value o = 15)
      ;;

      let%expect_test _ =
        (* unobserved, [advance_clock] past [at] prior to stabilization. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create_ [%here] 13 in
        let i =
          ok_exn
            (Clock.snapshot
               clock
               (Var.watch x)
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:15)
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        Var.set x 17;
        let o = observe i in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        (* invalidated *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let t =
          ok_exn
            (Clock.snapshot
               clock
               invalid
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:13)
        in
        let o = observe t in
        stabilize_ [%here];
        assert (value o = 13);
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid t));
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* [snapshot] nodes increment [num_nodes_became_necessary] *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i1 = State.(num_nodes_became_necessary t) in
        let c = const () in
        for _ = 1 to 5 do
          ignore
            (ok_exn
               (Clock.snapshot
                  clock
                  c
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:())
             : _ t)
        done;
        Clock.advance_clock_by clock (sec 2.);
        let i2 = State.(num_nodes_became_necessary t) in
        (* the 5 [snapshot]s that became [freeze] plus the [const] *)
        [%test_result: int] i2 ~expect:(i1 + 6)
      ;;

      let relative_step_function clock ~init steps =
        let now = Clock.now clock in
        Step_function.create_exn
          ~init
          ~steps:
            (List.map steps ~f:(fun (after, a) ->
               Time_ns.add now (sec (Float.of_int after)), a))
      ;;

      let relative_step_function_incr clock ~init steps =
        Clock.step_function
          clock
          ~init
          (List.map steps ~f:(fun (after, a) ->
             Time_ns.add (Clock.now clock) (sec (Float.of_int after)), a))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        require
          (is_invalidated_on_bind_rhs (fun i -> Clock.step_function clock ~init:i []))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        require
          (is_invalidated_on_bind_rhs (fun i ->
             relative_step_function_incr clock ~init:i [ 1, i + 1 ]))
      ;;

      let%expect_test _ =
        (* no steps *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = Clock.step_function clock ~init:13 [] in
        let o = observe i in
        stabilize_ [%here];
        print_s [%sexp (value o : int)];
        [%expect {| 13 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* one step at a time *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14; 2, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* all steps in the past *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 2.)) () in
        let i = relative_step_function_incr clock ~init:13 [ -2, 14; -1, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* some steps in the past *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let i = relative_step_function_incr clock ~init:13 [ -1, 14; 1, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* cross multiple steps in one stabilization cycle *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14; 2, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Clock.advance_clock_by clock (sec 1.5);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* cross step in same stabilization as creation *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* observe after step *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14 ] in
        stabilize_ [%here];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* advancing exactly to steps doesn't skip steps *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let base = Clock.now clock in
        let curr = ref base in
        let steps = ref [] in
        for i = 1 to 20 do
          curr := Time_ns.next_multiple ~base ~after:!curr ~interval:(sec 1.) ();
          steps := (!curr, i) :: !steps
        done;
        let steps = List.rev !steps in
        let o = observe (Clock.step_function clock ~init:0 steps) in
        List.iter steps ~f:(fun (to_, _) ->
          Clock.advance_clock clock ~to_;
          stabilize_ [%here];
          print_s [%sexp (value o : int)]);
        [%expect
          {|
          1
          2
          3
          4
          5
          6
          7
          8
          9
          10
          11
          12
          13
          14
          15
          16
          17
          18
          19
          20
          |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* Advancing to a scheduled time shouldn't break things. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let fut = Time_ns.add (Clock.now clock) (sec 1.0) in
        let o1 = observe (Clock.at clock fut) in
        let o2 = observe (ok_exn (Clock.snapshot clock (const 1) ~at:fut ~before:0)) in
        Clock.advance_clock clock ~to_:fut;
        stabilize_ [%here];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* alarms get cleaned up for invalidated time-based incrementals *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        List.iter
          [ (fun () -> Clock.after clock (sec 1.) >>| fun _ -> ())
          ; (fun () -> Clock.at_intervals clock (sec 1.))
          ; (fun () -> relative_step_function_incr clock ~init:() [ 1, () ])
          ]
          ~f:(fun create_time_based_incremental ->
            let num_alarms = Clock.timing_wheel_length clock in
            let x = Var.create_ [%here] 0 in
            let o =
              observe
                (bind (Var.watch x) ~f:(fun i ->
                   if i >= 0 then create_time_based_incremental () else return ()))
            in
            stabilize_ [%here];
            for i = 1 to 10 do
              Var.set x i;
              stabilize_ [%here];
              if check_invalidity
              then
                [%test_result: int]
                  ~expect:(num_alarms + 1)
                  (Clock.timing_wheel_length clock)
            done;
            Var.set x (-1);
            stabilize_ [%here];
            if check_invalidity
            then [%test_result: int] ~expect:num_alarms (Clock.timing_wheel_length clock);
            disallow_future_use o)
      ;;

