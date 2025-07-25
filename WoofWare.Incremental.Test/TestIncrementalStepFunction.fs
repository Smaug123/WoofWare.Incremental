namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

(*
[<TestFixture>]
module TestIncrementalStepFunction =

      let%expect_test "[incremental_step_function]" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (Step_function.constant 1) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 1 |}];
        Var.set x (Step_function.constant 2);
        stabilize_ [%here];
        show ();
        [%expect {| 2 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function with step in the past, present, and \
                       future"
        =
        let test ~step_at =
          let clock = Clock.create ~start:Time_ns.epoch () in
          let x = Var.create (Step_function.constant 1) in
          let o = observe (Clock.incremental_step_function clock (watch x)) in
          let show () = print_s [%sexp (value o : int)] in
          stabilize_ [%here];
          show ();
          [%expect {| 1 |}];
          Var.set x (relative_step_function clock ~init:2 [ step_at, 3 ]);
          stabilize_ [%here];
          show ();
          disallow_future_use o
        in
        test ~step_at:(-1);
        [%expect {| 3 |}];
        test ~step_at:0;
        [%expect {| 3 |}];
        test ~step_at:1;
        [%expect {| 2 |}]
      ;;

      let%expect_test "incremental step function; advance time and change function" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (relative_step_function clock ~init:13 [ 1, 14 ]) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Var.set x (relative_step_function clock ~init:15 [ 1, 16 ]);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 16 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function; change to const and then advance time" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (relative_step_function clock ~init:13 [ 1, 14 ]) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Var.set x (Step_function.constant 15);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function is invalidated when child is" =
        let b = Var.create true in
        let clock = Clock.create ~start:Time_ns.epoch () in
        let t =
          Clock.incremental_step_function
            clock
            (if_ (watch b) ~then_:(const 0) ~else_:invalid >>| Step_function.constant)
        in
        let o = observe t in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 0 |}];
        Var.set b false;
        stabilize_ [%here];
        print_s [%sexp (check_invalidity && is_valid t : bool)];
        [%expect {| false |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function that becomes observable in a \
                       stabilization after its child stabilizes"
        =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (Step_function.constant 13) in
        let ox = observe (watch x) in
        let t = Clock.incremental_step_function clock (watch x) in
        stabilize_ [%here];
        let o = observe t in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        disallow_future_use ox;
        disallow_future_use o
      ;;

      let%expect_test "incremental step function of const used in other places" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let c = const (Step_function.constant 13) in
        let c1 = c >>| fun s -> s in
        let o1 = observe c1 in
        let x = Clock.incremental_step_function clock c in
        let ox = observe x in
        stabilize_ [%here];
        disallow_future_use o1;
        disallow_future_use ox
      ;;

      let%expect_test _ =
        (* Equivalence between [step_function] and reimplementation with [at] *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let my_step_function ~init steps =
          let xs =
            Array.map (Array.of_list steps) ~f:(fun (time, x) ->
              map (Clock.at clock time) ~f:(function
                | Before -> None
                | After -> Some x))
          in
          array_fold xs ~init ~f:(fun acc x -> Option.value x ~default:acc)
        in
        let base = Clock.now clock in
        let steps =
          List.map
            ~f:(fun (d, v) -> Time_ns.add base (sec d), v)
            [ 1.0, 1
            ; 1.99999, 2
              (* It is unspecified whether this alarm has fired when the
                 time is 2. but this test relies on the two
                 step_functions having the same unspecified behaviour. *)
            ; 2.0, 3
            ; 3.00001, 4
            ; 4.0, 5
            ; 4.00001, 6
            ; 5.0, 6
            ; 6.0, 7
            ]
        in
        let o1 = observe (Clock.step_function clock ~init:0 steps) in
        let o2 = observe (my_step_function ~init:0 steps) in
        stabilize_ [%here];
        for i = 1 to 7 do
          Clock.advance_clock clock ~to_:(Time_ns.add base (sec (Float.of_int i)));
          stabilize_ [%here];
          print_s [%sexp (value o1 : int), (value o2 : int)];
          require (value o1 = value o2)
        done;
        [%expect
          {|
          (1 1)
          (3 3)
          (3 3)
          (5 5)
          (6 6)
          (7 7)
          (7 7)
          |}];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test "[Step_function.create_from_sequence]" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x =
          Var.create
            (Step_function.create_from_sequence
               ~init:13
               ~steps:
                 (Sequence.unfold
                    ~init:(Clock.now clock, 14)
                    ~f:(fun (at, i) ->
                      print_s [%message "unfold" (i : int)];
                      Some ((at, i), (Time_ns.add at (sec 1.), i + 1)))))
        in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        for _ = 1 to 5 do
          Clock.advance_clock_by clock (sec 1.);
          stabilize_ [%here];
          show ()
        done;
        [%expect
          {|
          (unfold (i 14))
          (unfold (i 15))
          (unfold (i 16))
          (unfold (i 16))
          15
          (unfold (i 16))
          (unfold (i 17))
          (unfold (i 17))
          16
          (unfold (i 17))
          (unfold (i 18))
          (unfold (i 18))
          17
          (unfold (i 18))
          (unfold (i 19))
          (unfold (i 19))
          18
          (unfold (i 19))
          (unfold (i 20))
          (unfold (i 20))
          19
          |}];
        disallow_future_use o
      ;;

*)
