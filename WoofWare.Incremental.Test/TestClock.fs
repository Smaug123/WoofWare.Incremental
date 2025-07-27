namespace WoofWare.Incremental.Test

open System
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel
open WoofWare.Expect

[<TestFixture>]
module TestClock =
    [<Test>]
    let ``test 1`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let w = I.Observe (I.Clock.WatchNow clock)

        fix.Stabilize ()
        let beforeAdvance = Clock.now clock
        Observer.valueThrowing w |> shouldEqual beforeAdvance
        let to_ = TimeNs.add beforeAdvance (TimeNs.Span.ofSec 1.0)
        I.Clock.AdvanceClock clock to_

        Clock.now clock |> shouldEqual to_
        // but we haven't yet stabilized...
        Observer.valueThrowing w |> shouldEqual beforeAdvance

        fix.Stabilize ()
        Observer.valueThrowing w |> shouldEqual to_

    [<Test>]
    let ``advanceClock backwards`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let o = I.Clock.WatchNow clock |> I.Observe

        let showNow () =
            fix.Stabilize ()

            TimeNs.display (Clock.now clock)
            + "\n"
            + TimeNs.display (Observer.valueThrowing o)

        expect {
            snapshot
                @"1970-01-01T00:00:00.0000000Z
1970-01-01T00:00:00.0000000Z"

            return showNow ()
        }

        I.Clock.AdvanceClock clock (TimeNs.add TimeNs.epoch (TimeNs.Span.ofSec 1.0))

        expect {
            snapshot
                @"1970-01-01T00:00:01.0000000Z
1970-01-01T00:00:01.0000000Z"

            return showNow ()
        }

        I.Clock.AdvanceClock clock TimeNs.epoch

        expect {
            snapshot
                @"1970-01-01T00:00:01.0000000Z
1970-01-01T00:00:01.0000000Z"

            return showNow ()
        }

    let is<'a when 'a : equality> (observer : Observer<'a>) (v : 'a) =
        Observer.valueThrowing observer = v |> shouldEqual true

    [<Test>]
    let ``test 2`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let o = I.Observe (I.Clock.After clock (TimeNs.Span.ofSec 1.0))

        let show (expected : BeforeOrAfter) =
            fix.Stabilize ()
            Observer.valueThrowing o |> shouldEqual expected

        show BeforeOrAfter.Before
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        show BeforeOrAfter.After
        I.Clock.AdvanceClockBy clock (I.Clock.AlarmPrecision clock)
        show BeforeOrAfter.After

    [<Test>]
    let ``isInvalidatedOnBindRhs, time in future`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        isInvalidatedOnBindRhs fix (fun _ -> I.Clock.At clock (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)))

    [<Test>]
    let ``isInvalidatedOnBindRhs, time in past`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create (TimeNs.add TimeNs.epoch (TimeNs.Span.ofSec 1.0))
        isInvalidatedOnBindRhs fix (fun _ -> I.Clock.At clock (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec -1.0)))

    [<Test>]
    let ``isInvalidatedOnBindRhs, after, in future`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        isInvalidatedOnBindRhs fix (fun _ -> I.Clock.After clock (TimeNs.Span.ofSec 1.0))

    [<Test>]
    let ``isInvalidatedOnBindRhs, after, in past`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create (TimeNs.add TimeNs.epoch (TimeNs.Span.ofSec 1.0))
        isInvalidatedOnBindRhs fix (fun _ -> I.Clock.After clock (TimeNs.Span.ofSec -1.0))

    [<Test>]
    let ``test 3`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create (TimeNs.add TimeNs.epoch (TimeNs.Span.ofSec 1.0))
        let now = Clock.now clock

        let at span =
            I.Observe (I.Clock.At clock (TimeNs.add now span))

        let i1 = at (TimeNs.Span.ofSec 1.0)
        let i2 = at (TimeNs.Span.ofSec -0.1)
        let i3 = at (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()

        is i1 BeforeOrAfter.After
        is i2 BeforeOrAfter.After
        is i3 BeforeOrAfter.Before

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 0.5)
        fix.Stabilize ()
        is i1 BeforeOrAfter.After
        is i2 BeforeOrAfter.After
        is i3 BeforeOrAfter.Before

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()
        is i1 BeforeOrAfter.After
        is i2 BeforeOrAfter.After
        is i3 BeforeOrAfter.After

    [<Test>]
    let ``advance the clock in the same stabilization cycle as creation`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let i = I.Observe (I.Clock.After clock (TimeNs.Span.ofSec 1.0))
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()

        is i BeforeOrAfter.After

    [<Test>]
    let ``firing an unnecessary after and then observing it`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create (TimeNs.add TimeNs.epoch (TimeNs.Span.ofSec 1.0))
        let i = I.Clock.After clock (TimeNs.Span.ofSec -1.0)

        fix.Stabilize ()
        let o = I.Observe i
        fix.Stabilize ()

        is o BeforeOrAfter.After

        let mutable r = 0

        let i =
            I.Clock.After clock (TimeNs.Span.ofSec 1.0)
            |> I.Map (fun z ->
                Interlocked.Increment &r |> ignore<int>
                z
            )

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()
        r |> shouldEqual 0
        fix.Stabilize ()
        let o = I.Observe i
        fix.Stabilize ()
        r |> shouldEqual 1
        is o BeforeOrAfter.After

    [<Test>]
    let ``at_intervals in the past raises`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        expect {
            snapshotThrows @"System.Exception: at_intervals got too small interval: -1000000000"
            return! fun () -> I.Clock.AtIntervals clock (TimeNs.Span.ofSec -1.0)
        }

    [<Test>]
    let ``at_intervals of 0`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        expect {
            snapshotThrows @"System.Exception: at_intervals got too small interval: 0"
            return! fun () -> I.Clock.AtIntervals clock (TimeNs.Span.ofSec 0.0)
        }

    [<Test>]
    let ``isInvalidatedOnBindRhs, atIntervals`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        isInvalidatedOnBindRhs fix (fun _ -> I.Clock.AtIntervals clock (TimeNs.Span.ofSec 1.0))


    [<Test>]
    let ``advancing the clock does nothing by itself`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let mutable r = 0

        let i =
            I.Clock.AtIntervals clock (TimeNs.Span.ofSec 1.0)
            |> I.Map (fun () -> Interlocked.Increment &r |> ignore<int>)

        let o = I.Observe i

        r |> shouldEqual 0
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        r |> shouldEqual 0

        Observer.disallowFutureUse o

    [<Test>]
    let ``test advanceBy`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let mutable r = -1

        let i =
            I.Clock.AtIntervals clock (TimeNs.Span.ofSec 1.0)
            |> I.Map (fun () -> Interlocked.Increment &r |> ignore<int>)

        let o = I.Observe i

        fix.Stabilize ()

        r |> shouldEqual 0
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 0.5)
        fix.Stabilize ()
        r |> shouldEqual 0

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()
        r |> shouldEqual 1

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        r |> shouldEqual 1

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        r |> shouldEqual 1

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        r |> shouldEqual 1

        fix.Stabilize ()
        r |> shouldEqual 2

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 10.0)
        fix.Stabilize ()
        r |> shouldEqual 3

        Observer.disallowFutureUse o

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()
        r |> shouldEqual 3

        let o = I.Observe i
        fix.Stabilize ()
        r |> shouldEqual 4

        Observer.disallowFutureUse o

    [<Test>]
    let ``advancing exactly to intervals doesn't skip any`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let mutable r = -1

        let o =
            I.Observe (
                I.Clock.AtIntervals clock (TimeNs.Span.ofSec 1.0)
                |> I.Map (fun () -> Interlocked.Increment &r |> ignore<int>)
            )

        fix.Stabilize ()

        r |> shouldEqual 0
        let base_ = Clock.now clock
        let mutable curr = base_

        for i = 1 to 20 do
            curr <- TimeNs.nextMultiple None base_ curr (TimeNs.Span.ofSec 1.0)
            I.Clock.AdvanceClock clock curr
            fix.Stabilize ()
            r |> shouldEqual i

        Observer.disallowFutureUse o

    [<Test>]
    let ``interval less than alarm precision raises`` () =
        let I = Incremental.make ()
        let clock = I.Clock.Create TimeNs.epoch

        expect {
            snapshotThrows @"System.Exception: at_intervals got too small interval: 500000"
            return! fun () -> I.Clock.AtIntervals clock (TimeNs.Span.ofSec 0.0005)
        }

    [<Test>]
    let ``at in the past`` () =
        let I = Incremental.make ()
        let clock = I.Clock.Create TimeNs.epoch

        expect {
            snapshot @"cannot take snapshot in the past"

            return
                I.Clock.Snapshot clock (I.Const 14.0) (TimeNs.sub (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 13.0
                |> Result.getError
        }

    [<Test>]
    let ``at in the future`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let o =
            I.Clock.Snapshot clock (I.Const 14) (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 13
            |> Result.get
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13
        fix.Stabilize ()
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        Observer.valueThrowing o |> shouldEqual 13
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 14

    [<Test>]
    let ``at in the future, unobserved`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let x = I.Var.Create 13

        let i =
            I.Clock.Snapshot clock (I.Var.Watch x) (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 15
            |> Result.get

        fix.Stabilize ()
        I.Var.Set x 17
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()
        I.Var.Set x 19
        let o = I.Observe i
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 17

    [<Test>]
    let ``advanceClock past at prior to stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let o =
            I.Clock.Snapshot clock (I.Const 15) (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 13
            |> Result.get
            |> I.Observe

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 15

    [<Test>]
    let ``unobserved, advanceClock past at prior to stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let x = I.Var.Create 13

        let i =
            I.Clock.Snapshot clock (I.Var.Watch x) (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 15
            |> Result.get

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()
        I.Var.Set x 17
        let o = I.Observe i
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13

    [<Test>]
    let ``test invalidated`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let t =
            I.Clock.Snapshot clock fix.Invalid (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 13
            |> Result.get

        let o = I.Observe t
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        fix.Stabilize ()
        NodeHelpers.isValid t |> shouldEqual false
        Observer.disallowFutureUse o

    [<Test>]
    let ``snapshot nodes increment numNodesBecameNecessary`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let i1 = State.numNodesBecameNecessary I.State
        let c = I.Const ()

        for _ = 1 to 5 do
            I.Clock.Snapshot clock c (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) ()
            |> Result.get
            |> ignore

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.0)
        let i2 = State.numNodesBecameNecessary I.State
        // 5 snapshots that became freeze, plus the const
        i2 |> shouldEqual (i1 + 6)

    let relativeStepFunction (clock : Clock) (init : 'a) (steps : (int * 'a) list) : StepFunction<'a> =
        let now = Clock.now clock

        steps
        |> List.map (fun (after, a) -> TimeNs.add now (TimeNs.Span.ofSec (float<int> after)), a)
        |> StepFunction.create init

    let relativeStepFunctionIncr (I : Incremental) (clock : Clock) (init : 'a) (steps : (int * 'a) list) : Node<'a> =
        steps
        |> List.map (fun (after, a) -> TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec (float<int> after)), a)
        |> I.Clock.StepFunction clock init

    [<Test>]
    let ``isInvalidatedOnBindRhs of empty step-function`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        isInvalidatedOnBindRhs fix (fun i -> I.Clock.StepFunction clock i [])

    [<Test>]
    let ``isInvalidatedOnBindRhs of incr step-function`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        isInvalidatedOnBindRhs fix (fun i -> relativeStepFunctionIncr I clock i [ 1, i + 1 ])

    [<Test>]
    let ``no steps`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let i = I.Clock.StepFunction clock 13 []
        let o = I.Observe i
        fix.Stabilize ()

        Observer.valueThrowing o |> shouldEqual 13

        Observer.disallowFutureUse o

(*
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

*)
