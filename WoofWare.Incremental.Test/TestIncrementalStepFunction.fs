namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel
open WoofWare.Expect

[<TestFixture>]
module TestIncrementalStepFunction =

    [<Test>]
    let ``incremental step function`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let x = I.Var.Create (StepFunction.constant 1)
        let o = I.Observe (I.Clock.IncrementalStepFunction clock (I.Var.Watch x))

        fix.Stabilize ()
        Observer.value o |> shouldEqual 1

        I.Var.Set x (StepFunction.constant 2)
        fix.Stabilize ()
        Observer.value o |> shouldEqual 2

        Observer.disallowFutureUse o

    let relativeStepFunction (clock : Clock) (init : 'a) (steps : (int * 'a) list) : StepFunction<'a> =
        let now = Clock.now clock

        steps
        |> List.map (fun (after, a) -> TimeNs.add now (TimeNs.Span.ofSec (float<int> after)), a)
        |> StepFunction.create init

    [<Test>]
    let ``incremental step function with step in past, present, and future`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let test stepAt expected =
            let clock = I.Clock.Create TimeNs.epoch
            let x = I.Var.Create (StepFunction.constant 1)
            let o = I.Observe (I.Clock.IncrementalStepFunction clock (I.Var.Watch x))

            fix.Stabilize ()
            Observer.value o |> shouldEqual 1

            I.Var.Set x (relativeStepFunction clock 2 [ stepAt, 3 ])
            fix.Stabilize ()

            Observer.value o |> shouldEqual expected
            Observer.disallowFutureUse o

        test -1 3
        test 0 3
        test 1 2

    [<Test>]
    let ``incremental step function, advance time and change function`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let x = I.Var.Create (relativeStepFunction clock 13 [ 1, 14 ])
        let o = I.Observe (I.Clock.IncrementalStepFunction clock (I.Var.Watch x))

        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

        I.Var.Set x (relativeStepFunction clock 15 [ 1, 16 ])
        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()
        Observer.value o |> shouldEqual 16

        Observer.disallowFutureUse o

    [<Test>]
    let ``incremental step function, change to const and then advance time`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let clock = I.Clock.Create TimeNs.epoch
        let x = I.Var.Create (relativeStepFunction clock 13 [ 1, 14 ])
        let o = I.Observe (I.Clock.IncrementalStepFunction clock (I.Var.Watch x))

        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

        I.Var.Set x (StepFunction.constant 15)
        fix.Stabilize ()
        Observer.value o |> shouldEqual 15

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        fix.Stabilize ()
        Observer.value o |> shouldEqual 15

        Observer.disallowFutureUse o

    [<Test>]
    let ``incremental step function is invalidated when child is`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let b = I.Var.Create true
        let clock = I.Clock.Create TimeNs.epoch

        let t =
            I.If (I.Var.Watch b) (I.Const 0) fix.Invalid
            |> I.Map StepFunction.constant
            |> I.Clock.IncrementalStepFunction clock

        let o = I.Observe t
        fix.Stabilize ()
        Observer.value o |> shouldEqual 0

        I.Var.Set b false
        fix.Stabilize ()
        NodeHelpers.isValid t |> shouldEqual false
        Observer.disallowFutureUse o

    [<Test>]
    let ``incremental step function that becomes observable in a stabilization after its child stabilizes`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let x = I.Var.Create (StepFunction.constant 13)
        let ox = I.Observe (I.Var.Watch x)
        let t = I.Clock.IncrementalStepFunction clock (I.Var.Watch x)

        fix.Stabilize ()
        let o = I.Observe t
        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

        Observer.disallowFutureUse ox
        Observer.disallowFutureUse o

    [<Test>]
    let ``incremental step function of const used in other places`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let c = I.Const (StepFunction.constant 13)
        let c1 = c |> I.Map id
        let o1 = I.Observe c1
        let x = I.Clock.IncrementalStepFunction clock c
        let ox = I.Observe x
        fix.Stabilize ()
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse ox

    [<Test>]
    let ``equivalence between stepfunction and reimplementation with at`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let myStepFunction init steps =
            steps
            |> Array.ofList
            |> Array.map (fun (time, x) ->
                I.Clock.At clock time
                |> I.Map (fun m ->
                    match m with
                    | BeforeOrAfter.Before -> None
                    | BeforeOrAfter.After -> Some x
                )
            )
            |> I.ArrayFold init (fun acc x -> Option.defaultValue acc x)

        let base_ = Clock.now clock

        let steps =
            [
                1.0, 1
                1.99999, 2
                // It is unspecified whether this alarm has fired when the time is 2, but this
                // test relies on the two step_functions having the same unspecified behaviour.
                2.0, 3
                3.00001, 4
                4.0, 5
                4.00001, 6
                5.0, 6
                6.0, 7
            ]
            |> List.map (fun (d, v) -> TimeNs.add base_ (TimeNs.Span.ofSec d), v)

        let o1 = I.Observe (I.Clock.StepFunction clock 0 steps)
        let o2 = I.Observe (myStepFunction 0 steps)

        fix.Stabilize ()

        expect {
            snapshotJson
                @"[
  1,
  3,
  3,
  5,
  6,
  7,
  7
]"

            return
                [ 1..7 ]
                |> List.map (fun i ->
                    I.Clock.AdvanceClock clock (TimeNs.add base_ (TimeNs.Span.ofSec (float<int> i)))
                    fix.Stabilize ()
                    let actual = Observer.value o1
                    actual |> shouldEqual (Observer.value o2)
                    actual
                )
        }

    [<Test>]
    let ``StepFunction createFromSequence`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch

        let values = ResizeArray ()

        let x =
            (Clock.now clock, 14)
            |> Sequence.unfold (fun (at, i) ->
                values.Add $"unfold: %i{i}"
                Some ((at, i), (TimeNs.add at (TimeNs.Span.ofSec 1.0), i + 1))
            )
            |> StepFunction.createFromSequence 13
            |> I.Var.Create

        let o = I.Observe (I.Clock.IncrementalStepFunction clock (I.Var.Watch x))

        for _ = 1 to 5 do
            I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
            fix.Stabilize ()

            values.Add $"%i{Observer.value o}"

        expect {
            snapshot
                @"unfold: 14
unfold: 15
unfold: 16
unfold: 16
15
unfold: 16
unfold: 17
unfold: 17
16
unfold: 17
unfold: 18
unfold: 18
17
unfold: 18
unfold: 19
unfold: 19
18
unfold: 19
unfold: 20
unfold: 20
19"

            return values |> String.concat "\n"
        }

        Observer.disallowFutureUse o
