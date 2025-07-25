namespace WoofWare.Incremental.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental
open WoofWare.Expect

[<TestFixture>]
module TestState =
    [<Test>]
    let ``default max height`` () =
        let I = Incremental.make ()
        State.maxHeightAllowed I.State |> shouldEqual 128

    [<Test>]
    let ``max height seen`` () =
        let fix = IncrementalFixture.Make ()
        // because of the Invalid we made
        State.maxHeightSeen fix.I.State |> shouldEqual 3

    [<TestCase 1>]
    [<TestCase 2>]
    let ``can't set smaller height`` (height : int) =
        let fix = IncrementalFixture.Make ()

        expect {
            snapshotThrows @"System.Exception: cannot set_max_height_allowed less than the max height already seen"
            return! fun () -> State.setMaxHeightAllowed fix.I.State height
        }

    [<Test>]
    let ``max height setting`` () =
        let fix = IncrementalFixture.Make ()
        State.setMaxHeightAllowed fix.I.State 10
        State.maxHeightAllowed fix.I.State |> shouldEqual 10

    [<Test>]
    let ``observe max height`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        State.setMaxHeightAllowed I.State 256

        let rec loop n =
            if n = 0 then
                I.Return 0
            else
                loop (n - 1) |> I.Map (fun i -> i + 1)

        let o = I.Observe (loop (State.maxHeightAllowed I.State))

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual (State.maxHeightAllowed I.State)

        State.maxHeightAllowed I.State |> shouldEqual (State.maxHeightSeen I.State)
        State.invariant I.State

        do
            GC.Collect ()
            GC.WaitForPendingFinalizers ()
            GC.Collect ()

        fix.Stabilize ()

        do
            let n = State.numActiveObservers I.State
            let o = I.Observe (I.Const 0)
            Observer.disallowFutureUse o
            State.numActiveObservers I.State |> shouldEqual n

        do
            Gc.collect ()
            fix.Stabilize ()
            let n = State.numActiveObservers I.State
            let o = I.Observe (I.Const 0)
            fix.Stabilize ()
            State.numActiveObservers I.State |> shouldEqual (n + 1)
            Observer.disallowFutureUse o
            State.numActiveObservers I.State |> shouldEqual n
            fix.Stabilize ()
            State.numActiveObservers I.State |> shouldEqual n

        do
            Gc.collect ()
            fix.Stabilize ()

            let _o = I.Observe' true (I.Const 13)
            fix.Stabilize ()
            let n = State.numActiveObservers I.State
            Gc.collect ()
            fix.Stabilize ()

            State.numActiveObservers I.State |> shouldEqual (n - 1)

        do
            Gc.collect ()
            fix.Stabilize ()

            let _o = I.Observe' false (I.Const 13)
            fix.Stabilize ()
            let n = State.numActiveObservers I.State
            Gc.collect ()
            fix.Stabilize ()

            State.numActiveObservers I.State |> shouldEqual n

    [<Test>]
    let ``test recomputedDirectlyBecauseMinHeight`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let var = I.Var.Create 1

        let o =
            (I.Map2 (+) (I.Var.Watch var) (I.Const 1), I.Map2 (+) (I.Const 2) (I.Const 3))
            ||> I.Map2 (+)
            |> I.Observe

        fix.Stabilize ()

        let stat1 = State.numNodesRecomputedDirectlyBecauseMinHeight I.State
        I.Var.Set var 2
        fix.Stabilize ()

        let stat2 = State.numNodesRecomputedDirectlyBecauseMinHeight I.State

        stat2 - stat1 |> shouldEqual 2

        Observer.disallowFutureUse o

    [<Test>]
    let ``test recomputedDirectlyBecauseOneChild`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        // We can't use the same variable twice otherwise the optimization is not applied.

        let var1 = I.Var.Create 1
        let var2 = I.Var.Create 1

        let o var =
            I.Observe (I.Map id (I.Map id (I.Var.Watch var)))

        let o1 = o var1
        let o2 = o var2

        fix.Stabilize ()
        let stat1 = State.numNodesRecomputedDirectlyBecauseOneChild I.State
        I.Var.Set var1 2
        I.Var.Set var2 2
        fix.Stabilize ()
        let stat2 = State.numNodesRecomputedDirectlyBecauseOneChild I.State

        stat2 - stat1 |> shouldEqual 4

        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2
