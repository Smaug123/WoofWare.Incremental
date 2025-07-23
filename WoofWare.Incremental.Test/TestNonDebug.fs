namespace WoofWare.Incremental.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
[<NonParallelizable>]
module TestNonDebug =
    let mutable oldDebugFlag = false

    [<OneTimeSetUp>]
    let oneTimeSetup () =
#if DEBUG
        do failwith "These tests only run in release mode"
#endif
        oldDebugFlag <- Debug.globalFlag
        Debug.globalFlag <- false

    [<OneTimeTearDown>]
    let oneTimeTearDown () = Debug.globalFlag <- oldDebugFlag

    let inline noAlloc ([<InlineIfLambda>] f : unit -> unit) =
        let alloc = GC.GetTotalAllocatedBytes ()
        f ()
        let alloc2 = GC.GetTotalAllocatedBytes ()
        alloc2 |> shouldEqual alloc

    // Debug-enabled incremental allocates a bunch of words during stabilization,
    // so we only run this test for non-debug incremental.
    [<Test>]
    let ``Stabilization propagating values through an existing graph should not allocate`` () =
        let I = Incremental.make ()
        let v' = I.Var.Create 0
        let v = I.Map ((+) 1) (I.Var.Watch v')
        let w' = I.Var.Create 0
        let w = I.Map ((+) 1) (I.Var.Watch w')
        let a = I.Map ((+) 1) v
        let b = I.Map ((+) 1) w
        let o = I.Observe (I.Map2 (+) a b)

        // The first stabilization allocates, but the next two do not.
        //   The point is that stabilization shouldn't allocate anything except to create
        //   new nodes. This means that it is okay for stabilization to allocate new
        //   nodes when the rhs of a bind node changes, but if the shape of the graph
        //   has not changed, then it shouldn't allocate.

        I.Stabilize ()
        I.Var.Set v' 4

        noAlloc I.Stabilize
        I.Var.Set v' 5
        I.Var.Set w' 5
        noAlloc I.Stabilize

        Observer.disallowFutureUse o
