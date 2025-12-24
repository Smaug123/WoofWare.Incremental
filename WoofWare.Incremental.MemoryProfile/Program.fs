namespace Program

open System
open WoofWare.Incremental
open JetBrains.dotMemoryUnit
open JetBrains.Profiler.Api

[<RequireQualifiedAccess>]
module Gc =
    let inline collect () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()

module Program =
    let inline checkAlloc (name : string) ([<InlineIfLambda>] f : unit -> unit) =
        let beforeS = "before" + name
        let afterS = "after" + name
        MemoryProfiler.CollectAllocations true
        MemoryProfiler.GetSnapshot beforeS
        let alloc = GC.GetTotalAllocatedBytes ()
        f ()
        let alloc2 = GC.GetTotalAllocatedBytes ()
        MemoryProfiler.GetSnapshot afterS

        let allocDiff = alloc2 - alloc

        if allocDiff > 0L then
            printfn "[%s] GC.GetTotalAllocatedBytes reports %d bytes allocated" name allocDiff
        else
            printfn "[%s] GC.GetTotalAllocatedBytes reports no allocations" name

    [<EntryPoint>]
    let main _ =
#if DEBUG
        failwith "This program should only be run in Release mode"
#endif

        Debug.globalFlag <- false
        Gc.collect ()

        // === Warmup phase ===
        // Exercise all code paths once to trigger any one-time allocations
        // (e.g., generic equality comparer initialization for NodeId)
        printfn "=== Warmup phase (triggering one-time initializations) ==="

        // Warm up EqualityComparer<T>.Default for value types used in tests
        // This triggers one-time RuntimeType allocation per type
        let _ = System.Collections.Generic.EqualityComparer<int>.Default
        let _ = System.Collections.Generic.EqualityComparer<unit>.Default
        let _ = System.Collections.Generic.EqualityComparer<bool>.Default

        do
            let warmupI = Incremental.make ()
            let warmupBindInput = warmupI.Var.Create 0
            let warmupInner = warmupI.Var.Create 0
            let warmupInnerNode = warmupI.Map id (warmupI.Var.Watch warmupInner)

            let warmupBound =
                warmupI.Bind (fun _ -> warmupInnerNode) (warmupI.Var.Watch warmupBindInput)

            let warmupObs = warmupI.Observe warmupBound
            warmupI.Stabilize ()
            // Trigger changeChild's Id comparison path by changing the bind input
            warmupI.Var.Set warmupBindInput 1
            warmupI.Stabilize ()
            Observer.disallowFutureUse warmupObs

        Gc.collect ()
        printfn ""
        printfn "=== Map/Map2 tests ==="
        printfn "Setting up incremental graph..."
        let I = Incremental.make ()
        let v' = I.Var.Create 0
        let v = I.Map ((+) 1) (I.Var.Watch v')
        let w' = I.Var.Create 0
        let w = I.Map ((+) 1) (I.Var.Watch w')
        let a = I.Map ((+) 1) v
        let b = I.Map ((+) 1) w
        let o = I.Observe (I.Map2 (+) a b)

        printfn "Initial stabilization (expected to allocate)..."
        I.Stabilize ()

        printfn ""
        printfn "=== First re-stabilization test ==="
        I.Var.Set v' 4
        Gc.collect ()
        checkAlloc "Stabilize after setting v'" I.Stabilize

        printfn ""
        printfn "=== Second re-stabilization test ==="
        I.Var.Set v' 5
        I.Var.Set w' 5
        Gc.collect ()
        checkAlloc "Stabilize after setting both vars" I.Stabilize

        Observer.disallowFutureUse o

        printfn ""
        printfn "=== Bind tests (graph structure unchanged) ==="
        // Create a bind where the function always returns the same node
        // regardless of the input value - this tests that when graph structure
        // doesn't change, no allocations occur
        let bindInput = I.Var.Create 0
        // Pre-create the node that the bind will return
        let innerVar = I.Var.Create 100
        let innerNode = I.Map ((+) 1) (I.Var.Watch innerVar)
        // The bind function ignores its input and always returns the same node
        let boundNode = I.Bind (fun _ -> innerNode) (I.Var.Watch bindInput)
        let bindObserver = I.Observe boundNode

        printfn "Initial bind stabilization (expected to allocate)..."
        I.Stabilize ()

        printfn ""
        I.Var.Set bindInput 1
        Gc.collect ()
        checkAlloc "Bind: change input, same graph structure" I.Stabilize

        printfn ""
        I.Var.Set bindInput 2
        I.Var.Set innerVar 200
        Gc.collect ()
        checkAlloc "Bind: change input and inner var" I.Stabilize

        printfn ""
        I.Var.Set innerVar 300
        Gc.collect ()
        checkAlloc "Bind: change only inner var (no lhs change)" I.Stabilize

        Observer.disallowFutureUse bindObserver

        printfn ""
        printfn "=== Join tests ==="
        // Join is similar to Bind - when the inner node stays the same, should be allocation-free
        let joinInput = I.Var.Create 0
        let joinInner = I.Var.Create 42
        let joinInnerNode = I.Map ((+) 1) (I.Var.Watch joinInner)
        // Map to always return the same node
        let nodeOfNode = I.Map (fun _ -> joinInnerNode) (I.Var.Watch joinInput)
        let joinedNode = I.Join nodeOfNode
        let joinObserver = I.Observe joinedNode

        printfn "Initial join stabilization (expected to allocate)..."
        I.Stabilize ()

        printfn ""
        I.Var.Set joinInput 1
        Gc.collect ()
        checkAlloc "Join: change outer, same inner node" I.Stabilize

        printfn ""
        I.Var.Set joinInner 100
        Gc.collect ()
        checkAlloc "Join: change only inner value" I.Stabilize

        Observer.disallowFutureUse joinObserver

        printfn ""
        printfn "=== If/Then/Else tests ==="
        let condition = I.Var.Create true
        let thenBranch = I.Var.Create 1
        let elseBranch = I.Var.Create 2

        let ifNode =
            I.If (I.Var.Watch condition) (I.Var.Watch thenBranch) (I.Var.Watch elseBranch)

        let ifObserver = I.Observe ifNode

        printfn "Initial if stabilization (expected to allocate)..."
        I.Stabilize ()

        printfn ""
        I.Var.Set thenBranch 10
        Gc.collect ()
        checkAlloc "If: change active branch value" I.Stabilize

        printfn ""
        I.Var.Set elseBranch 20
        Gc.collect ()
        checkAlloc "If: change inactive branch value" I.Stabilize

        printfn ""
        I.Var.Set condition false
        Gc.collect ()
        checkAlloc "If: switch branch" I.Stabilize

        printfn ""
        I.Var.Set elseBranch 30
        Gc.collect ()
        checkAlloc "If: change newly active branch" I.Stabilize

        Observer.disallowFutureUse ifObserver

        printfn ""
        printfn "Done."
        0
