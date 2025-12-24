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
        MemoryProfiler.CollectAllocations true
        MemoryProfiler.GetSnapshot "before"
        let alloc = GC.GetTotalAllocatedBytes ()
        f ()
        let alloc2 = GC.GetTotalAllocatedBytes ()
        MemoryProfiler.GetSnapshot "after"

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
        printfn "Done."
        0
