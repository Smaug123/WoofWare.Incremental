namespace WoofWare.Incremental.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental
open System
open System.Threading

type TestObject (id : int) =
    member _.Id = id
    override _.ToString () = $"TestObject({id})"

[<RequireQualifiedAccess>]
module TestGc =
    [<OneTimeSetUp>]
    let oneTimeSetup () =
#if DEBUG
        do failwith "These tests only run in release mode"
#endif
        ()

    let forceGCAndFinalizers () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()

    [<Test>]
    let ``Finalizer runs when target is collected`` () =
        let mutable finalizerRan = 0
        let mutable testResult = -1

        do
            let target = TestObject 42

            Gc.addFinalizerIgnore
                target
                (fun t ->
                    Interlocked.Increment &finalizerRan |> ignore
                    testResult <- t.Id
                )

        forceGCAndFinalizers ()

        finalizerRan |> shouldEqual 1
        testResult |> shouldEqual 42

    [<Test>]
    let ``Does not keep target alive unnecessarily`` () =
        let weakRef =
            let target = TestObject 123
            let weak = WeakReference target
            Gc.addFinalizerIgnore target (fun _ -> ())
            weak

        weakRef.IsAlive |> shouldEqual true

        forceGCAndFinalizers ()

        weakRef.IsAlive |> shouldEqual false

    [<Test>]
    let ``Action receives correct target value`` () =
        let mutable receivedId = 0

        do
            let target = TestObject 999
            Gc.addFinalizerIgnore target (fun t -> receivedId <- t.Id)

        forceGCAndFinalizers ()

        receivedId |> shouldEqual 999

    [<Test>]
    let ``Finalizer does not run while object is still reachable`` () =
        let mutable finalizerRan = 0
        let target = TestObject 55

        Gc.addFinalizerIgnore target (fun _ -> Interlocked.Increment &finalizerRan |> ignore<int>)

        forceGCAndFinalizers ()

        finalizerRan |> shouldEqual 0
        // this keeps `target` alive
        target.Id |> shouldEqual 55

    [<Test>]
    let ``Finalizers run on finalizer thread`` () =
        let mutable finalizerThreadId = -1
        let mainThreadId = Thread.CurrentThread.ManagedThreadId

        do
            let target = TestObject 88
            Gc.addFinalizerIgnore target (fun _ -> finalizerThreadId <- Thread.CurrentThread.ManagedThreadId)

        forceGCAndFinalizers ()

        finalizerThreadId |> shouldNotEqual mainThreadId

    [<Test>]
    let ``Finalizer runs when target is collected 2`` () =
        let mutable finalizerRan = -1

        do
            let target = TestObject 42
            Gc.addFinalizerIgnore target (fun _ -> Interlocked.Increment &finalizerRan |> ignore)

        forceGCAndFinalizers ()

        finalizerRan |> shouldEqual 0

    [<Test>]
    let ``ConditionalWeakTable does not create strong references`` () =
        // This tests that our implementation doesn't accidentally keep objects alive
        let collectableRefs = ResizeArray<WeakReference> ()
        let finalizerCounts = ResizeArray<int> ()

        for i = 1 to 10 do
            let target = TestObject i
            let weakRef = WeakReference target
            collectableRefs.Add weakRef

            let mutable count = 0

            Gc.addFinalizerIgnore
                target
                (fun _ ->
                    let count = Interlocked.Increment &count
                    lock finalizerCounts (fun () -> finalizerCounts.Add count)
                )

        forceGCAndFinalizers ()

        // All objects should have been collected
        let aliveCount = collectableRefs |> Seq.filter (fun wr -> wr.IsAlive) |> Seq.length
        aliveCount |> shouldEqual 0
        finalizerCounts.Count |> shouldEqual 10

    [<Test>]
    let ``Finalizer works with different object types`` () =
        let results = ResizeArray<string> ()

        // Test with different types
        do
            // put on the heap, don't intern
            let stringTarget = "H" + "ello" :> obj

            Gc.addFinalizerIgnore
                stringTarget
                (fun s ->
                    let s = unbox<string> s
                    lock results (fun () -> results.Add $"String: %s{s}")
                )

            let arrayTarget = [| 1 ; 2 ; 3 |]

            Gc.addFinalizerIgnore arrayTarget (fun arr -> lock results (fun () -> results.Add $"Array: %i{arr.Length}"))

            let tupleTarget = (42, "test")
            Gc.addFinalizerIgnore tupleTarget (fun t -> lock results (fun () -> results.Add $"Tuple: %i{fst t}"))

        forceGCAndFinalizers ()

        results.Count |> shouldEqual 3
        // Check that we got results from all three types
        results
        |> Seq.exists (fun s -> s.StartsWith ("String:", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual true

        results
        |> Seq.exists (fun s -> s.StartsWith ("Array:", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual true

        results
        |> Seq.exists (fun s -> s.StartsWith ("Tuple:", StringComparison.OrdinalIgnoreCase))
        |> shouldEqual true

    [<Test>]
    let ``Stress test with many objects and finalizers`` () =
        let mutable finalizerCount = 0
        let expectedCount = 1000

        // Create many objects with finalizers
        for i in 1..expectedCount do
            let target = TestObject i
            Gc.addFinalizerIgnore target (fun _ -> Interlocked.Increment (&finalizerCount) |> ignore<int>)

        // Force multiple GC cycles to ensure all finalizers run
        for _ = 1 to 5 do
            forceGCAndFinalizers ()

        finalizerCount |> shouldEqual expectedCount
