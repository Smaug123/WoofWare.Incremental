namespace WoofWare.Incremental.Test

open System
open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Expect

[<TestFixture>]
module TestObserver =

    [<Test>]
    let ``toString test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let t = I.Observe (I.Var.Watch (I.Var.Create 13))

        expect {
            snapshot "<unstabilized>"
            return Observer.toString t
        }

        fix.Stabilize ()

        expect {
            snapshot "13"
            return Observer.toString t
        }

        Observer.disallowFutureUse t

        expect {
            snapshot "<disallowed>"
            return Observer.toString t
        }

    [<Test>]
    let ``observe and watch`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        let o = I.Observe (I.Var.Watch x)

        Object.ReferenceEquals (I.Var.Watch x, Observer.observing o) |> shouldEqual true

    [<Test>]
    let ``useIsAllowed test`` () =
        let I = Incremental.make ()
        let o = I.Observe (I.Var.Watch (I.Var.Create 0))

        Observer.useIsAllowed o |> shouldEqual true
        Observer.disallowFutureUse o
        Observer.useIsAllowed o |> shouldEqual false

    [<Test>]
    let ``calling value before stabilizing returns error`` () =
        let I = Incremental.make ()
        let x = I.Var.Create 0
        let o = I.Observe (I.Var.Watch x)
        Observer.value(o).IsError |> shouldEqual true

        expect {
            snapshotThrows @"System.Exception: Observer.valueThrowing called without stabilizing"
            return! fun () -> Observer.valueThrowing o
        }

    [<Test>]
    let ``calling value on a just-created observer of an already computed incremental before stabilizing is error`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()

        Observer.disallowFutureUse o
        I.Var.Set x 14
        fix.Stabilize ()
        I.Var.Set x 15
        let o = I.Observe (I.Var.Watch x)

        Observer.value(o).IsError |> shouldEqual true

        expect {
            snapshotThrows @"System.Exception: Observer.valueThrowing called without stabilizing"
            return! fun () -> Observer.valueThrowing o
        }

    [<Test>]
    let ``calling value after disallowFutureUse returns error`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()
        Observer.disallowFutureUse o

        Observer.value(o).IsError |> shouldEqual true

        expect {
            snapshotThrows @"System.Exception: Observer.valueThrowing called after disallowFutureUse"
            return! fun () -> Observer.valueThrowing o
        }

    [<Test>]
    let ``disallowFutureUse disables on-update handlers`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)
        let mutable r = 0

        Observer.onUpdateThrowing o (fun _ -> Interlocked.Increment &r |> ignore<int>)
        fix.Stabilize ()
        r |> shouldEqual 1

        Observer.disallowFutureUse o
        I.Var.Set x 14
        fix.Stabilize ()

        r |> shouldEqual 1

    [<Test ; Explicit "not yet passing">]
    let ``finalizers work`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        Gc.collect ()

        fix.Stabilize ()
        let before = State.numActiveObservers I.State

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        State.numActiveObservers I.State |> shouldEqual (before + 1)

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13

        Gc.collect ()

        State.numActiveObservers I.State |> shouldEqual (before + 1)

        Gc.collect ()

        State.numActiveObservers I.State |> shouldEqual before

    [<Test>]
    let ``finalizers don't disable on-update handlers`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)
        let mutable r = 0

        Observer.onUpdateThrowing o (fun _ -> Interlocked.Increment &r |> ignore<int>)

        fix.Stabilize ()
        r |> shouldEqual 1

        Gc.collect ()

        I.Var.Set x 14
        fix.Stabilize ()

        r |> shouldEqual 2

    [<Test ; Explicit "not yet passing">]
    let ``finalizers cause an Unnecessary update to be sent`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch x) push

        fix.Stabilize ()

        check [ NodeUpdate.Necessary 13 ]

        GC.KeepAlive o

        Gc.collect ()
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

    [<Test>]
    let ``disallowFutureUse and finalize in the same stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 1
        let o = I.Observe (I.Var.Watch x)

        fix.Stabilize ()

        Observer.disallowFutureUse o
        Gc.collect ()

        fix.Stabilize ()

    [<Test>]
    let ``finalize after disallowFutureUse`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 1
        let o = I.Observe (I.Var.Watch x)

        fix.Stabilize ()

        Observer.disallowFutureUse o

        fix.Stabilize ()

        // This collect + stabilize causes the finalizer for o to run.
        // Assertion: since disallowFutureUse has already been called, the finalizer doesn't do
        // anything wrong.

        Gc.collect ()
        fix.Stabilize ()

    [<Test>]
    let ``After user resurrection of an observer, it is still disallowed`` () =
#if DEBUG
        Assert.Inconclusive "This test depends on the GC running finalizers in a timely manner"
#endif
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        fix.Stabilize ()
        GC.KeepAlive o
        let mutable r = None
        Gc.addFinalizerIgnore o (fun o -> r <- Some o)
        Gc.collect ()
        fix.Stabilize ()

        let o = r.Value
        Observer.useIsAllowed o |> shouldEqual false

    [<Test>]
    let ``lots of observers on the same node isn't quadratic`` () =
        let I = Incremental.make ()
        let t = I.Const 13
        let obs = List.init 100_000 (fun _ -> I.Observe t)
        // We don't run the invariant here, because that's too slow.
        // The original OCaml measured CPU time here.
        I.Stabilize ()

        for obs in obs do
            Observer.disallowFutureUse obs

        I.Stabilize ()

    [<Test>]
    let ``another test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let parent = I.Var.Watch x |> I.Map (fun x -> x + 1)

        let parentO = I.Observe parent
        let mutable numCalls = 0
        let mutable r = 0

        Observer.onUpdateThrowing
            parentO
            (fun u ->
                match u with
                | Observer.Update.Initialized i
                | Observer.Update.Changed (_, i) ->
                    Interlocked.Increment &numCalls |> ignore<int>
                    r <- i
                | Observer.Update.Invalidated -> failwith "oh no"
            )

        fix.Stabilize ()

        numCalls |> shouldEqual 1
        I.Var.Set x 15
        fix.Stabilize ()
        numCalls |> shouldEqual 2
        r |> shouldEqual 16

        Observer.disallowFutureUse parentO
        I.Var.Set x 17

        fix.Stabilize ()
        numCalls |> shouldEqual 2
        r |> shouldEqual 16

    [<Test>]
    let ``onUpdateThrowing of an invalid node, not during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let o = fix.I.Observe fix.Invalid

        Observer.onUpdateThrowing o (fun _ -> failwith "oh no")
        Observer.disallowFutureUse o

    [<Test>]
    let ``onUpdateThrowing of an invalid node`` () =
        let fix = IncrementalFixture.Make ()
        let o = fix.I.Observe fix.Invalid
        let mutable isOk = false

        Observer.onUpdateThrowing
            o
            (fun update ->
                match update with
                | Observer.Update.Invalidated -> isOk <- true
                | _ -> failwith "should not hit"
            )

        fix.Stabilize ()
        isOk |> shouldEqual true

    [<Test>]
    let ``stabilizing with an on-update handler of a node that is invalidated`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        let mutable r = None

        let o1 =
            I.Var.Watch x
            |> I.Bind (fun i ->
                let t = I.Const i
                r <- Some t
                t
            )
            |> I.Observe

        fix.Stabilize ()

        let o2 = I.Observe r.Value
        let mutable invalidated = false

        Observer.onUpdateThrowing
            o2
            (fun update ->
                match update with
                | Observer.Update.Invalidated -> invalidated <- true
                | _ -> ()
            )

        fix.Stabilize ()

        invalidated |> shouldEqual false

        I.Var.Set x 1
        fix.Stabilize ()
        invalidated |> shouldEqual true

        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``onUpdateThrowing of a disallowed observer`` () =
        let I = Incremental.make ()
        let o = I.Observe (I.Const 5)
        Observer.disallowFutureUse o

        expect {
            snapshotThrows @"System.Exception: onUpdate disallowed"
            return! fun () -> Observer.onUpdateThrowing o (fun _ -> failwith "should not call")
        }

    [<Test>]
    let ``disallowFutureUse before first stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.Observe (I.Const 5)
        Observer.disallowFutureUse o

        fix.Stabilize ()
        Observer.disallowFutureUse o

    [<Test>]
    let ``disallowFutureUse during an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        Observer.onUpdateThrowing o (fun _ -> Observer.disallowFutureUse o)
        fix.Stabilize ()

        Observer.value(o).IsError |> shouldEqual true

    [<Test>]
    let ``disallowing other on-update handlers in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        for _ = 1 to 2 do
            Observer.onUpdateThrowing
                o
                (fun _ ->
                    Observer.useIsAllowed o |> shouldEqual true
                    Observer.disallowFutureUse o
                )

        fix.Stabilize ()

    [<Test>]
    let ``disallowing other observers of the same node in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o1 = I.Observe (I.Var.Watch x)
        let o2 = I.Observe (I.Var.Watch x)
        let o3 = I.Observe (I.Var.Watch x)

        for o in [ o1 ; o3 ] do
            Observer.onUpdateThrowing o (fun _ -> Observer.useIsAllowed o |> shouldEqual true)

        Observer.onUpdateThrowing
            o2
            (fun _ ->
                Observer.disallowFutureUse o1
                Observer.disallowFutureUse o3
            )

        I.Stabilize ()

    [<Test>]
    let ``disallowing observers of other nodes in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o () =
            I.Observe (I.Var.Watch (I.Var.Create 13))

        let o1 = o ()
        let o2 = o ()
        let o3 = o ()

        for o in [ o1 ; o3 ] do
            Observer.onUpdateThrowing o (fun _ -> Observer.useIsAllowed o |> shouldEqual true)

        Observer.onUpdateThrowing
            o2
            (fun _ ->
                Observer.disallowFutureUse o1
                Observer.disallowFutureUse o3
            )

        I.Stabilize ()

    [<Test>]
    let ``adding an on-update handler to an already stable node`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13 |> I.Var.Watch
        let o = I.Observe x

        fix.Stabilize ()

        let mutable didRun = false
        Observer.onUpdateThrowing (I.Observe x) (fun _ -> didRun <- true)
        didRun |> shouldEqual false

        fix.Stabilize ()
        didRun |> shouldEqual true

        Observer.disallowFutureUse o

    [<Test>]
    let ``adding an on-update handler after a change`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        let push1, check1 = onObserverUpdateQueue ()
        Observer.onUpdateThrowing o push1

        fix.Stabilize ()

        check1 [ Observer.Update.Initialized 13 ]

        I.Var.Set x 14
        fix.Stabilize ()

        check1 [ Observer.Update.Changed (13, 14) ]

        let push2, check2 = onObserverUpdateQueue ()
        Observer.onUpdateThrowing o push2
        fix.Stabilize ()

        check2 [ Observer.Update.Initialized 14 ]

        I.Var.Set x 15
        fix.Stabilize ()

        check1 [ Observer.Update.Changed (14, 15) ]
        check2 [ Observer.Update.Changed (14, 15) ]

    [<Test>]
    let ``adding an on-update handler in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)

        let mutable didRun = false

        Observer.onUpdateThrowing
            o
            (fun _ -> Observer.onUpdateThrowing (I.Observe (I.Var.Watch x)) (fun _ -> didRun <- true))

        fix.Stabilize ()
        didRun |> shouldEqual false

        I.Var.Set x 14
        fix.Stabilize ()
        didRun |> shouldEqual true

        Gc.collect ()
        fix.Stabilize ()

    [<Test>]
    let ``adding an on-update handler to an invalid node in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let o = I.Observe (I.Var.Watch (I.Var.Create 13))
        let mutable isOk = false

        Observer.onUpdateThrowing
            o
            (fun _ ->
                Observer.onUpdateThrowing
                    (I.Observe fix.Invalid)
                    (fun u ->
                        match u with
                        | Observer.Update.Invalidated -> isOk <- true
                        | _ -> failwith "unexpected"
                    )
            )

        fix.Stabilize ()
        isOk |> shouldEqual false
        fix.Stabilize ()
        isOk |> shouldEqual true

    [<TestCase true>]
    [<TestCase false>]
    let ``on-update handlers added during firing of other handlers should wait for next stabilization`` (valid : bool) =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let node = if valid then I.Const 1 else fix.Invalid

        let o1 = I.Observe (I.Const 1)
        let o2 = I.Observe node
        let mutable ran = 0

        Observer.onUpdateThrowing
            o1
            (fun _ -> Observer.onUpdateThrowing o2 (fun _ -> Interlocked.Increment &ran |> ignore<int>))

        Observer.onUpdateThrowing o2 ignore

        ran |> shouldEqual 0
        fix.Stabilize ()
        ran |> shouldEqual 0
        fix.Stabilize ()
        ran |> shouldEqual 1
        fix.Stabilize ()
        ran |> shouldEqual 1

        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``on-update handler set up during stabilization fires after the stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let mutable called = false
        let unit = I.Const ()
        let oUnit = I.Observe unit

        let o =
            unit
            |> I.Map (fun () -> Observer.onUpdateThrowing oUnit (fun _ -> called <- true))
            |> I.Observe

        called |> shouldEqual false
        fix.Stabilize ()
        called |> shouldEqual true

        Observer.disallowFutureUse o
        Observer.disallowFutureUse oUnit

    [<Test>]
    let ``on-update handlers are initialized once`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create (I.Const 0)
        let i = I.Var.Watch v
        let o = I.Observe i

        let oldValIsNoneOnce () =
            let mutable isFirstCall = true

            fun update ->
                match update with
                | Observer.Update.Initialized _ ->
                    isFirstCall |> shouldEqual true
                    isFirstCall <- false
                | Observer.Update.Changed _ -> isFirstCall |> shouldEqual false
                | Observer.Update.Invalidated -> failwith "should not hit"

        Observer.onUpdateThrowing o (oldValIsNoneOnce ())
        fix.Stabilize ()
        Observer.onUpdateThrowing o (oldValIsNoneOnce ())
        fix.Stabilize ()

    [<Test>]
    let ``creating an observer during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let mutable r = None

        let o1 =
            I.Var.Watch x
            |> I.Map (fun _ ->
                let o2 = I.Observe (I.Var.Watch x)
                Observer.useIsAllowed o2 |> shouldEqual true
                Observer.value(o2).IsError |> shouldEqual true
                r <- Some o2
                0
            )
            |> I.Observe

        fix.Stabilize ()
        let o2 = r.Value
        Observer.useIsAllowed o2 |> shouldEqual true
        Observer.value(o2).IsError |> shouldEqual true
        fix.Stabilize ()
        Observer.valueThrowing o2 |> shouldEqual 13

        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2
        fix.Stabilize ()
        Observer.value(o2).IsError |> shouldEqual true

    [<Test>]
    let ``creating an observer and adding on-update handler during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 0
        let push, check = onObserverUpdateQueue ()
        let mutable innerObs = None

        let o =
            I.Var.Watch v
            |> I.Map (fun i ->
                let obs = I.Observe (I.Var.Watch v)
                innerObs <- Some obs
                Observer.onUpdateThrowing obs push
                i
            )
            |> I.Observe

        check []
        fix.Stabilize ()
        check []
        fix.Stabilize ()
        check [ Observer.Update.Initialized 0 ]

        Observer.disallowFutureUse o
        Observer.disallowFutureUse innerObs.Value

    [<Test>]
    let ``disallowFutureUse during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let mutable handlerRan = false
        let o1 = I.Var.Watch x |> I.Observe

        let o2 =
            I.Var.Watch x
            |> I.Map (fun i ->
                Observer.onUpdateThrowing o1 (fun _ -> handlerRan <- true)
                Observer.disallowFutureUse o1
                Observer.useIsAllowed o1 |> shouldEqual false
                i
            )
            |> I.Observe

        Observer.useIsAllowed o1 |> shouldEqual true
        handlerRan |> shouldEqual false

        fix.Stabilize ()
        Observer.useIsAllowed o1 |> shouldEqual false
        handlerRan |> shouldEqual false

        Observer.disallowFutureUse o2

    [<Test>]
    let ``creating an observer and disallowing use during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let mutable r = None

        let o1 =
            I.Var.Watch x
            |> I.Map (fun _ ->
                let o2 = I.Observe (I.Var.Watch x)
                r <- Some o2
                Observer.disallowFutureUse o2
                0
            )
            |> I.Observe

        fix.Stabilize ()
        let o2 = r.Value
        Observer.useIsAllowed o2 |> shouldEqual false
        Observer.disallowFutureUse o1
        fix.Stabilize ()

    [<Test>]
    let ``creating an observer and finalizing it during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13

        let o =
            I.Var.Watch x
            |> I.Map (fun _ ->
                I.Observe (I.Var.Watch x) |> ignore<Observer<_>>
                Gc.collect ()
                0
            )
            |> I.Observe

        fix.Stabilize ()
        fix.Stabilize ()
        Observer.disallowFutureUse o
