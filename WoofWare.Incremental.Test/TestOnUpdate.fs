namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel

[<TestFixture>]
module TestOnUpdate =

    [<Test>]
    let ``on update test 1`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 13
        let push, check = onUpdateQueue ()
        let o = I.Observe (I.Var.Watch v)

        I.OnUpdate (I.Var.Watch v) push

        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        fix.Stabilize ()
        check []

        I.Var.Set v 14
        fix.Stabilize ()
        check [ NodeUpdate.Changed (13, 14) ]

        Observer.disallowFutureUse o
        I.Var.Set v 15
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

    [<Test>]
    let ``on-change handlers of a node that changes but is not necessary at the end of stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 0
        let n = I.Var.Watch v
        let push, check = onUpdateQueue ()
        I.OnUpdate n push

        let o = I.Observe n
        fix.Stabilize ()
        check [ NodeUpdate.Necessary 0 ]

        Observer.disallowFutureUse o
        I.Var.Set v 1
        let o = I.Observe (I.Freeze n)
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]
        Observer.disallowFutureUse o

    [<Test>]
    let ``value changing with different observers`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 13
        let o = I.Observe (I.Var.Watch v)
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch v) push

        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        Observer.disallowFutureUse o
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

        I.Var.Set v 14
        let o = I.Observe (I.Var.Watch v)
        fix.Stabilize ()
        Observer.disallowFutureUse o
        check [ NodeUpdate.Necessary 14 ]

    [<Test>]
    let ``call at next stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 13
        let o = I.Observe (I.Var.Watch v)
        fix.Stabilize ()

        let mutable r = 0
        I.OnUpdate (I.Var.Watch v) (fun _ -> Interlocked.Increment &r |> ignore<int>)
        fix.Stabilize ()
        r |> shouldEqual 1

        Observer.disallowFutureUse o

    [<Test>]
    let ``called at next stabilization with unnecessary update`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v = I.Var.Create 13
        let o = I.Observe (I.Var.Watch v)
        fix.Stabilize ()
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch v) push

        Observer.disallowFutureUse o
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

    [<Test>]
    let ``transition from unnecessary to necessary and back`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch x) push
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]

        I.Var.Set x 14
        fix.Stabilize ()
        check [ NodeUpdate.Changed (13, 14) ]

        Observer.disallowFutureUse o
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

    [<Test>]
    let ``an indirectly necessary node`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 13
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch x) push
        let t = I.Var.Watch x |> I.Map (fun i -> i + 1)
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]
        let o = I.Observe t
        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        Observer.disallowFutureUse o
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]
        let o = I.Observe t
        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        Observer.disallowFutureUse o
        fix.Stabilize ()
        check [ Unnecessary ]

    [<Test>]
    let ``OnUpdate doesn't make a node necessary`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let v = I.Var.Create 13
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch v) push
        fix.Stabilize ()
        check [ Unnecessary ]
        I.Var.Set v 14
        fix.Stabilize ()
        check []
        let o = I.Observe (I.Var.Watch v)
        fix.Stabilize ()
        check [ Necessary 14 ]
        Observer.disallowFutureUse o

    [<Test>]
    let ``invalid from the start`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let push, check = onUpdateQueue ()
        I.OnUpdate fix.Invalid push
        fix.Stabilize ()
        check [ Invalidated ]

    [<Test>]
    let ``invalidation of an unnecessary node`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let v = I.Var.Create 13
        let mutable r = None

        let o =
            I.Var.Watch v
            |> I.Bind (fun i ->
                r <- Some (I.Const i)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let i = r.Value
        let push, check = onUpdateQueue ()
        I.OnUpdate i push
        fix.Stabilize ()
        check [ Unnecessary ]
        I.Var.Set v 14
        fix.Stabilize ()
        check [ Invalidated ]
        Observer.disallowFutureUse o

    [<Test>]
    let ``invalidation of a necessary node`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let v = I.Var.Create 13
        let mutable r = None

        let o1 =
            I.Var.Watch v
            |> I.Bind (fun i ->
                r <- Some (I.Const i)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let i = r.Value
        let o2 = I.Observe i
        let push, check = onUpdateQueue ()
        I.OnUpdate i push
        fix.Stabilize ()
        check [ Necessary 13 ]
        I.Var.Set v 14
        fix.Stabilize ()
        check [ Invalidated ]
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``invalidation of a necessary node after a change`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let v = I.Var.Create 13
        let w = I.Var.Create 14
        let mutable r = None

        let o1 =
            I.Var.Watch v
            |> I.Bind (fun _ ->
                r <- I.Var.Watch w |> I.Map id |> Some
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let i = r.Value
        let o2 = I.Observe i
        let push, check = onUpdateQueue ()
        I.OnUpdate i push
        fix.Stabilize ()
        check [ Necessary 14 ]
        I.Var.Set w 15
        fix.Stabilize ()
        check [ Changed (14, 15) ]
        I.Var.Set v 16
        fix.Stabilize ()
        check [ Invalidated ]
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``making a node necessary from an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 13
        let y = I.Var.Create 14
        let mutable r = None
        let pushX, checkX = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch x) pushX
        let o = I.Observe (I.Var.Watch y)
        let pushO, checkO = onObserverUpdateQueue ()

        Observer.onUpdateThrowing
            o
            (fun u ->
                pushO u
                r <- Some (I.Observe (I.Var.Watch x))
            )

        fix.Stabilize ()
        checkX [ NodeUpdate.Unnecessary ]
        checkO [ Observer.Update.Initialized 14 ]
        let ox = r.Value
        I.Var.Set x 15
        fix.Stabilize ()
        checkX [ NodeUpdate.Necessary 15 ]
        checkO []
        Observer.disallowFutureUse o
        Observer.disallowFutureUse ox

    [<Test>]
    let ``calling advanceClock in an on-update handler`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let i = I.Clock.After clock (TimeNs.Span.ofSec 1.0)
        let o = I.Observe i
        let mutable numFires = 0

        I.OnUpdate
            i
            (fun _ ->
                Interlocked.Increment &numFires |> ignore<int>
                I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 2.)
            )

        numFires |> shouldEqual 0
        fix.Stabilize ()
        numFires |> shouldEqual 1
        fix.Stabilize ()
        numFires |> shouldEqual 2
        Observer.disallowFutureUse o
