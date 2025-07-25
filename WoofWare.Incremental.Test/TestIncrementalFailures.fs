namespace WoofWare.Incremental.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Expect
open WoofWare.Incremental
open WoofWare.TimingWheel

[<TestFixture>]
module TestIncrementalFailures =

    [<Test>]
    let ``stabilizing while stabilizing`` () =
        let i = Incremental.make ()
        let o = i.Observe (i.Const () |> i.Map (fun () -> i.Stabilize ()))

        expect {
            snapshotThrows @"System.Exception: cannot stabilize during stabilization"
            return! fun () -> i.Stabilize ()
        }

        Observer.disallowFutureUse o

    [<Test>]
    let ``setMaxHeightAllowed while stabilizing`` () =
        let i = Incremental.make ()

        let o =
            i.Observe (i.Const () |> i.Map (fun () -> i.SetMaxHeightAllowed 13))

        expect {
            snapshotThrows @"System.Exception: cannot set_max_height_allowed during stabilization"
            return! fun () -> i.Stabilize ()
        }

        Observer.disallowFutureUse o

    [<Test>]
    let ``creating a cycle`` () =
        let fix = IncrementalFixture.Make ()
        let i = fix.I
        let x = i.Var.Create 1
        let r = ref (i.Const 2)
        let j = i.Var.Watch x |> i.Bind (fun x -> r.Value |> i.Map (fun y -> x + y))
        let o = i.Observe j

        fix.Stabilize ()

        Observer.valueThrowing o |> shouldEqual 3

        r.Value <- j

        i.Var.Set x 0

        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> i.Stabilize ()
        }

    [<Test>]
    let ``making a node necessary without its scope being necessary`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let r = ref None
        let x = I.Var.Create 13

        let o =
            I.Var.Watch x
            |> I.Bind (fun i ->
                r.Value <- Some (I.Const i)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let inner = r.Value.Value
        Observer.disallowFutureUse o
        fix.Stabilize ()
        // make `inner`'s scope unnecessary
        let o = I.Observe inner

        expect {
            snapshotThrows @"System.Exception: Trying to make a node necessary whose defining bind is not necessary"
            return! fun () -> I.Stabilize ()
        }

        Observer.disallowFutureUse o

    [<Test>]
    let ``stabilizing in an on-update handler`` () =
        let I = Incremental.make ()
        let x = I.Var.Create 13
        let o = I.Observe (I.Var.Watch x)
        Observer.onUpdateThrowing o (fun _ -> I.Stabilize ())

        expect {
            snapshotThrows @"System.Exception: cannot stabilize during on-update handlers"
            return! fun () -> I.Stabilize ()
        }

        Observer.disallowFutureUse o

    [<Test>]
    let ``snapshot cycle`` () =
        let I = Incremental.make ()
        let clock = I.Clock.Create TimeNs.epoch
        let x = I.Var.Create (I.Const 14)

        let s =
            I.Clock.Snapshot clock (I.Join (I.Var.Watch x)) (Clock.now clock) 13
            |> Result.get

        I.Clock.AdvanceClockBy clock (TimeNs.Span.ofSec 1.0)
        I.Var.Set x s

        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> I.Stabilize ()
        }

    [<Test>]
    let ``snapshot cycle in the future`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let clock = I.Clock.Create TimeNs.epoch
        let mutable r = None
        let valueAt = I.Const () |> I.Bind (fun () -> r.Value)

        let s =
            I.Clock.Snapshot clock valueAt (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 1.0)) 13
            |> Result.get

        r <- Some s
        let o1 = I.Observe valueAt
        let o2 = I.Observe s
        fix.Stabilize ()
        // advanceClock throws because the snapshot's valueAt depends on the snapshot itself.
        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> I.Clock.AdvanceClock clock (TimeNs.add (Clock.now clock) (TimeNs.Span.ofSec 2.0))
        }

        GC.KeepAlive o1
        GC.KeepAlive o2

    [<Test>]
    let ``another cycle`` () =
        let I = Incremental.make ()
        let v = I.Var.Create (I.Const 0)
        let w = I.Var.Create (I.Const 0)
        let a = I.Join (I.Var.Watch v)
        let b = I.Join (I.Var.Watch w)
        let oa = I.Observe a
        let ob = I.Observe b

        expect {
            snapshotThrows @"System.Exception: node too large height"

            return!
                fun () ->
                    for _ = 1 to 200 do
                        I.Var.Set w a
                        I.Stabilize ()
                        I.Var.Set w (I.Const 0)
                        I.Stabilize ()
                        I.Var.Set v b
                        I.Stabilize ()
                        I.Var.Set v (I.Const 0)
                        I.Stabilize ()
        }

        GC.KeepAlive oa
        GC.KeepAlive ob

    [<Test>]
    let ``another cycle 2`` () =
        let I = Incremental.make ()
        let v = I.Var.Create (I.Const 0)
        let w = I.Var.Create (I.Const 0)
        let a = I.Join (I.Var.Watch v)
        let b = I.Join (I.Var.Watch w)
        let o = I.Observe (I.Map2 (+) a b)
        // b depends on a
        I.Var.Set w a
        I.Var.Set v (I.Const 2)
        I.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 4
        // a depends on b
        I.Var.Set w (I.Const 3)
        I.Var.Set v b

        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> I.Stabilize ()
        }

    [<Test>]
    let ``another cycle 2 but reversed`` () =
        let I = Incremental.make ()

        for join in [ I.Join ; I.Bind id ] do
            let v = I.Var.Create (I.Const 0)
            let w = I.Var.Create (I.Const 0)
            let a = join (I.Var.Watch v)
            let b = join (I.Var.Watch w)
            let o = I.Observe (I.Map2 (+) a b)
            I.Stabilize ()
            Observer.valueThrowing o |> shouldEqual 0
            // b depends on a, doing `Var.Set` in the other order
            I.Var.Set v (I.Const 2)
            I.Var.Set w a
            I.Stabilize ()
            Observer.valueThrowing o |> shouldEqual 4
            // a depends on b
            I.Var.Set v b
            I.Var.Set w (I.Const 3)
            I.Stabilize ()
            Observer.valueThrowing o |> shouldEqual 6

    [<Test>]
    let ``The cycle that isn't created in the above two tests`` () =
        let I = Incremental.make ()
        let v = I.Var.Create (I.Const 0)
        let w = I.Var.Create (I.Const 0)
        let a = I.Join (I.Var.Watch v)
        let b = I.Join (I.Var.Watch w)
        let o = I.Observe (I.Map2 (+) a b)
        I.Var.Set v b
        I.Var.Set w a

        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> I.Stabilize ()
        }

        GC.KeepAlive o

    [<Test>]
    let ``another thing`` () =
        let I = Incremental.make ()
        let v = I.Var.Create true
        let p = I.Var.Watch v
        let a = ref (I.Const 0)
        let b = ref (I.Const 0)
        a.Value <- p |> I.Bind (fun p -> if p then I.Const 2 else b.Value)
        b.Value <- p |> I.Bind (fun p -> if p then a.Value else I.Const 3)
        let o = I.Observe (I.Map2 (+) a.Value b.Value)

        I.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 4

        I.Var.Set v false

        expect {
            snapshotThrows @"System.Exception: adding edge made graph cyclic"
            return! fun () -> I.Stabilize ()
        }

    let timeNsOfString (s : string) : TimeNs =
        let sinceEpoch = DateTime.Parse(s).Subtract(DateTime.UnixEpoch).TotalMicroseconds
        sinceEpoch * 10.0 |> int64<float> |> (*) 1L<timeNs>

    [<Test>]
    let ``atIntervals doesn't try to add alarms before the current time`` () =
        let I = Incremental.make ()

        let clock : Clock =
            I.Clock.Create'
                (TimingWheelConfig.create
                    None
                    (LevelBits.createThrowing [ 11 ; 10 ; 10 ; 10 ; 10 ; 10 ])
                    (AlarmPrecision.mul AlarmPrecision.aboutOneMillisecond 3))
                (timeNsOfString "2014-01-09 00:00:00.000000-05:00")

        I.Clock.AdvanceClock clock (timeNsOfString "2014-01-09 09:35:05.030000-05:00")
        let t = I.Clock.AtIntervals clock (TimeNs.Span.ofSec 1.0)

        let o = I.Observe t
        I.Stabilize ()
        // Here, we advance to a time that has the bad property mentioned above, due to floating point precision.
        // A previously buggy implementation of Incremental raised at this point because
        // it tried to add the next alarm for the [at_intervals] in the past.
        I.Clock.AdvanceClock clock (timeNsOfString "2014-01-09 09:35:05.040000-05:00")
        I.Stabilize ()
        Observer.disallowFutureUse o

    [<Test>]
    let ``Updating var during partial stabilization should not reflect until next stabilization`` () =
        let I = Incremental.make ()
        let v = I.Var.Create 0
        let x = I.Var.Watch v |> I.Map (fun v -> v + 1)
        let y = I.Var.Watch v |> I.Map (fun v -> v - 1)
        let z = I.Both x y |> I.Map (fun (x, y) -> x + y)
        let o = I.Observe z

        I.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 0

        match I.Expert.DoOneStepOfStabilize () with
        | StepResult.KeepGoing -> ()
        | StepResult.Done -> failwith "oh no"

        I.Var.Set v 1

        while I.Expert.DoOneStepOfStabilize().IsKeepGoing do
            ()

        Observer.valueThrowing o |> shouldEqual 0
        I.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 2

    [<Test>]
    let ``stabilizing in the middle of a partial stabilization should raise`` () =
        let I = Incremental.make ()
        let v = I.Var.Create 0
        let x = I.Var.Watch v |> I.Map (fun v -> v + 1)
        let y = I.Var.Watch v |> I.Map (fun v -> v - 1)
        let z = I.Both x y |> I.Map (fun (x, y) -> x + y)
        let o = I.Observe z

        I.Stabilize ()

        Observer.valueThrowing o |> shouldEqual 0

        match I.Expert.DoOneStepOfStabilize () with
        | StepResult.KeepGoing -> ()
        | StepResult.Done -> failwith "oh no"

        I.Var.Set v 1

        expect {
            snapshotThrows @"System.Exception: cannot stabilize during stabilization"
            return! fun () -> I.Stabilize ()
        }

    [<Test>]
    let ``can't step after stabilization raises`` () =
        let I = Incremental.make ()
        let v = I.Var.Create 0
        let x = I.Var.Watch v |> I.Map (fun _ -> failwith<unit> "nope")
        let o = I.Observe x

        expect {
            snapshotThrows @"System.Exception: nope"
            return! fun () -> I.Stabilize ()
        }

        expect {
            snapshotThrows @"System.AggregateException: cannot step -- stabilize previously raised (nope)"
            return! fun () -> I.Expert.DoOneStepOfStabilize ()
        }

        Observer.disallowFutureUse o

    [<Test>]
    let ``can't step inside update handler`` () =
        let I = Incremental.make ()
        let v = I.Var.Create 0
        let x = I.Var.Watch v |> I.Map (fun v -> v + 1)
        let o = I.Observe x

        Observer.onUpdateThrowing o (fun _ -> I.Expert.DoOneStepOfStabilize () |> ignore)

        expect {
            snapshotThrows @"System.Exception: cannot step during on-update handlers"
            return! fun () -> I.Stabilize ()
        }

        Observer.disallowFutureUse o
