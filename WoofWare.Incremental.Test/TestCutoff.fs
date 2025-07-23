namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open FsCheck
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestCutoff =
    [<Test>]
    let ``test ofCompare`` () =
        let t = Cutoff.ofCompare (fun (a : int) (b : int) -> a.CompareTo b)

        let property (a : int) (b : int) =
            Cutoff.shouldCutoff t a b |> shouldEqual (a <= b)

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``test equal`` () =
        let t = Cutoff.ofEqual (fun (a : int) b -> a = b)

        let property (a : int) =
            Cutoff.shouldCutoff t a a |> shouldEqual true

        Check.QuickThrowOnFailure property

        let property (a : int) (b : int) =
            Cutoff.shouldCutoff t a b |> shouldEqual (a = b)

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``test always`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        I.SetCutoff (I.Var.Watch x) Cutoff.always
        let mutable r = 0

        let o =
            I.Var.Watch x
            |> I.Map (fun _ -> Interlocked.Increment &r |> ignore<int>)
            |> I.Observe

        fix.Stabilize ()

        r |> shouldEqual 1

        for v, expect in [ 1, 1 ; 0, 1 ] do
            I.Var.Set x v
            fix.Stabilize ()
            r |> shouldEqual expect

        Observer.disallowFutureUse o

    [<Test>]
    let ``test never`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        I.SetCutoff (I.Var.Watch x) Cutoff.never
        let mutable r = 0

        let o =
            I.Var.Watch x
            |> I.Map (fun _ -> Interlocked.Increment &r |> ignore<int>)
            |> I.Observe

        fix.Stabilize ()

        r |> shouldEqual 1

        for v, expect in [ 1, 2 ; 1, 3 ; 1, 4 ] do
            I.Var.Set x v
            fix.Stabilize ()
            r |> shouldEqual expect

        Observer.disallowFutureUse o

    [<Test>]
    let ``test physEqual`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let r1 = ref ()
        let r2 = ref ()

        let x = I.Var.Create r1
        I.SetCutoff (I.Var.Watch x) Cutoff.physEqual

        let mutable r = 0

        let o =
            I.Var.Watch x
            |> I.Map (fun _ -> Interlocked.Increment &r |> ignore<int>)
            |> I.Observe

        fix.Stabilize ()

        r |> shouldEqual 1

        for v, expect in [ r1, 1 ; r2, 2 ; r2, 2 ; r1, 3 ] do
            I.Var.Set x v
            fix.Stabilize ()
            r |> shouldEqual expect

        Observer.disallowFutureUse o

    [<Test>]
    let ``test polyEqual`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let r1a = ref 1
        let r1b = ref 1
        let r2 = ref 2

        let x = I.Var.Create r1a
        I.SetCutoff (I.Var.Watch x) Cutoff.polyEqual

        let mutable r = 0

        let o =
            I.Var.Watch x
            |> I.Map (fun _ -> Interlocked.Increment &r |> ignore<int>)
            |> I.Observe

        fix.Stabilize ()

        r |> shouldEqual 1

        for v, expect in [ r1a, 1 ; r1b, 1 ; r2, 2 ; r1a, 3 ] do
            I.Var.Set x v
            fix.Stabilize ()
            r |> shouldEqual expect

        Observer.disallowFutureUse o

    [<Test>]
    let ``never is never`` () =
        Cutoff.equal Cutoff.never Cutoff.never<int>

    [<Test>]
    let ``never isn't always`` () =
        Cutoff.equal Cutoff.never Cutoff.always<int>
