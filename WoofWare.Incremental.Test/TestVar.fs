namespace WoofWare.Incremental.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental

[<TestFixture>]
module TestVar =

    [<Test>]
    let ``observe a var after stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        fix.Stabilize ()
        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()

        Observer.valueThrowing o |> shouldEqual 0

    [<Test>]
    let ``observe a set var after stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        I.Var.Set x 1
        fix.Stabilize ()
        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 1

    [<Test>]
    let ``observe a replace var after stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        I.Var.Replace x ((+) 1)
        fix.Stabilize ()

        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()

        Observer.valueThrowing o |> shouldEqual 1

    [<Test>]
    let ``observing and setting var after stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0
        fix.Stabilize ()
        let o = I.Observe (I.Var.Watch x)
        I.Var.Set x 1
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 1

    [<Test>]
    let ``set without stabilizing`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        I.Var.Value x |> shouldEqual 13
        I.Var.LatestValue x |> shouldEqual 13

        let o = I.Observe (I.Var.Watch x)
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13
        I.Var.Set x 14

        I.Var.Value x |> shouldEqual 14
        I.Var.LatestValue x |> shouldEqual 14
        Observer.valueThrowing o |> shouldEqual 13

    [<Test>]
    let ``set during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let v0 = I.Var.Create 0
        let v1 = I.Var.Create 1
        let v2 = I.Var.Create 2
        let o0 = I.Observe (I.Var.Watch v0)

        let o1 =
            I.Var.Watch v1
            |> I.Map (fun i ->
                let i0 = I.Var.Value v0
                I.Var.Set v0 i
                I.Var.Value v0 |> shouldEqual i0
                I.Var.LatestValue v0 |> shouldEqual i
                let i2 = I.Var.Value v2
                I.Var.Set v2 i
                I.Var.Value v2 |> shouldEqual i2
                I.Var.LatestValue v2 |> shouldEqual i
                i
            )
            |> I.Observe

        let o2 = I.Var.Watch v2 |> I.Observe

        let varVals i0 i1 i2 =
            I.Var.Value v0 |> shouldEqual i0
            I.Var.Value v1 |> shouldEqual i1
            I.Var.Value v2 |> shouldEqual i2

        let obsVals i0 i1 i2 =
            Observer.valueThrowing o0 |> shouldEqual i0
            Observer.valueThrowing o1 |> shouldEqual i1
            Observer.valueThrowing o2 |> shouldEqual i2

        varVals 0 1 2
        fix.Stabilize ()
        obsVals 0 1 2
        varVals 1 1 1
        fix.Stabilize ()
        obsVals 1 1 1
        varVals 1 1 1
        I.Var.Set v1 13
        obsVals 1 1 1
        varVals 1 13 1
        fix.Stabilize ()
        obsVals 1 13 1
        varVals 13 13 13
        fix.Stabilize ()
        obsVals 13 13 13
        varVals 13 13 13

    [<Test>]
    let ``set during stabilization gets last value that was set`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0

        let o =
            I.Var.Watch x
            |> I.Map (fun v ->
                I.Var.Set x 1
                I.Var.Set x 2
                I.Var.LatestValue x |> shouldEqual 2
                v
            )
            |> I.Observe

        fix.Stabilize ()
        I.Var.Value x |> shouldEqual 2
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 2
        Observer.disallowFutureUse o

    [<Test>]
    let ``replace during stabilization gets latest value`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0

        let o =
            I.Var.Watch x
            |> I.Map (fun v ->
                I.Var.Set x 2

                I.Var.Replace
                    x
                    (fun v ->
                        v |> shouldEqual 2
                        v + 1
                    )

                v
            )
            |> I.Observe

        fix.Stabilize ()

        I.Var.Value x |> shouldEqual 3
        Observer.valueThrowing o |> shouldEqual 0

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 3

        Observer.disallowFutureUse o

    [<Test>]
    let ``create during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.Const 13
            |> I.Bind (fun i ->
                let v = I.Var.Create i
                I.Var.Watch v
            )
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13

    [<Test>]
    let ``create and set during stabilization`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.Const 13
            |> I.Bind (fun i ->
                let v = I.Var.Create i
                let t = I.Var.Watch v
                I.Var.Set v 15
                t
            )
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 15

    [<TestCase false>]
    [<TestCase true>]
    let ``maybe invalidating a variable`` (useCurrentScope : bool) =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let lhs = I.Var.Create 0
        let rhs = ref (I.Const 0)

        let o =
            I.Var.Watch lhs
            |> I.Bind (fun i ->
                rhs.Value <- I.Var.Watch (I.Var.Create' useCurrentScope i)
                rhs.Value
            )
            |> I.Observe

        fix.Stabilize ()
        let rhs = rhs.Value
        NodeHelpers.isValid rhs |> shouldEqual true
        I.Var.Set lhs 1
        fix.Stabilize ()

        not (NodeHelpers.isValid rhs) |> shouldEqual useCurrentScope
        Observer.valueThrowing o |> shouldEqual 1
