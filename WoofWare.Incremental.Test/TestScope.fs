namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestScope =
    [<Test>]
    let ``current is top`` () =
        let I = Incremental.make ()
        I.CurrentScope |> shouldEqual Scope.top

    [<Test>]
    let ``within scope`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let o = I.WithinScope I.CurrentScope (fun () -> I.Const 13) |> I.Observe
        fix.Stabilize ()

        Observer.value o |> shouldEqual 13

    [<Test>]
    let ``escape a bind`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let s = I.CurrentScope
        let r = ref None
        let x = I.Var.Create 13

        let o =
            I.Var.Watch x
            |> I.Bind (fun i ->
                r.Value <- Some (I.WithinScope s (fun () -> I.Const i))
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let o2 = I.Observe r.Value.Value
        fix.Stabilize ()
        Observer.value o2 |> shouldEqual 13
        I.Var.Set x 14
        fix.Stabilize ()
        Observer.value o2 |> shouldEqual 13
        Observer.disallowFutureUse o
        fix.Stabilize ()
        Observer.value o2 |> shouldEqual 13

    [<Test>]
    let ``return to a bind`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let r = ref None
        let x = I.Var.Create 13

        let o1 =
            I.Var.Watch x
            |> I.Bind (fun _ ->
                r.Value <- Some I.CurrentScope
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let s = r.Value.Value
        let o2 = I.Observe (I.WithinScope s (fun () -> I.Const 13))
        fix.Stabilize ()
        Observer.value o2 |> shouldEqual 13
        I.Var.Set x 14
        Observer.disallowFutureUse o2
        fix.Stabilize ()
        Observer.disallowFutureUse o1

    [<Test>]
    let ``top is top`` () =
        Scope.isTop Scope.top |> shouldEqual true

    [<Test>]
    let ``scope inside bind is not top`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let i = I.Var.Create true

        let o =
            I.Var.Watch i
            |> I.Bind (fun b ->
                Scope.isTop I.CurrentScope |> shouldEqual false
                I.Return b
            )
            |> I.Observe

        fix.Stabilize ()

        Observer.value o |> shouldEqual true

        I.Var.Set i false
        fix.Stabilize ()

        Observer.value o |> shouldEqual false
