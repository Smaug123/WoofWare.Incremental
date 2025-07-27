namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open FsUnitTyped

[<TestFixture>]
module TestMap =

    let testMap (fix : IncrementalFixture) (n : int) (mapN : Node<int> -> Node<int>) =
        let I = fix.I
        let o = I.Observe (mapN (I.Const 1))
        fix.Stabilize ()
        Observer.value o |> shouldEqual n

        let x = I.Var.Create 1
        let o = I.Observe (mapN (I.Var.Watch x))
        fix.Stabilize ()
        Observer.value o |> shouldEqual n

        I.Var.Set x 0
        fix.Stabilize ()
        Observer.value o |> shouldEqual 0

        I.Var.Set x 2
        fix.Stabilize ()
        Observer.value o |> shouldEqual (2 * n)

        isInvalid fix (mapN fix.Invalid) |> shouldEqual true
        isInvalidatedOnBindRhs fix (fun i -> mapN (I.Const i))

    [<Test>]
    let ``a couple of maps`` () =
        do
            let fix = IncrementalFixture.Make ()
            testMap fix 1 (fun i -> i |> fix.I.Map (fun a -> a))

        do
            let fix = IncrementalFixture.Make ()
            testMap fix 2 (fun i -> fix.I.Map2 (fun a b -> a + b) i i)

    [<Test>]
    let ``something `` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x0 = I.Var.Create 13
        let o0 = I.Observe (I.Var.Watch x0)
        let t1 = I.Var.Watch x0 |> I.Map (fun x -> x + 1)
        let t1o = I.Observe t1

        fix.Stabilize ()
        Observer.value t1o |> shouldEqual (Observer.value o0 + 1)

        I.Var.Set x0 14
        fix.Stabilize ()
        Observer.value t1o |> shouldEqual (Observer.value o0 + 1)

        let x1 = I.Var.Create 15
        let o1 = I.Observe (I.Var.Watch x1)
        let t2 = (I.Var.Watch x0, I.Var.Watch x1) ||> I.Map2 (+)
        let t2o = I.Observe t2
        let t3 = (t1, t2) ||> I.Map2 (fun x y -> x - y)
        let t3o = I.Observe t3

        let check () =
            fix.Stabilize ()
            Observer.value t1o |> shouldEqual (Observer.value o0 + 1)

            Observer.value t2o |> shouldEqual (Observer.value o0 + Observer.value o1)

            Observer.value t3o |> shouldEqual (Observer.value t1o - Observer.value t2o)

        check ()
        I.Var.Set x0 16
        check ()
        I.Var.Set x1 17
        check ()
        I.Var.Set x0 18
        I.Var.Set x1 19
        check ()

    [<Test>]
    let ``deep graph`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let rec loop i t =
            if i = 0 then t else loop (i - 1) (I.Map (fun x -> x + 1) t)

        let x0 = I.Var.Create 0
        let n = 100
        let o = I.Observe (loop n (I.Var.Watch x0))

        fix.Stabilize ()
        Observer.value o |> shouldEqual n

        I.Var.Set x0 1
        fix.Stabilize ()
        Observer.value o |> shouldEqual (n + 1)

        Observer.disallowFutureUse o
        fix.Stabilize ()
