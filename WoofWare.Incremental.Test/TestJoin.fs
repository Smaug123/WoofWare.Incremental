namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture true>]
[<TestFixture false>]
type TestJoin (joinViaBind : bool) =
    let join (I : Incremental) (x : Node<Node<'a>>) : Node<'a> =
        if joinViaBind then I.Bind id x else I.Join x

    [<Test>]
    member _.``join of a constant`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let o = I.Observe (join I (I.Const (I.Const 1)))

        fix.Stabilize ()

        Observer.value o |> shouldEqual 1

    [<Test>]
    member _.``graph changes only`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let a = I.Const 3
        let b = I.Const 4
        let x = I.Var.Create a
        let o = I.Observe (join I (I.Var.Watch x))

        let check expect =
            fix.Stabilize ()
            Observer.value o |> shouldEqual expect

        check 3
        I.Var.Set x b
        check 4
        I.Var.Set x a
        check 3

    [<Test>]
    member _.``join of a var`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v1 = I.Var.Create 1
        let v2 = I.Var.Create 2
        let v3 = I.Var.Create (I.Var.Watch v1)
        let o = I.Observe (join I (I.Var.Watch v3))

        fix.Stabilize ()
        Observer.value o |> shouldEqual 1

        I.Var.Set v1 13
        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

        I.Var.Set v3 (I.Var.Watch v2)
        fix.Stabilize ()
        Observer.value o |> shouldEqual 2

        I.Var.Set v3 (I.Var.Watch v1)
        I.Var.Set v1 14
        fix.Stabilize ()
        Observer.value o |> shouldEqual 14

    [<Test>]
    member _.``an invalid unused RHS doesn't invalidate the join`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create (I.Const 0)
        let lhs = I.Var.Create 1

        let o1 =
            I.Var.Watch lhs
            |> I.Bind (fun i ->
                I.Var.Set x (I.Const i)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        let o2 = Utils.makeHigh I (I.Var.Watch x) |> join I |> I.Observe
        fix.Stabilize ()
        I.Var.Set lhs 2
        // invalidate
        I.Var.Set x (I.Const 3)
        fix.Stabilize ()
        Observer.value o2 |> shouldEqual 3
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    member _.``join can be invalidated`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let join = join I (I.Const fix.Invalid)
        let o = I.Observe join
        fix.Stabilize ()
        Observer.disallowFutureUse o
        NodeHelpers.isValid join |> shouldEqual false

    [<Test>]
    member _.``Change RHS from a node to its ancestor`` () =
        // This causes problems if we leave the node with a broken invariant while adding the ancestor.

        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let mutable numCalls = 0
        let rhsVar = I.Var.Create 13

        let first =
            I.Var.Watch rhsVar
            |> I.Map (fun i ->
                Interlocked.Increment &numCalls |> ignore<int>
                i + 1
            )

        let second = first |> I.Map (fun i -> i + 1)

        let lhsVar = I.Var.Create first
        let o = I.Observe (join I (I.Var.Watch lhsVar))

        fix.Stabilize ()
        numCalls |> shouldEqual 1

        I.Var.Set lhsVar second
        fix.Stabilize ()
        numCalls |> shouldEqual 1

        Observer.disallowFutureUse o
        fix.Stabilize ()

        I.Var.Set rhsVar 14
        fix.Stabilize ()
        numCalls |> shouldEqual 1
