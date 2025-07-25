namespace WoofWare.Incremental.Test

open System.Threading
open NUnit.Framework
open WoofWare.Incremental
open FsUnitTyped

[<TestFixture>]
module TestIf =
    [<Test>]
    let ``test if true`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.If (I.Const true) (I.Const 13) (I.Const 14) |> I.Observe
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13

    [<Test>]
    let ``test if false`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.If (I.Const false) (I.Const 13) (I.Const 14) |> I.Observe
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 14

    [<Test>]
    let ``if, graph changes only`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create true
        let o = I.If (I.Var.Watch x) (I.Const 3) (I.Const 4) |> I.Observe

        let check expect =
            fix.Stabilize ()
            Observer.valueThrowing o |> shouldEqual expect

        check 3
        I.Var.Set x false
        check 4
        I.Var.Set x true
        check 3
        I.Var.Set x false
        check 4

    [<Test>]
    let ``if, another test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let test = I.Var.Create true
        let thenCase = I.Var.Create 1
        let elseCase = I.Var.Create 2

        let mutable numThenRun = 0
        let mutable numElseRun = 0

        let ite =
            I.If
                (I.Var.Watch test)
                (I.Var.Watch thenCase
                 |> I.Map (fun i ->
                     Interlocked.Increment &numThenRun |> ignore<int>
                     i
                 ))
                (I.Var.Watch elseCase
                 |> I.Map (fun i ->
                     Interlocked.Increment &numElseRun |> ignore<int>
                     i
                 ))
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing ite |> shouldEqual 1
        numThenRun |> shouldEqual 1
        numElseRun |> shouldEqual 0

        I.Var.Set test false
        fix.Stabilize ()
        Observer.valueThrowing ite |> shouldEqual 2
        numThenRun |> shouldEqual 1
        numElseRun |> shouldEqual 1

        I.Var.Set test true
        fix.Stabilize ()
        Observer.valueThrowing ite |> shouldEqual 1
        numThenRun |> shouldEqual 1
        numElseRun |> shouldEqual 1

        I.Var.Set thenCase 3
        I.Var.Set elseCase 4
        let ntr = numThenRun
        let ner = numElseRun
        fix.Stabilize ()
        Observer.valueThrowing ite |> shouldEqual 3
        numThenRun |> shouldEqual (ntr + 1)
        numElseRun |> shouldEqual ner

        I.Var.Set test false
        I.Var.Set thenCase 5
        I.Var.Set elseCase 6
        fix.Stabilize ()
        Observer.valueThrowing ite |> shouldEqual 6

    [<Test>]
    let ``invalid unused branch doesn't invalidate the if`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let mutable r = None
        let lhs = I.Var.Create 1

        let o1 =
            I.Var.Watch lhs
            |> I.Bind (fun i ->
                r <- Some (I.Const i)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()

        let elseCase = r.Value
        let test = I.Var.Create false
        let o2 = I.If (makeHigh I (I.Var.Watch test)) (I.Const 13) elseCase |> I.Observe

        fix.Stabilize ()
        I.Var.Set lhs 2
        // invalidate `else`
        I.Var.Set test true

        fix.Stabilize ()
        NodeHelpers.isValid elseCase |> shouldEqual false
        Observer.valueThrowing o2 |> shouldEqual 13

        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``if-then-else with an invalid test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.If (fix.Invalid |> I.Map (fun _ -> true)) (I.Const ()) (I.Const ())
            |> I.Observe

        fix.Stabilize ()

        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``if-then-else created with an invalid branch`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.If (I.Const true) fix.Invalid (I.Const 13) |> I.Observe

        fix.Stabilize ()

        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``if-then-else switching to an invalid branch`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.If (I.Const true) fix.Invalid (I.Const 13) |> I.Observe

        fix.Stabilize ()

        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``if-then-else switching to an invalid branch, 2`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let b = I.Var.Create false
        let o = I.Observe (I.If (I.Var.Watch b) fix.Invalid (I.Const 13))

        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual true

        I.Var.Set b true
        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``if-then-else switching to an invalid branch via a map`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let b = I.Var.Create false

        let o =
            I.Observe (I.If (I.Var.Watch b) (fix.Invalid |> I.Map (fun _ -> 13)) (I.Const 13))

        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual true

        I.Var.Set b true
        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``if-then-else switching to an invalid test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let b = I.Var.Create false

        let o =
            I.If (I.If (I.Var.Watch b) (fix.Invalid |> I.Map (fun _ -> true)) (I.Const true)) (I.Const 13) (I.Const 15)
            |> I.Observe

        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual true

        I.Var.Set b true
        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``changing branches from a node to its ancestor`` () =
        // this causes problems if we leave the node with a broken invariant while adding the ancestor
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let testVar = I.Var.Create false
        let mutable numCalls = 0
        let branchVar = I.Var.Create 13

        let else_ =
            I.Var.Watch branchVar
            |> I.Map (fun i ->
                Interlocked.Increment &numCalls |> ignore<int>
                i + 1
            )

        let then_ = else_ |> I.Map (fun i -> i + 1)

        let o = I.If (I.Var.Watch testVar) then_ else_ |> I.Observe

        fix.Stabilize ()
        numCalls |> shouldEqual 1

        I.Var.Set testVar true
        fix.Stabilize ()
        numCalls |> shouldEqual 1

        Observer.disallowFutureUse o
        fix.Stabilize ()
        I.Var.Set branchVar 14
        fix.Stabilize ()
        numCalls |> shouldEqual 1
