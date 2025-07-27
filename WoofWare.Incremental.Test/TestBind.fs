namespace WoofWare.Incremental.Test

open System.Threading
open WoofWare.Incremental
open NUnit.Framework
open FsUnitTyped
open System

[<TestFixture>]
module TestBind =
    [<Test>]
    let ``bind of a constant`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        fix.Stabilize ()
        let o = I.Observe (I.Const 13 |> I.Bind I.Const)
        fix.Stabilize ()

        Observer.value o |> shouldEqual 13

    [<Test>]
    let ``bind of a constant is invalidated`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        isInvalidatedOnBindRhs fix (fun i -> I.Bind (fun _ -> I.Const i) (I.Const i))

    [<Test>]
    let ``bind created with invalid RHS`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let o = I.Observe (I.Const () |> I.Bind (fun () -> fix.Invalid))
        fix.Stabilize ()
        NodeHelpers.isValid (Observer.observing o) |> shouldEqual false

    [<Test>]
    let ``bind created with an RHS that becomes invalid`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let b = I.Var.Create true

        let o =
            I.Var.Watch b
            |> I.Bind (fun b -> if b then I.Const 13 else fix.Invalid)
            |> I.Observe

        fix.Stabilize ()

        I.Var.Set b false
        Observer.observing o |> NodeHelpers.isValid |> shouldEqual true

        fix.Stabilize ()
        Observer.observing o |> NodeHelpers.isValid |> shouldEqual false

    [<Test>]
    let ``an invalid node created on RHS of valid bind, later invalidated`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let mutable r = None

        let o1 =
            I.Var.Watch x
            |> I.Bind (fun _ ->
                r <- Some (I.Map id fix.Invalid)
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()

        let o2 = r.Value |> I.Observe

        fix.Stabilize ()
        Observer.observing o2 |> NodeHelpers.isValid |> shouldEqual false

        I.Var.Set x 14
        fix.Stabilize ()
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``LHS becomes necessary and only then the right`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        // invariants blow up here if we don't make sure that we first make the
        // lhs-change node of binds necessary and only then the rhs necessary.

        let node1 = I.Const () |> I.Bind I.Return
        let o = I.Observe node1
        fix.Stabilize ()
        Observer.disallowFutureUse o
        fix.Stabilize ()
        let o = I.Observe node1
        fix.Stabilize ()
        Observer.disallowFutureUse o

    [<Test ; Explicit "not passing yet">]
    let ``more binds`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v1 = I.Var.Create 0
        let i1 = I.Var.Watch v1
        let i4 = i1 |> I.Bind (fun x1 -> i1 |> I.Bind (fun x2 -> I.Const (x1 + x2)))
        let o4 = I.Observe i4

        fix.Stabilize ()

        for x = 0 to 0 do
            Gc.collect ()
            I.Var.Set v1 x
            fix.Stabilize ()
            Observer.value o4 |> shouldEqual ((2 * x) + 3)

    [<Test>]
    let ``graph changes only`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create true
        let a = I.Const 3
        let b = I.Const 4
        let o = I.Observe (I.Bind (fun cond -> if cond then a else b) (I.Var.Watch x))

        let check expect =
            fix.Stabilize ()
            Observer.value o |> shouldEqual expect

        check 3
        I.Var.Set x false
        check 4
        I.Var.Set x true
        check 3

    [<Test>]
    let ``more tests`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x0 = I.Var.Create 13
        let o0 = I.Observe (I.Var.Watch x0)
        let x1 = I.Var.Create 15
        let o1 = I.Observe (I.Var.Watch x1)
        let x2 = I.Var.Create true
        let o2 = I.Observe (I.Var.Watch x2)

        let t =
            I.Var.Watch x2 |> I.Bind (fun b -> if b then I.Var.Watch x0 else I.Var.Watch x1)

        let tO = I.Observe t

        let check () =
            fix.Stabilize ()

            Observer.value tO
            |> shouldEqual (Observer.value (if Observer.value o2 then o0 else o1))

        check ()
        I.Var.Set x0 17
        check ()
        I.Var.Set x1 19
        check ()
        I.Var.Set x2 false
        check ()
        I.Var.Set x0 21
        I.Var.Set x2 true
        check ()

    [<Test>]
    let ``another test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let v1 = I.Var.Create 0
        let i1 = I.Var.Watch v1
        let o1 = I.Observe i1
        I.Var.Set v1 1
        let i2 = i1 |> I.Bind (fun _ -> i1)
        let o2 = I.Observe i2

        fix.Stabilize ()
        I.Var.Set v1 2
        fix.Stabilize ()

        GC.KeepAlive i1
        GC.KeepAlive i2
        GC.KeepAlive o1
        GC.KeepAlive o2

    [<Test>]
    let ``topological overload many`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let rec copyTrue c1 =
            c1 |> I.Bind (fun x -> if x then c1 else copyFalse c1)

        and copyFalse c1 =
            c1 |> I.Bind (fun x -> if x then copyTrue c1 else c1)

        let x1 = I.Var.Create false

        let rec loop cur i =
            if i > 1000 then
                cur
            else
                loop (copyTrue (copyFalse cur)) (i + 1)

        let hold = loop (I.Var.Watch x1) 0

        let rec setLoop at i =
            if i < 5 then
                I.Var.Set x1 at
                fix.Stabilize ()
                setLoop (not at) (i + 1)

        setLoop true 0

        GC.KeepAlive hold

    [<Test>]
    let ``invalid unused RHS doesn't invalidate the bind`` () =
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
        let else_ = r.Value
        let test = I.Var.Create false

        let o2 =
            makeHigh I (I.Var.Watch test)
            |> I.Bind (fun test -> if test then I.Const 13 else else_)
            |> I.Observe

        fix.Stabilize ()

        I.Var.Set lhs 2
        // invalidate `else_`
        I.Var.Set test true
        fix.Stabilize ()
        NodeHelpers.isValid else_ |> shouldEqual false
        Observer.value o2 |> shouldEqual 13
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2

    [<Test>]
    let ``plugging an invalid node in a bind can invalidate the bind, though not always`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 4
        let r = ref (I.Const -1)

        let o =
            I.Var.Watch x
            |> I.Bind (fun i ->
                r.Value <- I.Const i
                I.Const ()
            )
            |> I.Observe

        fix.Stabilize ()
        let escaped = r.Value
        let escapedO = I.Observe escaped
        fix.Stabilize ()
        Observer.value escapedO |> shouldEqual 4

        I.Var.Set x 5
        fix.Stabilize ()
        NodeHelpers.isValid escaped |> shouldEqual false

        Observer.disallowFutureUse o
        let o = I.Var.Watch x |> I.Bind (fun _ -> escaped) |> I.Observe
        fix.Stabilize ()
        Observer.disallowFutureUse o
        Observer.disallowFutureUse escapedO

    [<Test>]
    let ``changing rhs from a node to its ancestor`` () =
        // this causes problems if we leave the node with a broken invariant while adding the ancestor
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let lhsVar = I.Var.Create false
        let mutable numCalls = 0
        let rhsVar = I.Var.Create 13

        let rhsFalse =
            I.Var.Watch rhsVar
            |> I.Map (fun i ->
                Interlocked.Increment &numCalls |> ignore<int>
                i + 1
            )

        let rhsTrue = rhsFalse |> I.Map (fun i -> i + 1)

        let o =
            I.Var.Watch lhsVar
            |> I.Bind (fun b -> if b then rhsTrue else rhsFalse)
            |> I.Observe

        fix.Stabilize ()
        numCalls |> shouldEqual 1
        I.Var.Set lhsVar true
        fix.Stabilize ()
        numCalls |> shouldEqual 1

        Observer.disallowFutureUse o
        fix.Stabilize ()
        I.Var.Set rhsVar 14
        fix.Stabilize ()
        numCalls |> shouldEqual 1
