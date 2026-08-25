namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestScope =
    /// Run a bind, capturing the (non-Top) scope that is current inside the bind's rhs.
    let private getBindScope (fix : IncrementalFixture) : Scope =
        let I = fix.I
        let r = ref None
        let x = I.Var.Create 13

        let _o =
            I.Var.Watch x
            |> I.Bind (fun _ ->
                r.Value <- Some I.CurrentScope
                I.Return ()
            )
            |> I.Observe

        fix.Stabilize ()
        r.Value.Value

    [<Test>]
    let ``current is top`` () =
        let I = Incremental.make ()
        I.CurrentScope |> shouldEqual Scope.top

    [<Test>]
    let ``top equals null returns false, does not throw`` () =
        // The .NET equality contract requires x.Equals(null) = false rather than throwing.
        (Scope.top :> obj).Equals (null : obj) |> shouldEqual false

    [<Test>]
    let ``top equals non-Scope returns false, does not throw`` () =
        (Scope.top :> obj).Equals (box "not a scope") |> shouldEqual false

    [<Test>]
    let ``top hash code does not throw and is consistent`` () =
        Scope.top.GetHashCode () |> shouldEqual (Scope.top.GetHashCode ())

    [<Test>]
    let ``bind scope equals null and non-Scope returns false, does not throw`` () =
        let fix = IncrementalFixture.Make ()
        let s = getBindScope fix
        (s :> obj).Equals (null : obj) |> shouldEqual false
        (s :> obj).Equals (box 42) |> shouldEqual false

    [<Test>]
    let ``equal bind scopes have equal hash codes`` () =
        let fix = IncrementalFixture.Make ()
        let s = getBindScope fix
        // Build a distinct Scope object that wraps the same underlying Bind, so it is equal to `s`
        // by reference-equality semantics but is not the same object.
        let sCopy =
            match s with
            | Scope.Bind c -> Scope.Bind c
            | Scope.Top -> failwith "expected a bind scope"

        System.Object.ReferenceEquals (s, sCopy) |> shouldEqual false
        s.Equals sCopy |> shouldEqual true
        // Equal objects must have equal hash codes, and GetHashCode must not throw.
        s.GetHashCode () |> shouldEqual (sCopy.GetHashCode ())

    [<Test>]
    let ``distinct bind scopes are unequal`` () =
        let fix = IncrementalFixture.Make ()
        let s1 = getBindScope fix
        let s2 = getBindScope fix
        s1.Equals s2 |> shouldEqual false

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
