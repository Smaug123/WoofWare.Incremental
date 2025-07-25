namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestSum =

    [<Test>]
    let ``empty sum`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.Sum<int, _> None 13 (fun _ -> failwith "should not call") (fun _ -> failwith "should not call") [||]
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 13

    [<Test>]
    let ``full recompute`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13.0
        let y = I.Var.Create 15.0

        let mutable numAdds = 0

        let add (a : float) b =
            Interlocked.Increment &numAdds |> ignore<int>
            a + b

        let mutable numSubs = 0

        let sub (a : float) b =
            Interlocked.Increment &numSubs |> ignore<int>
            a - b

        let z = I.Sum (Some 2) 0.0 add sub [| I.Var.Watch x ; I.Var.Watch y |] |> I.Observe

        fix.Stabilize ()
        numAdds |> shouldEqual 2
        numSubs |> shouldEqual 0
        Observer.valueThrowing z |> shouldEqual 28.0

        I.Var.Set x 17.0
        fix.Stabilize ()
        numAdds |> shouldEqual 3
        numSubs |> shouldEqual 1
        Observer.valueThrowing z |> shouldEqual 32.0

        I.Var.Set x 19.0
        fix.Stabilize ()
        // increases 2 for the full recompute
        numAdds |> shouldEqual 5
        numSubs |> shouldEqual 1
        Observer.valueThrowing z |> shouldEqual 34.0

    [<Test>]
    let ``empty optSum`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.OptSum<int, _> None () (fun _ -> failwith "should not call") (fun _ -> failwith "should not call") [||]
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing(o).IsSome |> shouldEqual true

    [<Test>]
    let ``full recompute, opt`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create ValueNone
        let y = I.Var.Create ValueNone
        let t = [| I.Var.Watch x ; I.Var.Watch y |] |> I.OptSum None 0 (+) (-) |> I.Observe

        let check expect =
            fix.Stabilize ()
            Observer.valueThrowing t |> shouldEqual expect

        check ValueNone
        I.Var.Set x (ValueSome 13)
        check ValueNone
        I.Var.Set y (ValueSome 14)
        check (ValueSome 27)
        I.Var.Set y ValueNone
        check ValueNone

    let testSum<'a> (fix : IncrementalFixture) sum (ofInt : int -> 'a) (shouldEqual : 'a -> 'a -> unit) =
        let I = fix.I

        let x = I.Var.Create (ofInt 13)
        let y = I.Var.Create (ofInt 15)
        let z = I.Observe (sum [| I.Var.Watch x ; I.Var.Watch y |])
        fix.Stabilize ()
        shouldEqual (ofInt 28) (Observer.valueThrowing z)

        fix.Stabilize ()
        I.Var.Set x (ofInt 17)
        fix.Stabilize ()
        shouldEqual (ofInt 32) (Observer.valueThrowing z)

        I.Var.Set x (ofInt 19)
        I.Var.Set y (ofInt 21)
        fix.Stabilize ()
        shouldEqual (ofInt 40) (Observer.valueThrowing z)

    [<Test>]
    let ``test ints`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        testSum fix (I.Sum None 0 (+) (-)) id shouldEqual

    [<Test>]
    let ``test floats`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        testSum fix (I.Sum None 0.0 (+) (-)) float<int> shouldEqual

    [<Test>]
    let ``zero of floats`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = I.Observe (I.Sum None 0.0 (+) (-) [||])
        fix.Stabilize ()
        Observer.valueThrowing o |> shouldEqual 0.0
