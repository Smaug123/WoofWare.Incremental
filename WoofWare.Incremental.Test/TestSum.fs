namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open FsCheck
open FsCheck.FSharp

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
        Observer.value o |> shouldEqual 13

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
        Observer.value z |> shouldEqual 28.0

        I.Var.Set x 17.0
        fix.Stabilize ()
        numAdds |> shouldEqual 3
        numSubs |> shouldEqual 1
        Observer.value z |> shouldEqual 32.0

        I.Var.Set x 19.0
        fix.Stabilize ()
        // increases 2 for the full recompute
        numAdds |> shouldEqual 5
        numSubs |> shouldEqual 1
        Observer.value z |> shouldEqual 34.0

    [<Test>]
    let ``empty optSum`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            I.OptSum<int, _> None () (fun _ -> failwith "should not call") (fun _ -> failwith "should not call") [||]
            |> I.Observe

        fix.Stabilize ()
        Observer.value(o).IsSome |> shouldEqual true

    [<Test>]
    let ``full recompute, opt`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create ValueNone
        let y = I.Var.Create ValueNone
        let t = [| I.Var.Watch x ; I.Var.Watch y |] |> I.OptSum None 0 (+) (-) |> I.Observe

        let check expect =
            fix.Stabilize ()
            Observer.value t |> shouldEqual expect

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
        shouldEqual (ofInt 28) (Observer.value z)

        fix.Stabilize ()
        I.Var.Set x (ofInt 17)
        fix.Stabilize ()
        shouldEqual (ofInt 32) (Observer.value z)

        I.Var.Set x (ofInt 19)
        I.Var.Set y (ofInt 21)
        fix.Stabilize ()
        shouldEqual (ofInt 40) (Observer.value z)

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
        Observer.value o |> shouldEqual 0.0

    [<Test>]
    let ``sum' does a periodic full recompute to bound float drift`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        // Choose values that make incremental add/sub drift: 1.0 is lost when added to 1e16
        // (the ULP of 1e16 is 2), so (a + b) - b = 0.0 <> 1.0 = a.
        let a = I.Var.Create 1.0
        let b = I.Var.Create 1e16

        let o = State.sumFloat I.State [| I.Var.Watch a ; I.Var.Watch b |] |> I.Observe

        fix.Stabilize ()
        // Initial full compute: (0 + 1.0) + 1e16 = 1e16.
        Observer.value o |> shouldEqual 1e16

        // Change #1 is always incremental: (1e16 - 1e16) + 0.0 = 0.0. The true sum is now 1.0.
        I.Var.Set b 0.0
        fix.Stabilize ()
        Observer.value o |> shouldEqual 0.0

        // Change #2 is the nodes.Length-th change. With fullComputeEveryNChanges = nodes.Length
        // it forces a full recompute, giving the exact answer (0 + 2.0 + 0.0 = 2.0). Without it,
        // the incremental update gives (0.0 - 1.0) + 2.0 = 1.0, i.e. accumulated drift of 1.0.
        I.Var.Set a 2.0
        fix.Stabilize ()
        Observer.value o |> shouldEqual 2.0

    [<Test>]
    let ``sum' agrees with an explicit full_compute_every_n_changes of nodes.Length`` () =
        // A finite, varied-magnitude float so that incremental drift actually appears.
        let genValue =
            gen {
                let! sign = Gen.elements [ 1.0 ; -1.0 ]
                let! mant = Gen.choose (0, 1000)
                let! scale = Gen.elements [ 1.0 ; 1e8 ; 1e16 ]
                return sign * float mant * scale
            }

        let genScenario =
            gen {
                let! n = Gen.choose (2, 5)
                let! initial = Gen.listOfLength n genValue

                let! updates =
                    gen {
                        let! i = Gen.choose (0, n - 1)
                        let! v = genValue
                        return (i, v)
                    }
                    |> Gen.listOf

                return (initial, updates)
            }

        let property (initial : float list, updates : (int * float) list) =
            let fix = IncrementalFixture.Make ()
            let I = fix.I
            let n = List.length initial
            let vars = initial |> List.map I.Var.Create
            let watches = vars |> List.map I.Var.Watch |> Array.ofList

            // sum' must behave exactly like Sum with full_compute_every_n_changes = nodes.Length.
            let oPrime = State.sumFloat I.State watches |> I.Observe
            let oExplicit = I.Sum (Some n) 0.0 (+) (-) watches |> I.Observe

            fix.Stabilize ()
            let mutable ok = Observer.value oPrime = Observer.value oExplicit

            for i, v in updates do
                I.Var.Set (List.item i vars) v
                fix.Stabilize ()
                ok <- ok && (Observer.value oPrime = Observer.value oExplicit)

            Observer.disallowFutureUse oPrime
            Observer.disallowFutureUse oExplicit
            ok

        let config = Config.QuickThrowOnFailure.WithQuietOnSuccess true
        Check.One (config, Prop.forAll (Arb.fromGen genScenario) property)
