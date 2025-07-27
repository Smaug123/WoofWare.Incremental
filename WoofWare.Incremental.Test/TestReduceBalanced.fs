namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open FsCheck
open FsCheck.FSharp

[<TestFixture>]
module TestReduceBalanced =

    [<Test>]
    let ``empty array`` () =
        let I = Incremental.make ()

        let f =
            [||]
            |> I.ReduceBalanced (fun () -> failwith "should not call") (fun () -> failwith "should not call")

        f.IsNone |> shouldEqual true

    [<Test>]
    let ``single value`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let f = [| I.Var.Watch (I.Var.Create 1) |] |> I.ReduceBalanced id (+) |> Option.get

        let o = I.Observe f
        fix.Stabilize ()
        Observer.value o |> shouldEqual 1

    [<Test>]
    let ``non-commutative function`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let list = [ 97..103 ] |> List.map (char >> string >> I.Var.Create)
        let arr = list |> List.map I.Var.Watch |> Array.ofList

        let mutable reduceCalls = 0

        let f =
            arr
            |> I.ReduceBalanced
                id
                (fun x y ->
                    Interlocked.Increment &reduceCalls |> ignore<int>
                    x + y
                )
            |> Option.get

        let o = I.Observe f

        fix.Stabilize ()
        Observer.value o |> shouldEqual "abcdefg"
        reduceCalls |> shouldEqual 6

        I.Var.Set (List.head list) "z"
        fix.Stabilize ()
        Observer.value o |> shouldEqual "zbcdefg"
        reduceCalls |> shouldEqual 9

    [<Test>]
    let ``observability changes`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let observeStabilizeDisallow node =
            let o = I.Observe node
            fix.Stabilize ()
            let v = Observer.value o
            Observer.disallowFutureUse o
            v

        let v = I.Var.Create 0
        let res = observeStabilizeDisallow (I.Var.Watch v)
        res |> shouldEqual 0

        // stabilize a reduce_balanced_exn node with already stabilized children
        let f = [| I.Var.Watch v |] |> I.ReduceBalanced id (+) |> Option.get
        let res = observeStabilizeDisallow f
        res |> shouldEqual 0

        // re-stabilize a reduce_balanced_exn with a stale cache of its stabilized children.
        I.Var.Set v 1
        let res = observeStabilizeDisallow (I.Var.Watch v)
        res |> shouldEqual 1
        let res = observeStabilizeDisallow f
        res |> shouldEqual 1

    [<Test>]
    let ``multiple occurrences of a node in the fold`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 1
        let f = [| I.Var.Watch x ; I.Var.Watch x |] |> I.ReduceBalanced id (+) |> Option.get
        let o = I.Observe f

        let f2 =
            [| I.Var.Watch x ; I.Var.Watch x ; I.Var.Watch x |]
            |> I.ReduceBalanced id (+)
            |> Option.get

        let o2 = I.Observe f2

        fix.Stabilize ()
        Observer.value o |> shouldEqual 2
        Observer.value o2 |> shouldEqual 3

        I.Var.Set x 3
        fix.Stabilize ()
        Observer.value o |> shouldEqual 6
        Observer.value o2 |> shouldEqual 9

        Observer.disallowFutureUse o
        Observer.disallowFutureUse o2
        fix.Stabilize ()
        I.Var.Set x 4
        let o = I.Observe f
        let o2 = I.Observe f2
        fix.Stabilize ()
        Observer.value o |> shouldEqual 8
        Observer.value o2 |> shouldEqual 12

    type TestValue =
        {
            Var : Var<int>
            Update1 : int option
            Update2 : int option
            Update3 : int option
        }

    let gen (m : IArbMap) (I : Incremental) : Gen<TestValue> =
        let intGen = m.ArbFor<int>().Generator
        let intOptGen = m.ArbFor<int option>().Generator

        gen {
            let! var = intGen
            let! update1 = intOptGen
            let! update2 = intOptGen
            let! update3 = intOptGen

            return
                {
                    Var = I.Var.Create var
                    Update1 = update1
                    Update2 = update2
                    Update3 = update3
                }
        }

    let arb (I : Incremental) =
        Arb.fromGen (gen ArbMap.defaults I |> Gen.listOf)

    open System.Numerics

    let ceilLog2 (n : int) : int =
        match n with
        | n when n <= 0 -> invalidArg (nameof n) "Argument must be positive"
        | 1 -> 0
        | n ->
            let un = uint32 n
            let floorLog2 = BitOperations.Log2 un

            if BitOperations.PopCount un = 1 then
                int floorLog2 // n is a power of 2
            else
                int floorLog2 + 1 // n is not a power of 2

    let property (fix : IncrementalFixture) (testValues : TestValue list) =
        let I = fix.I

        let arr =
            testValues
            |> List.map (fun testValue -> I.Var.Watch testValue.Var)
            |> Array.ofList

        let len = Array.length arr
        let mutable reduceCount = 0
        let mutable foldCount = 0
        let mutable updateCount = 0

        let assertExpectedAndReset () =
            if updateCount = 0 then
                foldCount |> shouldEqual 0
                reduceCount |> shouldEqual 0
            else
                foldCount |> shouldEqual len

                if reduceCount > min (len - 1) (ceilLog2 len * updateCount) then
                    failwith "assertion failed"

            foldCount <- 0
            reduceCount <- 0
            updateCount <- 0

        let reduceF =
            arr
            |> I.ReduceBalanced
                id
                (fun a b ->
                    Interlocked.Increment &reduceCount |> ignore<int>
                    a * b
                )
            |> Option.get

        let foldF =
            arr
            |> I.ArrayFold
                1
                (fun a b ->
                    Interlocked.Increment &foldCount |> ignore<int>
                    a * b
                )

        updateCount <- len
        let reduceO = I.Observe reduceF
        let foldO = I.Observe foldF
        fix.Stabilize ()

        Observer.value foldO |> shouldEqual (Observer.value reduceO)
        assertExpectedAndReset ()

        for testValue in testValues do
            testValue.Update1
            |> Option.iter (fun a ->
                I.Var.Set testValue.Var a
                Interlocked.Increment &updateCount |> ignore<int>
            )

        fix.Stabilize ()
        Observer.value foldO |> shouldEqual (Observer.value reduceO)

        assertExpectedAndReset ()

        for testValue in testValues do
            let mutable updated = false

            testValue.Update2
            |> Option.iter (fun a ->
                I.Var.Set testValue.Var a
                updated <- true
            )

            testValue.Update3
            |> Option.iter (fun a ->
                I.Var.Set testValue.Var a
                updated <- true
            )

            if updated then
                Interlocked.Increment &updateCount |> ignore<int>

        fix.Stabilize ()
        Observer.value foldO |> shouldEqual (Observer.value reduceO)
        assertExpectedAndReset ()

    [<Test>]
    let ``general creation and updating`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let config = Config.QuickThrowOnFailure.WithQuietOnSuccess true

        (property fix)
        |> Prop.forAll (arb I)
        |> fun property -> Check.One (config, property)

    let replays =
        [
            11695027613933141854UL, 12778457246669182719UL
            18365685331342435458UL, 15790651599255451313UL
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof replays)>]
    let ``reproductions of past failures`` (seed : uint64, gamma : uint64) =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let config =
            Config.QuickThrowOnFailure.WithQuietOnSuccess(true).WithReplay (seed, gamma)

        (property fix)
        |> Prop.forAll (arb I)
        |> fun property -> Check.One (config, property)
