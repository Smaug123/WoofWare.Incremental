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
