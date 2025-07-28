namespace WoofWare.Incremental.Test

open System.Threading
open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestUnorderedArrayFold =
    [<Test>]
    let ``empty array`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            [||]
            |> I.UnorderedArrayFold'
                0
                13
                (fun _ () -> failwith "should not call")
                (FoldUpdate.FInverse (fun _ -> failwith "should not call"))
            |> I.Observe

        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

    [<Test>]
    let ``unnecessary unorderedArrayFold isn't computed`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 1
        let mutable numFInverse = 0
        let ox = I.Var.Watch x |> I.Observe

        let fold =
            [| I.Var.Watch x |]
            |> I.UnorderedArrayFold
                0
                (+)
                (FoldUpdate.FInverse (fun b a ->
                    Interlocked.Increment &numFInverse |> ignore<int>
                    b - a
                ))

        let r = I.Observe fold

        fix.Stabilize ()
        Observer.value r |> shouldEqual 1
        numFInverse |> shouldEqual 0

        I.Var.Set x 2
        fix.Stabilize ()
        Observer.value r |> shouldEqual 2
        numFInverse |> shouldEqual 1

        Observer.disallowFutureUse r
        I.Var.Set x 3
        fix.Stabilize ()
        Observer.value ox |> shouldEqual 3
        numFInverse |> shouldEqual 1

        let r = I.Observe fold
        fix.Stabilize ()
        Observer.value r |> shouldEqual 3
        numFInverse |> shouldEqual 1

    [<Test>]
    let ``multiple occurrences of a node in the fold`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 1

        let f =
            [| I.Var.Watch x ; I.Var.Watch x |]
            |> I.UnorderedArrayFold 0 (+) (FoldUpdate.FInverse (-))

        let o = I.Observe f

        fix.Stabilize ()
        Observer.value o |> shouldEqual 2

        I.Var.Set x 3
        fix.Stabilize ()
        Observer.value o |> shouldEqual 6

        Observer.disallowFutureUse o
        fix.Stabilize ()
        I.Var.Set x 4
        fix.Stabilize ()
        let o = I.Observe f
        fix.Stabilize ()
        Observer.value o |> shouldEqual 8

    [<Test>]
    let ``update is an Update`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 1

        let fold =
            [| I.Var.Watch x |]
            |> I.UnorderedArrayFold 0 (+) (FoldUpdate.Update (fun acc old newV -> acc - old + newV))

        let r = I.Observe fold

        let check v =
            fix.Stabilize ()
            Observer.value r |> shouldEqual v

        check 1
        I.Var.Set x 3
        check 3
