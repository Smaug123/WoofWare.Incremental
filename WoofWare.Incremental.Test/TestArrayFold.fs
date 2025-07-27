namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open FsUnitTyped

[<TestFixture>]
module TestArrayFold =
    [<Test>]
    let ``empty array`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o = [||] |> I.ArrayFold 13 (fun _ () -> failwith "should not call") |> I.Observe

        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

    [<Test>]
    let ``another test`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let y = I.Var.Create 14

        let o =
            [| I.Var.Watch y ; I.Var.Watch x |]
            |> I.ArrayFold [] (fun ac x -> x :: ac)
            |> I.Observe

        let check expect =
            fix.Stabilize ()
            Observer.value o |> shouldEqual expect

        check [ 13 ; 14 ]
        I.Var.Set x 15
        check [ 15 ; 14 ]
        I.Var.Set y 16
        check [ 15 ; 16 ]
        I.Var.Set x 17
        I.Var.Set y 18
        check [ 17 ; 18 ]
