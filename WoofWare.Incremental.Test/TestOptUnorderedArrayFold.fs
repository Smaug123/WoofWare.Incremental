namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestOptUnorderedArrayFold =

    [<Test>]
    let ``can observe fold on empty arr`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            [||]
            |> I.OptUnorderedArrayFold<int, _>
                ()
                (fun _ -> failwith "should not call")
                (fun _ -> failwith "should not call")
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing(o).IsSome |> shouldEqual true

    [<Test>]
    let ``non-empty arr`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create None
        let y = I.Var.Create None

        let t =
            [| I.Var.Watch x ; I.Var.Watch y |]
            |> I.OptUnorderedArrayFold 0 (+) (-)
            |> I.Observe

        let check expect =
            fix.Stabilize ()
            Observer.valueThrowing t |> shouldEqual expect

        check None
        I.Var.Set x (Some 13)
        check None
        I.Var.Set y (Some 14)
        check (Some 27)
        I.Var.Set y None
        check None
