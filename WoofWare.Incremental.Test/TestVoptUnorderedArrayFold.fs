namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestVoptUnorderedArrayFold =

    [<Test>]
    let ``can observe fold on empty arr`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let o =
            [||]
            |> I.VoptUnorderedArrayFold<int, _>
                ()
                (fun _ -> failwith "should not call")
                (fun _ -> failwith "should not call")
            |> I.Observe

        fix.Stabilize ()
        Observer.value(o).IsSome |> shouldEqual true

    [<Test>]
    let ``non-empty arr`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create ValueNone
        let y = I.Var.Create ValueNone

        let t =
            [| I.Var.Watch x ; I.Var.Watch y |]
            |> I.VoptUnorderedArrayFold 0 (+) (-)
            |> I.Observe

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
