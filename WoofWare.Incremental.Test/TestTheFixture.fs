namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open FsUnitTyped

[<TestFixture>]
module TestTheFixture =
    [<Test>]
    let ``invalid is invalid`` () =
        let fix = IncrementalFixture.Make ()
        isInvalid fix fix.Invalid |> shouldEqual true

        fix.Invalid |> NodeHelpers.isValid |> shouldEqual false

    [<Test>]
    let ``Test isInvalidatedOnBindRhs`` () =
        let fix = IncrementalFixture.Make ()
        isInvalidatedOnBindRhs fix (fun _ -> fix.I.Const 13) |> shouldEqual true
