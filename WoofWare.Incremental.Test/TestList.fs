namespace WoofWare.PlayFetch.Test

open FsCheck
open NUnit.Framework
open WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module TestList =
    [<Test>]
    let ``isSortedBy iff sorted`` () =
        let property (l1 : byte list) =
            let actual = List.isSortedBy id l1
            let expected = List.sort l1 = l1
            actual = expected

        Check.QuickThrowOnFailure property
