namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<State>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test ; Explicit "not yet documented">]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly

    [<Test ; Explicit "Not yet released to NuGet">]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let ``EnsureVersionIsMonotonic`` () =
        MonotonicVersion.validate assembly "WoofWare.Incremental"
