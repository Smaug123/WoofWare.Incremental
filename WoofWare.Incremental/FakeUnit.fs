namespace WoofWare.Incremental

[<Struct>]
type internal FakeUnit = private | FakeUnit

[<RequireQualifiedAccess>]
module internal FakeUnit =
    let inline ofUnit () = FakeUnit.FakeUnit
    let inline toUnit FakeUnit.FakeUnit = ()
