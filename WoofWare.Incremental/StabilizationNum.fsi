namespace WoofWare.Incremental

[<Measure>]
type stab

type StabilizationNum = int<stab>

[<RequireQualifiedAccess>]
module StabilizationNum =
    val none : StabilizationNum
    val zero : StabilizationNum
    val isNone : StabilizationNum -> bool
    val isSome : StabilizationNum -> bool
    val add1 : StabilizationNum -> StabilizationNum
    val toInt : StabilizationNum -> int

    val invariant : StabilizationNum -> unit
