namespace WoofWare.Incremental

open System

[<CustomComparison ; CustomEquality>]
[<Sealed>]
type StabilizationNum =
    interface IComparable
    interface IComparable<StabilizationNum>

[<RequireQualifiedAccess>]
module StabilizationNum =
    val none : StabilizationNum
    val zero : StabilizationNum
    val isNone : StabilizationNum -> bool
    val isSome : StabilizationNum -> bool
    val add1 : StabilizationNum -> StabilizationNum
    val toInt : StabilizationNum -> int

    val invariant : StabilizationNum -> unit
