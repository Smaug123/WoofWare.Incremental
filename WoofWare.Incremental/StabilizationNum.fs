namespace WoofWare.PlayFetch

open System

[<CustomComparison>]
[<CustomEquality>]
type StabilizationNum =
    | StabilizationNum of int

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? StabilizationNum as other -> (this :> IComparable<StabilizationNum>).CompareTo other
            | _ -> failwith "oh no"

    interface IComparable<StabilizationNum> with
        member this.CompareTo (StabilizationNum other) =
            match this with
            | StabilizationNum this -> this.CompareTo other

    override this.Equals (other : obj) : bool =
        match other with
        | :? StabilizationNum as other ->
            match this, other with
            | StabilizationNum this, StabilizationNum other -> this = other
        | _ -> failwith "oh no"

    override this.GetHashCode () : int =
        match this with
        | StabilizationNum this -> this.GetHashCode ()

[<RequireQualifiedAccess>]
module StabilizationNum =

    let none = StabilizationNum -1
    let zero = StabilizationNum 0
    let isNone (StabilizationNum i) = i = -1
    let isSome (StabilizationNum i) = i >= 0
    let add1 (StabilizationNum i) = i + 1 |> StabilizationNum
    let toInt (StabilizationNum i) = i
