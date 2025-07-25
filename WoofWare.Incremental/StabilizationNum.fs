namespace WoofWare.Incremental

[<Measure>]
type stab

type StabilizationNum = int<stab>

[<RequireQualifiedAccess>]
module StabilizationNum =

    let none = -1<stab>
    let zero = 0<stab>
    let isNone i = i = -1<stab>
    let isSome i = i >= 0<stab>
    let add1 i = i + 1<stab>
    let toInt (i : StabilizationNum) : int = int<int<stab>> i

    let invariant i =
        if i < -1<stab> then
            failwith "invariant failed"
