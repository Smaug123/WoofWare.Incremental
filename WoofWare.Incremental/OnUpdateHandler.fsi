namespace WoofWare.Incremental

type NodeUpdate<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

type OnUpdateHandler<'a>

[<RequireQualifiedAccess>]
module OnUpdateHandler =
    val create : ('a NodeUpdate -> unit) -> at : StabilizationNum -> 'a OnUpdateHandler
    val run : 'a OnUpdateHandler -> 'a NodeUpdate -> now : StabilizationNum -> unit
