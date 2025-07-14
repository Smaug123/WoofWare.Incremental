namespace WoofWare.PlayFetch

type NodeUpdate<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

type OnUpdateHandler<'a>

[<RequireQualifiedAccess>]
module NodeUpdate =
    val create : ('a NodeUpdate -> unit) -> at : StabilizationNum -> 'a OnUpdateHandler
    val run : 'a OnUpdateHandler -> 'a NodeUpdate -> now : StabilizationNum -> unit
