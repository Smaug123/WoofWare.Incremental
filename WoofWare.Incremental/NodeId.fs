namespace WoofWare.PlayFetch

open System.Threading

type NodeId = private | NodeId of int

[<RequireQualifiedAccess>]
module NodeId =

    let private count = ref 0

    let next () =
        let result = Interlocked.Increment count
        NodeId result
