namespace WoofWare.Incremental

open System.Threading

type NodeId = private | NodeId of int

[<RequireQualifiedAccess>]
module NodeId =

    let private count = ref 0

    let next () =
        let result = Interlocked.Increment count
        NodeId result

    let invariant (NodeId i) =
        if i < 1 then
            failwith "invariant failure"

    let toString (NodeId n) = $"%i{n}"

    let internal toInt (NodeId i) = i
    let internal ofInt i = NodeId i
