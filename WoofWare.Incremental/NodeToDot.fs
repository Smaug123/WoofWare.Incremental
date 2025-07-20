namespace WoofWare.Incremental

open System.Collections.Generic
open System.IO

[<RequireQualifiedAccess>]
module NodeToDot =
    let forAnalyzer name kind height =
        let label = [ name ; ForAnalyzer.Kind.toString kind ; $"height=%d{height}" ]
        DotUserInfo.dot label Map.empty

    let printNode (name : string) (kind : ForAnalyzer.Kind) (height : int) (userInfo : DotUserInfo option) : string =
        let default' = forAnalyzer name kind height

        let info =
            match userInfo with
            | None -> default'
            | Some user_info -> DotUserInfo.append default' user_info

        sprintf "%s\n" (DotUserInfo.toString "Mrecord" name (DotUserInfo.toDot info))

    let renderDot (emitBindEdges : bool) (write : string -> unit) ts =
        let nodeName id = "n" + NodeId.toString id
        write "digraph G {\n"
        write "  rankdir = BT\n"

        let seen = HashSet<NodeId> ()
        let bindEdges = ResizeArray ()

        ForAnalyzer.traverse
            ts
            (fun id kind _cutoff children bindChildren userInfo _ _ height ->
                let name = nodeName id
                seen.Add id |> ignore<bool>
                printNode name kind height userInfo |> write

                for childId in children do
                    write $"  %s{nodeName childId} -> %s{name}\n"

                for bindChildId in bindChildren do
                    bindEdges.Add (bindChildId, id)
            )

        if emitBindEdges then
            for bindChildId, id in bindEdges do
                if seen.Contains bindChildId then
                    $"  %s{nodeName id} -> %s{nodeName bindChildId} [style=dashed]\n" |> write

        write "}\n"

    let saveDotToFile (emitBindEdges : bool) (filePath : string) ts =
        use f = File.Open (filePath, FileMode.OpenOrCreate)
        use writer = new StreamWriter (f)
        renderDot emitBindEdges writer.Write ts
