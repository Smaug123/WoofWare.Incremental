namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module NodeToDot =
    /// Render the nodes as a dot file, calling `write` for each line in that file.
    /// Set stableNodeIds to true if you want deterministic output.
    val renderDot : stableNodeIds : bool -> emitBindEdges : bool -> write : (string -> unit) -> NodeCrate list -> unit
    val saveDotToFile : emitBindEdges : bool -> filePath : string -> NodeCrate list -> unit
