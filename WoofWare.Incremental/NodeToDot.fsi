namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module NodeToDot =
    /// Returns a seq of lines of a dot file.
    val renderDot : emitBindEdges:bool -> NodeCrate list -> string seq
    val saveDotToFile : emitBindEdges:bool -> string -> NodeCrate list -> unit
