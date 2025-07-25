namespace WoofWare.Incremental.Skeleton

open System.Collections.Generic
open WoofWare.Incremental

/// Structure for extracting desired information out of an [Incr] graph, to be used for
/// further analysis/filtering/visualization
type Skeleton =
    {
        Nodes : Node IReadOnlyList
        Seen : NodeId IReadOnlySet
        NumStabilizes : int
    }

type RenderTarget =
    | Dot
    | GraphEasy

[<RequireQualifiedAccess>]
module Skeleton =
    /// Creates a static snapshot of the current incremental graph.
    /// If [normalize] is true (default false), node IDs will be normalized relative to the
    /// minimum node ID in the graph. This is primarily useful for tests.
    val snapshot : normalize : bool option -> State -> Skeleton

    /// Converts a Skeleton to a dot string that can be rendered by graphviz.
    val toDot :
        extraAttrs : (Node -> DotUserInfo option) option ->
        renderTarget : RenderTarget option ->
        filteredNodes : Node list option ->
        ?renderRelation : RenderRelation option ->
        Skeleton ->
            string
