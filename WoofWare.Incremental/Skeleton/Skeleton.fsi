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

module Skeleton =
    val snapshot : normalize:bool option -> _ State -> Skeleton

    val toDot
      :  extraAttrs:(Node -> DotUserInfo option) option
      -> renderTarget:RenderTarget option
      -> filteredNodes:Node list option
      -> ?renderRelation:RenderRelation
      -> Skeleton
      -> string

