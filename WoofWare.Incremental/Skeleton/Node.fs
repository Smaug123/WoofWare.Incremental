namespace WoofWare.Incremental.Skeleton

open System.Collections.Generic
open WoofWare.Incremental

/// Snapshot of a [Incr.Node]'s data/metadata for use in analysis/visualization of the
/// overall graph
type Node =
    {
        Id : NodeId
        Kind : ForAnalyzer.Kind
        Children : NodeId IReadOnlyList
        BindChildren : NodeId IReadOnlyList
        UserInfo : DotUserInfo option
        RecomputedAt : StabilizationNum
        Cutoff : ForAnalyzer.Cutoff
        ChangedAt : StabilizationNum
        Height : int
    }
