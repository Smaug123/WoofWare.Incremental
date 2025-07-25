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

    override this.ToString () =
        let children = this.Children |> Seq.map NodeId.toString |> String.concat " "
        let bindChildren = this.BindChildren |> Seq.map NodeId.toString |> String.concat " "
        [
            yield "{"
            yield $"  id: %O{this.Id}"
            yield $"  kind: %s{ForAnalyzer.Kind.toString this.Kind}"
            if children <> "" then
                yield $"  children: (%s{children})"
            if bindChildren <> "" then
                yield $"  bindChildren: (%s{bindChildren})"
            yield $"  recomputedAt: (%i{StabilizationNum.toInt this.RecomputedAt})"
            match this.Cutoff with
            | ForAnalyzer.Cutoff.PhysEqual -> ()
            | cutoff -> yield $"  cutoff: (%s{ForAnalyzer.Cutoff.toString cutoff})"
            yield $"  changedAt: (%i{StabilizationNum.toInt this.ChangedAt})"
            yield $"  height: (%i{this.Height})"
            yield "}"
        ]
        |> String.concat "\n"
