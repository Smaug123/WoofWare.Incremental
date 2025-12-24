namespace WoofWare.Incremental

open System.Collections.Generic
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module ForAnalyzer =
    [<RequireQualifiedAccess>]
    type Cutoff =
        | Always
        | Never
        | PhysEqual
        | Compare
        | Equal
        | F

    [<RequireQualifiedAccess>]
    module Cutoff =
        let toString (c : Cutoff) =
            match c with
            | Cutoff.Always -> "Always"
            | Cutoff.Never -> "Never"
            | Cutoff.PhysEqual -> "PhysEqual"
            | Cutoff.Compare -> "Compare"
            | Cutoff.Equal -> "Equal"
            | Cutoff.F -> "F"

        let ofCutoff (c : WoofWare.Incremental.Cutoff<'a>) : Cutoff =
            match c with
            | WoofWare.Incremental.Cutoff.Always -> Cutoff.Always
            | WoofWare.Incremental.Cutoff.Never -> Cutoff.Never
            | WoofWare.Incremental.Cutoff.PhysEqual -> Cutoff.PhysEqual
            | WoofWare.Incremental.Cutoff.Compare _ -> Cutoff.Compare
            | WoofWare.Incremental.Cutoff.Equal _ -> Cutoff.Equal
            | WoofWare.Incremental.Cutoff.F _ -> Cutoff.F

    [<RequireQualifiedAccess>]
    type Kind =
        | ArrayFold
        | At of TimeNs
        | AtIntervals of base' : TimeNs * interval : TimeNs.Span
        | BindLhsChange
        | BindMain
        | Const
        | Expert
        | Freeze
        | IfTestChange
        | IfThenElse
        | Invalid
        | JoinLhsChange
        | JoinMain
        | Map
        | Snapshot of at : TimeNs
        | StepFunction
        | Uninitialized
        | UnorderedArrayFold
        | Var
        | Map2

    [<RequireQualifiedAccess>]
    module Kind =
        let toString (k : Kind) : string =
            match k with
            | Kind.ArrayFold -> "ArrayFold"
            | Kind.At int64 -> $"At(%i{int64})"
            | Kind.AtIntervals (base', interval) -> $"AtIntervals(%i{base'}, %i{interval}"
            | Kind.BindLhsChange -> "BindLhsChange"
            | Kind.BindMain -> "BindMain"
            | Kind.Const -> "Const"
            | Kind.Expert -> "Expert"
            | Kind.Freeze -> "Freeze"
            | Kind.IfTestChange -> "IfTestChange"
            | Kind.IfThenElse -> "IfThenElse"
            | Kind.Invalid -> "Invalid"
            | Kind.JoinLhsChange -> "JoinLhsChange"
            | Kind.JoinMain -> "JoinMain"
            | Kind.Map -> "Map"
            | Kind.Snapshot at -> "Snapshot"
            | Kind.StepFunction -> "StepFunction"
            | Kind.Uninitialized -> "Uninitialized"
            | Kind.UnorderedArrayFold -> "UnorderedArrayFold"
            | Kind.Var -> "Var"
            | Kind.Map2 -> "Map2"

        let ofKind (k : WoofWare.Incremental.Kind<'a>) : Kind =
            match k with
            | WoofWare.Incremental.Kind.ArrayFold _ -> Kind.ArrayFold
            | WoofWare.Incremental.Kind.At (at, _) -> Kind.At at.At
            | WoofWare.Incremental.Kind.AtIntervals (atIntervals, _) ->
                Kind.AtIntervals (atIntervals.Base, atIntervals.Interval)
            | WoofWare.Incremental.Kind.BindLhsChange _ -> Kind.BindLhsChange
            | WoofWare.Incremental.Kind.BindMain bindMainCrate -> Kind.BindMain
            | WoofWare.Incremental.Kind.Const foo -> Kind.Const
            | WoofWare.Incremental.Kind.Expert expert -> Kind.Expert
            | WoofWare.Incremental.Kind.Freeze freeze -> Kind.Freeze
            | WoofWare.Incremental.Kind.IfTestChange (fThenElseCrate, teq) -> Kind.IfTestChange
            | WoofWare.Incremental.Kind.IfThenElse ifThenElse -> Kind.IfThenElse
            | WoofWare.Incremental.Kind.Invalid -> Kind.Invalid
            | WoofWare.Incremental.Kind.JoinLhsChange (joinCrate, teq) -> Kind.JoinLhsChange
            | WoofWare.Incremental.Kind.JoinMain join -> Kind.JoinMain
            | WoofWare.Incremental.Kind.Map mapCrate -> Kind.Map
            | WoofWare.Incremental.Kind.Snapshot snapshot -> Kind.Snapshot snapshot.At
            | WoofWare.Incremental.Kind.StepFunction stepFunctionNode -> Kind.StepFunction
            | WoofWare.Incremental.Kind.Uninitialized -> Kind.Uninitialized
            | WoofWare.Incremental.Kind.UnorderedArrayFold unorderedArrayFoldCrate -> Kind.UnorderedArrayFold
            | WoofWare.Incremental.Kind.Var var -> Kind.Var
            | WoofWare.Incremental.Kind.Map2 map2Crate -> Kind.Map2

    let maybeIterOnBindNodesCreatedOnRhs (node : NodeCrate) f =
        { new NodeEval<_, _> with
            member _.Eval () node =
                match node.Kind with
                | WoofWare.Incremental.Kind.BindLhsChange (bind, _) ->
                    { new BindEval<_> with
                        member _.Eval bind =
                            Bind.iterNodesCreatedOnRhs bind f
                            FakeUnit.ofUnit ()
                    }
                    |> bind.Apply
                | _ -> FakeUnit.ofUnit ()
        }
        |> node.Apply ()
        |> FakeUnit.toUnit

    let private kindEval : NodeEval<_, Kind> =
        { new NodeEval<_, _> with
            member _.Eval () n = n.Kind |> Kind.ofKind
        }

    let private kind (n : NodeCrate) = n.Apply () kindEval

    let private cutoffEval : NodeEval<_, Cutoff> =
        { new NodeEval<_, _> with
            member _.Eval () n = n.Cutoff |> Cutoff.ofCutoff
        }

    let private cutoff (n : NodeCrate) = n.Apply () cutoffEval

    /// Args to the addNode callback:
    /// id, kind, cutoff, children, bindChildren, userInfo, recomputedAt, changedAt, height
    let traverse
        (packedList : NodeCrate list)
        (addNode :
            NodeId
                -> Kind
                -> Cutoff
                -> NodeId IReadOnlyList
                -> NodeId IReadOnlyList
                -> DotUserInfo option
                -> StabilizationNum
                -> StabilizationNum
                -> int
                -> unit)
        : unit
        =
        let mapOfIter iterator f =
            let out = ResizeArray ()
            iterator (fun x -> out.Add (f x))
            out

        NodeCrate.iterDescendants
            packedList
            (fun packedNode ->
                let children =
                    mapOfIter (fun f -> NodeCrate.iteriChildren packedNode (fun _ node -> f node)) NodeCrate.nodeId

                let bindChildren =
                    mapOfIter (maybeIterOnBindNodesCreatedOnRhs packedNode) NodeCrate.nodeId

                let id = NodeCrate.nodeId packedNode
                let kind = kind packedNode
                let cutoff = cutoff packedNode
                let userInfo = NodeCrate.userInfo packedNode
                let recomputedAt = NodeCrate.recomputedAt packedNode
                let changedAt = NodeCrate.changedAt packedNode
                let height = NodeCrate.height packedNode

                addNode id kind cutoff children bindChildren userInfo recomputedAt changedAt height
            )

    let directlyObserved s = State.directlyObserved s
