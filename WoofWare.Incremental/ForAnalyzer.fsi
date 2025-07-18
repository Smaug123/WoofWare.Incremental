namespace WoofWare.Incremental

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
        val toString : Cutoff -> string

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
        val toString : Kind -> string

    /// Args to the addNode callback:
    /// id, kind, cutoff, children, bindChildren, userInfo, recomputedAt, changedAt, height
    val traverse :
        NodeCrate list ->
        addNode :
            (NodeId
                -> Kind
                -> Cutoff
                -> NodeId seq
                -> NodeId seq
                -> DotUserInfo option
                -> StabilizationNum
                -> StabilizationNum
                -> int
                -> unit) ->
            unit
