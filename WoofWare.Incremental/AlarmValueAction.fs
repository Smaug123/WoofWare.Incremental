namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal AlarmValueAction =
    let invariant (t : AlarmValueAction) : unit =
        match t with
        | AlarmValueAction.At at -> At.invariant at
        | AlarmValueAction.AtIntervals atIntervals -> AtIntervals.invariant atIntervals
        | AlarmValueAction.Snapshot snapshot ->
            { new SnapshotEval<_> with
                member _.Eval snapshot =
                    Snapshot.invariant ignore snapshot
                    |> FakeUnit.ofUnit
            }
            |> snapshot.Apply
            |> FakeUnit.toUnit
        | AlarmValueAction.StepFunction node ->
            { new StepFunctionNodeEval<_> with
                member _.Eval node =
                    StepFunctionNode.invariant ignore node
                    |> FakeUnit.ofUnit
            }
            |> node.Apply
            |> FakeUnit.toUnit
