namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal RunOnUpdateHandlers =

    let invariant (t : RunOnUpdateHandlers) : unit =
        { new RunOnUpdateHandlersEval<_> with
            member _.Eval node _update =
                Node.invariant ignore node
                |> FakeUnit.ofUnit
        }
        |> t.Apply
        |> FakeUnit.toUnit
