namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal RunOnUpdateHandlers =

    /// rOUHs? I don't think they exist.
    let invariant (t : RunOnUpdateHandlers) : unit =
        { new RunOnUpdateHandlersEval<_> with
            member _.Eval node _update =
                Node.invariant ignore node |> FakeUnit.ofUnit
        }
        |> t.Apply
        |> FakeUnit.toUnit

    let make<'a> (node : Node<'a>) (update : NodeUpdate<'a>) : RunOnUpdateHandlers =
        { new RunOnUpdateHandlers with
            member _.Apply e = e.Eval node update
        }
