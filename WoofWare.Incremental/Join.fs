namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Join =

    let invariant (_inv : 'a -> unit) (t : Join<'a>) : unit =
        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.JoinMain t' -> if not (Object.ReferenceEquals (t, t')) then failwith "invariant failed"
        | k -> failwith $"invariant failed: {k}"

        match t.LhsChange.Kind with
        | Kind.Invalid -> ()
        | Kind.JoinLhsChange (t', _) ->
            { new JoinEval<_> with
                member _.Eval t' =
                    if not (Object.ReferenceEquals (t, t')) then
                        failwith "invariant failed"
                    FakeUnit.ofUnit ()
            }
            |> t'.Apply
            |> FakeUnit.toUnit
        | k -> failwith $"invariant failed: {k}"
