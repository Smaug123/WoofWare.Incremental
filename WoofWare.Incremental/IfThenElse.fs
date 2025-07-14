namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal IfThenElse =

    let invariant (_inv : 'a -> unit) (t : IfThenElse<'a>) : unit =
        do
            match t.Main.Kind with
            | Kind.Invalid -> ()
            | Kind.IfThenElse t' -> if not (Object.ReferenceEquals (t, t')) then failwith "invariant failure"
            | _ -> ()

        do
            match t.TestChange.Kind with
            | Kind.Invalid -> ()
            | Kind.IfTestChange (cr, t) ->
                { new IfThenElseEval<_> with
                    member _.Eval t' =
                        if not (Object.ReferenceEquals (t, t')) then
                            failwith "invariant failure"
                        FakeUnit.ofUnit ()
                }
                |> cr.Apply
                |> FakeUnit.toUnit
            | k -> failwith $"invariant failure: {k}"

        do
            match t.CurrentBranch with
            | ValueNone -> ()
            | ValueSome currentBranch ->
                if not (Object.ReferenceEquals (currentBranch, t.Then) || Object.ReferenceEquals (currentBranch, t.Else)) then
                    failwith "invariant failure"
