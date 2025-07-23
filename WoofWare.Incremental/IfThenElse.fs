namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal IfThenElse =

    let physSame<'a, 'b> (a : IfThenElse<'a>) (b : IfThenElse<'b>) : bool = Type.referenceEqual' a b

    let invariant (_inv : 'a -> unit) (t : IfThenElse<'a>) : unit =
        do
            match t.Main.Kind with
            | Kind.Invalid -> ()
            | Kind.IfThenElse t' ->
                if not (Type.referenceEqual t t') then
                    failwith "invariant failure"
            | _ -> ()

        do
            match t.TestChange.Kind with
            | Kind.Invalid -> ()
            | Kind.IfTestChange (cr, _) ->
                { new IfThenElseEval<_> with
                    member _.Eval t' =
                        if not (physSame t t') then
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
                if
                    not (
                        Type.referenceEqual currentBranch t.Then
                        || Type.referenceEqual currentBranch t.Else
                    )
                then
                    failwith "invariant failure"
