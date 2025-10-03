namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Var =

    let incrState t = t.Watch.State

    let latestValue (t : Var<'a>) =
        match t.ValueSetDuringStabilization with
        | Some t -> t
        | None -> t.Value

    let invariant (invA : 'a -> unit) (v : Var<'a>) : unit =
        invA v.Value
        v.ValueSetDuringStabilization |> Option.iter invA
        StabilizationNum.invariant v.SetAt

        match v.Watch.Kind with
        | Kind.Invalid ->
            // possible with useCurrentScope = true
            ()
        | Kind.Var t' ->
            if not (Type.referenceEqual v t') then
                failwith "invariant failed"
        | k -> failwith $"invariant failed: %O{k}"
