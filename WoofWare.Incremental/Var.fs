namespace WoofWare.PlayFetch

type VarEval<'ret> =
    abstract Eval<'a> : Var<'a> -> 'ret

type VarCrate =
    abstract Apply<'ret> : VarEval<'ret> -> 'ret

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Var =

    let incrState t = t.Watch.State

    let latestValue (t : Var<'a>) =
        match t.ValueSetDuringStabilization with
        | Some t -> t
        | None -> t.Value
