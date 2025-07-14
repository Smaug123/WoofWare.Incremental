namespace WoofWare.PlayFetch

// A Var<'a> is a leaf in the incremental DAG.

type internal VarEval<'ret> =
    abstract Eval<'a> : Var<'a> -> 'ret

type internal VarCrate =
    abstract Apply<'ret> : VarEval<'ret> -> 'ret

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Var =
    val latestValue : Var<'t> -> 't
    val incrState : Var<'a> -> State
