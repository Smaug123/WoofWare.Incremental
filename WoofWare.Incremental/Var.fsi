namespace WoofWare.Incremental

// A Var<'a> is a leaf in the incremental DAG.

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Var =
    val latestValue : Var<'t> -> 't
    val incrState : Var<'a> -> State

    val invariant<'a> : ('a -> unit) -> Var<'a> -> unit
