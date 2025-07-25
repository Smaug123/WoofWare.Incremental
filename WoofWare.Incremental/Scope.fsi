namespace WoofWare.Incremental

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal Scope =
    val top : Scope
    val isTop : Scope -> bool
    val height : Scope -> int
    val isValid : Scope -> bool
    val isNecessary : Scope -> bool
    val addNode : Scope -> _ Node -> unit

    val internal invariant : Scope -> unit

    val internal equal : Scope -> Scope -> bool
