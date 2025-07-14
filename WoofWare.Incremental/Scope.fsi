namespace WoofWare.PlayFetch

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Scope =
    val top : Scope
    val isTop : Scope -> bool
    val height : Scope -> int
    val isValid : Scope -> bool
    val isNecessary : Scope -> bool
    val addNode : Scope -> _ Node -> unit
