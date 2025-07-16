namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module internal AlarmValue =
    let invariant (t : AlarmValue) : unit =
        AlarmValueAction.invariant t.Action

    let create (action: AlarmValueAction) : AlarmValue = { Action = action; NextFired = ValueNone }
