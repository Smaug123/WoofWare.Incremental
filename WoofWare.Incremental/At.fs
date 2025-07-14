namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal At =

    let invariant (t : At) : unit =
        Alarm.invariant t.Alarm
        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.Const BeforeOrAfter.After ->
            // happens once the current time passes [t.at].
            ()
        | Kind.At (t', _) ->
            if not (Object.ReferenceEquals (t, t')) then
                failwith "invariant failed"
        | k -> failwith $"invariant failed: {k}"
