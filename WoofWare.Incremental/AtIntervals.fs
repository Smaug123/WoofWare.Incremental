namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Freeze =

    let invariant (_inv : 'a -> unit) (t : Freeze<'a>) : unit =
        if not (Scope.isTop t.Main.CreatedIn) then
            failwith "invariant failed"
        match t.Main.Kind with
        | Kind.Invalid ->
            // happens when freezing an invalid value
            ()
        | Kind.Const _ ->
            // happens on becoming frozen
            ()
        | Kind.Freeze t' -> if not (Object.ReferenceEquals (t, t')) then failwith "invariant failed"
        | k -> failwith $"invariant failed: {k}"
