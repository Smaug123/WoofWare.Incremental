namespace WoofWare.Incremental

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal AtIntervals =

    let invariant (t : AtIntervals) : unit =
        if not (TimeNs.Span.isPositive t.Interval) then
            failwith "invariant failed"

        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.AtIntervals (t', _) ->
            if not (Type.referenceEqual t t') then
                failwith "invariant failed"
        | k -> failwith $"invariant failed: %O{k}"
