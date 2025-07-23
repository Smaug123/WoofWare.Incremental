namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Snapshot =

    let invariant (inv : 'a -> unit) (t : Snapshot<'a>) : unit =
        do
            if not (Scope.isTop t.Main.CreatedIn) then
                failwith "invariant failed"

            match t.Main.Kind with
            | Kind.Invalid ->
                // happens when snapshotting an invalid node
                ()
            | Kind.Const _ ->
                // happens after the snapshot
                ()
            | Kind.Snapshot t' ->
                if not (Type.referenceEqual t t') then
                    failwith "invariant failed"
            | k -> failwith $"invariant failed: {k}"

        inv t.Before
