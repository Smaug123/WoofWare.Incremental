namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bind =
    let isValid (t : Bind<_, _>) =
        match t.Main.Kind with
        | Kind.Invalid -> false
        | _ -> true

    let physSame<'a, 'b, 'c, 'd> (a : Bind<'a, 'b>) (b : Bind<'c, 'd>) : bool =
        Type.referenceEqual' a b

    let iterNodesCreatedOnRhs (t : Bind<'a, 'b>) (f : NodeCrate -> unit) : unit =
        let mutable r = t.AllNodesCreatedOnRhs

        while r.IsSome do
            { new NodeEval<_> with
                member _.Eval nodeOnRhs =
                    r <- nodeOnRhs.NextNodeInSameScope
                    // TODO: inefficient, it's already in scope later
                    f (NodeCrate.make nodeOnRhs)
                    FakeUnit.ofUnit ()
            }
            |> r.Value.Apply
            |> FakeUnit.toUnit

    let invariant (_invA : 'a -> unit) (_invB : 'b -> unit) (t : Bind<'a, 'b>) : unit =
        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.BindMain cr ->
            { new BindMainEval<_, _> with
                member _.Eval t' =
                    if not (physSame t t') then
                        failwith "invariant failed"

                    FakeUnit.ofUnit ()
            }
            |> cr.Apply
            |> FakeUnit.toUnit
        | k -> failwith $"invariant failed: {k}"

        match t.RhsScope with
        | Scope.Top -> failwith "invariant failed"
        | Scope.Bind t' ->
            { new BindEval<_> with
                member _.Eval t' =
                    if not (physSame t t') then
                        failwith "invariant failed"

                    FakeUnit.ofUnit ()
            }
            |> t'.Apply
            |> FakeUnit.toUnit

        iterNodesCreatedOnRhs
            t
            (fun node ->
                { new NodeEval<_> with
                    member _.Eval node =
                        if not (Type.referenceEqual node.CreatedIn t.RhsScope) then
                            failwith "invariant failed"

                        if NodeHelpers.isNecessary node then
                            if t.LhsChange.Height >= node.Height then
                                failwith "invariant failed"

                        FakeUnit.ofUnit ()
                }
                |> node.Apply
                |> FakeUnit.toUnit
            )

        do
            if not (Type.referenceEqual t.LhsChange.CreatedIn t.Main.CreatedIn) then
                failwith "invariant failed"

            match t.LhsChange.Kind with
            | Kind.Invalid -> ()
            | Kind.BindLhsChange (node, _) ->
                { new BindEval<_> with
                    member _.Eval t' =
                        if not (physSame t t') then
                            failwith "invariant failed"

                        FakeUnit.ofUnit ()
                }
                |> node.Apply
                |> FakeUnit.toUnit
            | k -> failwith $"invariant failed: {k}"
