namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bind =
    let isValid (t : Bind<_, _>) =
      match t.Main.Kind with
      | Kind.Invalid -> false
      | _ -> true

    let iterNodesCreatedOnRhs (t: Bind<'a,'b>) (f : NodeCrate -> unit) : unit =
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
