namespace WoofWare.PlayFetch

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Scope =

    let top = Scope.Top

    let isTop (s : Scope) =
      match s with
      | Top -> true
      | Bind _ -> false

    (* Unlike for nodes, there is no invariant [is_necessary t <=> height > -1] (doesn't work
       because of [Top]).  This is fine since the height of a scope is only used to constrain
       other heights, not to schedule it. *)
    let height (s : Scope) =
      match s with
      | Top -> -1
      | Bind bind ->
          { new BindEval<_> with
              member _.Eval bind =
                  bind.LhsChange.Height
          }
          |> bind.Apply

    let isValid (s : Scope) =
      match s with
      | Top -> true
      | Bind bind ->
          { new BindEval<_> with
              member _.Eval e = Bind.isValid e
          }
          |> bind.Apply

    let isNecessary (s : Scope) =
      match s with
      | Top -> true
      | Bind bind ->
          { new BindEval<_> with
              member _.Eval bind =
                  bind.Main
                  |> Node.isNecessary
          }
          |> bind.Apply

    let addNode (t : Scope) (node : 'a Node) : unit =
      assert (Object.ReferenceEquals (node.CreatedIn, t))
      match t with
      | Top -> ()
      | Bind bind ->
        { new BindEval<FakeUnit> with
            member _.Eval bind =
                node.NextNodeInSameScope <- bind.AllNodesCreatedOnRhs;
                bind.AllNodesCreatedOnRhs <- ValueSome (NodeCrate.make node)
                FakeUnit.ofUnit ()
        }
        |> bind.Apply
        |> FakeUnit.toUnit
