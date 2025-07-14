namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal Kind =

    let name<'a> (k : Kind<'a>) : string =
        match k with
          | Kind.ArrayFold _ -> "Array_fold"
          | Kind.At _ -> "At"
          | Kind.AtIntervals _ -> "At_intervals"
          | Kind.BindLhsChange _ -> "Bind_lhs_change"
          | Kind.BindMain _ -> "Bind_main"
          | Kind.Const _ -> "Const"
          | Kind.Expert _ -> "Expert"
          | Kind.Freeze _ -> "Freeze"
          | Kind.IfTestChange _ -> "If_test_change"
          | Kind.IfThenElse _ -> "If_then_else"
          | Kind.Invalid -> "Invalid"
          | Kind.JoinLhsChange _ -> "Join_lhs_change"
          | Kind.JoinMain _ -> "Join_main"
          | Kind.Map _ -> "Map"
          | Kind.Map2 _ -> "Map2"
          | Kind.Snapshot _ -> "Snapshot"
          | Kind.StepFunction _ -> "Step_function"
          | Kind.Uninitialized -> "Uninitialized"
          | Kind.UnorderedArrayFold _ -> "Unordered_array_fold"
          | Kind.Var _ -> "Var"

    let initialNumChildren<'a> (t : Kind<'a>) =
          match t with
          | Kind.At _ -> 0
          | Kind.AtIntervals _ -> 0
          | Kind.BindLhsChange _ -> 1
          | Kind.BindMain _ -> 2
          | Kind.Const _ -> 0
          | Kind.Expert _ -> 0
          | Kind.Freeze _ -> 1
          | Kind.IfTestChange _ -> 1
          | Kind.IfThenElse _ -> 2
          | Kind.Invalid -> 0
          | Kind.JoinLhsChange _ -> 1
          | Kind.JoinMain _ -> 2
          | Kind.Map _ -> 1
          | Kind.Map2 _ -> 2
          | Kind.Snapshot _ -> 0
          | Kind.StepFunction _ -> 1
          | Kind.Uninitialized -> 0
          | Kind.Var _ -> 0
          | Kind.ArrayFold cr ->
              { new ArrayFoldEval<_, _> with
                  member _.Eval c = c.Children.Length
              }
              |> cr.Apply
          | Kind.UnorderedArrayFold cr ->
              { new UnorderedArrayFoldEval<_, _> with
                  member _.Eval c = c.Children.Length
              }
              |> cr.Apply

    let bindRhsChildIndex = 1
    let freezeChildIndex = 0
    let ifBranchChildIndex = 1
    let joinRhsChildIndex = 1

    /// We do not implement the time-based nodes ([At], [At_intervals], [Snapshot],
    /// [Step_function]) as parents of the current-time node for performance reasons.  We don't
    /// want all such nodes to be recomputed whenever the time changes, which would be horribly
    /// inneficient.  Instead, we only want them to be recomputed at the "right" time,
    /// i.e. when time passes some threshold relevant to them.  We do this via scheduling
    /// alarms at those thresholds.
    let iteriChildren<'a> (t : Kind<'a>) (f : int -> NodeCrate -> unit) : unit =
      match t with
      | Kind.ArrayFold cr ->
        { new ArrayFoldEval<_, _> with
            member _.Eval fold =
                for i = 0 to Array.length fold.Children - 1 do
                  fold.Children.[i]
                  |> NodeCrate.make
                  |> f i
                FakeUnit.ofUnit ()
        }
        |> cr.Apply
        |> FakeUnit.toUnit
      | Kind.At _
      | Kind.AtIntervals _ -> ()
      | Kind.BindLhsChange (bind, _) ->
          { new BindEval<_> with
              member _.Eval bind =
                  bind.Lhs |> NodeCrate.make |> f 0 |> FakeUnit.ofUnit
          }
          |> bind.Apply
          |> FakeUnit.toUnit
      | Kind.BindMain cr ->
        { new BindMainEval<_, _> with
            member _.Eval bind =
                // Various code, e.g. [state.became_necessary], relies on processing [lhs_change] before [rhs].
                bind.LhsChange
                |> NodeCrate.make
                |> f 0
                match bind.Rhs with
                | ValueNone -> ()
                | ValueSome b ->
                    b
                    |> NodeCrate.make
                    |> f 1
                FakeUnit.ofUnit ()
        }
        |> cr.Apply
        |> FakeUnit.toUnit
      | Kind.Const _ -> ()
      | Kind.Expert expert ->
        for i = 0 to expert.NumChildren - 1 do
          { new ExpertEdgeEval<_> with
              member _.Eval edge =
                  edge.Child
                  |> NodeCrate.make
                  |> f i
                  |> FakeUnit.ofUnit
          }
          |> expert.Children.[i].Value.Apply
          |> FakeUnit.toUnit
      | Kind.Freeze freeze -> freeze.Child |> NodeCrate.make |> f 0
      | Kind.IfTestChange (iTC, _) ->
          { new IfThenElseEval<_> with
              member _.Eval iTE =
                  iTE.Test
                  |> NodeCrate.make
                  |> f 0
                  |> FakeUnit.ofUnit
          }
          |> iTC.Apply
          |> FakeUnit.toUnit
      | Kind.IfThenElse iTE ->
        f 0 (NodeCrate.make iTE.TestChange)
        match iTE.CurrentBranch with
        | ValueNone -> ()
        | ValueSome b -> f 1 (NodeCrate.make b)
      | Kind.Invalid -> ()
      | Kind.JoinLhsChange (cr, _) ->
          { new JoinEval<_> with
              member _.Eval e =
                  e.Lhs
                  |> NodeCrate.make
                  |> f 0
                  |> FakeUnit.ofUnit
          }
          |> cr.Apply
          |> FakeUnit.toUnit
      | Kind.JoinMain join ->
        join.LhsChange
        |> NodeCrate.make
        |> f 0
        match join.Rhs with
        | ValueNone -> ()
        | ValueSome rhs -> rhs |> NodeCrate.make |> f 1
      | Kind.Snapshot _ -> ()
      | Kind.StepFunction stepFunc ->
        match stepFunc.Child with
        | ValueNone -> ()
        | ValueSome child -> NodeCrate.make child |> f 0
      | Kind.Uninitialized -> ()
      | Kind.UnorderedArrayFold cr ->
        { new UnorderedArrayFoldEval<_, _> with
            member _.Eval e =
                for i = 0 to Array.length e.Children - 1 do
                    f i (NodeCrate.make e.Children.[i])
                FakeUnit.ofUnit ()
        }
        |> cr.Apply
        |> FakeUnit.toUnit
      | Kind.Var _ -> ()
      | Kind.Map cr ->
          { new MapEval<_, _> with
              member _.Eval (_, node) =
                  f 0 (NodeCrate.make node)
                  |> FakeUnit.ofUnit
           }
          |> cr.Apply
          |> FakeUnit.toUnit
      | Kind.Map2 cr ->
        { new Map2Eval<_, _> with
            member _.Eval (_, node0, node1) =
             f 0 (NodeCrate.make node0)
             f 1 (NodeCrate.make node1)
             FakeUnit.ofUnit ()
             }
        |> cr.Apply
        |> FakeUnit.toUnit

    exception private StopIteration of NodeCrate

    /// Only used by Node.invariant, so we don't mind using withReturn and iteriChildren.
    /// If we ever need a fast [get_child], we coded it in rev 48dbfd03c9c5.
    /// (patricks note: that rev does not appear to be public.)
    let slowGetChild<'a> (t : Kind<'a>) index : NodeCrate =
        match t with
        | Kind.ArrayFold cr ->
            { new ArrayFoldEval<_, _> with
                member _.Eval af = NodeCrate.make af.Children.[index]
            }
            |> cr.Apply
        | Kind.UnorderedArrayFold cr ->
            { new UnorderedArrayFoldEval<_, _> with
                member _.Eval af = NodeCrate.make af.Children.[index]
            }
            |> cr.Apply
        | Kind.Expert e ->
            { new ExpertEdgeEval<_> with
                member _.Eval e = e.Child |> NodeCrate.make
            }
            |> e.Children.[index].Value.Apply
        | _ ->
            try
                iteriChildren t (fun i child ->
                    if i = index then
                        raise (StopIteration child)
                )
                failwith $"Kind.slowGetChild got invalid index %i{index}"
            with
            | :? StopIteration as e -> e.Data0
            | _ -> reraise ()
