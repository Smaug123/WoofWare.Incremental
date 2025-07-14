namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
type internal StaleResult =
    | AlreadyStale
    | Ok

[<RequireQualifiedAccess>]
type internal BeforeMainComputationResult =
    | Invalid
    | Ok

[<RequireQualifiedAccess>]
module internal Expert =
    let invariant
      invariantA
      {
        Expert.Children = children
        NumChildren = numChildren
      }
      =
      assert (numChildren <= children.Length);
      (* invariant is below, because we need some context *)
      children
      |> Array.iteri (fun i opt ->
          if i < numChildren then
              { new ExpertEdgeEval<FakeUnit> with
                  member _.Eval e =
                      assert (e.Index.Value = i)
                      FakeUnit.ofUnit ()
              }
              |> opt.Value.Apply
              |> FakeUnit.toUnit
          else
              assert opt.IsNone
      )

    let invariantAboutNumInvalidChildren (t : Expert<'a>) (isNecessary : bool) : unit =
      let { Children = children; NumChildren = numChildren; NumInvalidChildren = numInvalidChildren } = t
      if not isNecessary then
          assert (numInvalidChildren = 0)
      else
        let mutable countInvalidChildren = 0 in
        for i = 0 to numChildren - 1 do
          { new ExpertEdgeEval<_> with
              member _.Eval r =
                  if not (Node.isValid r.Child) then
                      countInvalidChildren <- countInvalidChildren + 1
                  FakeUnit.ofUnit ()
          }
          |> children.[i].Value.Apply
          |> FakeUnit.toUnit
        assert (numInvalidChildren = countInvalidChildren)

    let create f onObservabilityChange =
      {
          F = f
          OnObservabilityChange = onObservabilityChange
          Children = [||]
          NumChildren = 0
          ForceStale = false
          NumInvalidChildren = 0
          WillFireAllCallbacks = true
      }

    let makeStale (t: Expert<'a>) : StaleResult =
        if t.ForceStale then StaleResult.AlreadyStale
        else
            t.ForceStale <- true
            StaleResult.Ok

    let incrInvalidChildren t = t.NumInvalidChildren <- t.NumInvalidChildren + 1
    let decrInvalidChildren t = t.NumInvalidChildren <- t.NumInvalidChildren - 1

    let makeSpaceForChildIfNecessary (t: Expert<'a>) : unit =
      if t.NumChildren >= t.Children.Length then
#if DEBUG
        assert (t.NumChildren = t.Children.Length)
#endif
      let new_max = max 2 (2 * t.Children.Length) in
      t.Children <- Uniform_array.realloc t.children new_max

    let addChildEdge (t : Expert<'a>) (packedEdge : ExpertEdgeCrate) =
      { new ExpertEdgeEval<_> with
          member _.Eval edge =
              assert edge.Index.IsNone
              makeSpaceForChildIfNecessary t
              let newChildIndex = t.NumChildren
              edge.Index <- ValueSome newChildIndex
              t.Children.[newChildIndex] <- ValueSome packedEdge
              t.NumChildren <- t.NumChildren + 1
              t.ForceStale <- true
              // We will bump the number of invalid children if necessary when connecting child and
              // parent.  Same thing for running the [on_change] callbacks.
              newChildIndex

      }

    let swapChildren (t : Expert<_>) childIndex1 childIndex2 =
      { new ExpertEdgeEval<_> with
          member _.Eval edge1 =
              { new ExpertEdgeEval<_> with
                  member _.Eval edge2 =
                      edge1.Index <- ValueSome childIndex2
                      edge2.Index <- ValueSome childIndex1
                      let tmp = t.Children.[childIndex1]
                      t.Children.[childIndex1] <- t.Children.[childIndex2]
                      t.Children.[childIndex2] <- tmp
                      FakeUnit.ofUnit ()
              }
              |> t.Children.[childIndex2].Value.Apply
      }
      |> t.Children.[childIndex1].Value.Apply
      |> FakeUnit.toUnit

    let lastChildEdgeThrowing (t : Expert<_>) =
      let lastIndex = t.NumChildren - 1
      t.Children.[lastIndex].Value

    let removeLastChildEdgeThrowing t : unit =
      let lastIndex = t.NumChildren - 1
      let packedEdgeOpt = t.Children.[lastIndex]
      t.Children.[lastIndex] <- ValueNone
      t.NumChildren <- lastIndex
      t.ForceStale <- true;
      assert packedEdgeOpt.IsSome
      { new ExpertEdgeEval<_> with
          member _.Eval edge =
              edge.Index <- ValueNone
              FakeUnit.ofUnit ()
      }
      |> packedEdgeOpt.Value.Apply
      |> FakeUnit.toUnit

    let before_main_computation (t: Expert<'a>) : BeforeMainComputationResult =
      if t.NumInvalidChildren > 0 then BeforeMainComputationResult.Invalid else
        t.ForceStale <- false
        let willFireAllCallbacks = t.WillFireAllCallbacks
        t.WillFireAllCallbacks <- false
        if willFireAllCallbacks then
          for i = 0 to t.NumChildren - 1 do
            { new ExpertEdgeEval<_> with
                member _.Eval r =
                    r.OnChange r.Child.ValueOpt.Value
                    FakeUnit.ofUnit ()
            }
            |> t.Children.[i].Value.Apply
            |> FakeUnit.toUnit
        BeforeMainComputationResult.Ok

    let observabilityChange t isNowObservable =
      t.OnObservabilityChange isNowObservable
      if not isNowObservable
      then
        t.WillFireAllCallbacks <- true
        (* If we don't reset num_invalid_children, we would double count them: just imagine
           what happens if we reconnect/disconnect/reconnect/disconnect with an invalid
           child. *)
        t.NumInvalidChildren <- 0

    let runEdgeCallback (t: Expert<'a>) (childIndex: int) : unit =
      if not t.WillFireAllCallbacks then
        { new ExpertEdgeEval<_> with
            member _.Eval r =
                (* This value is not necessarily set, because we try to run this when connecting the
                   node to its children, which could be before they have run even once.  Also the node
                   could be invalid. *)
                match r.Child.ValueOpt with
                | ValueSome v -> r.OnChange v
                | ValueNone -> ()
                |> FakeUnit.ofUnit
        }
        |> t.Children.[childIndex].Value.Apply
        |> FakeUnit.toUnit
