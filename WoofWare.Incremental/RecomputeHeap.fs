namespace WoofWare.PlayFetch

open System

// module As_recompute_list = Node.Packed.As_list (struct
//     let next (Node.Packed.T node) = node.next_in_recompute_heap
//   end)

type NodesByHeight = AsRecomputeList[]

[<RequireQualifiedAccess>]
module RecomputeHeap =
    let maxHeightAllowed (t: RecomputeHeap) : int = t.NodesByHeight.Length - 1
    let isEmpty (t: RecomputeHeap) : bool = t.Length = 0

    let invariant (t : RecomputeHeap) : unit =
      do
          let mutable actualLength = 0
          t.NodesByHeight
          |> Array.iter (fun node ->
              actualLength <- actualLength + AsRecomputeList.length node
          )

          if t.Length <> actualLength then failwith "incorrect length"

      do
          if t.HeightLowerBound < 0 then failwith "HeightLowerBound must be nonnegative"
          if t.HeightLowerBound > t.NodesByHeight.Length then failwith "HeightLowerBound must be at most NodesByHeight length"
          for height = 0 to t.HeightLowerBound - 1 do
              match t.NodesByHeight.[height] with
              | ValueNone -> ()
              | ValueSome _ -> failwith "expected nodes to be None"

      do
          t.NodesByHeight
          |> Array.iteri (fun height node ->
                  match node with
                  | ValueNone -> ()
                  | ValueSome node ->
                      { new NodeEval<_> with
                          member _.Eval node =
                              if node.HeightInRecomputeHeap <> height then failwith "bad height"
                              if not (Node.needsToBeComputed node) then failwith "expected node needs to be computed"
                              FakeUnit.ofUnit ()
                      }
                      |> node.Apply
                      |> FakeUnit.toUnit
          )

    let createNodesByHeight maxHeightAllowed : ValueOption<NodeCrate> array =
        Array.zeroCreate (maxHeightAllowed + 1)

    let setMaxHeightAllowed t maxHeightAllowed =
      if Debug.globalFlag then
        for i = maxHeightAllowed + 1 to t.NodesByHeight.Length - 1 do
          assert t.NodesByHeight.[i].IsNone
      let src = t.NodesByHeight
      let dst = createNodesByHeight maxHeightAllowed
      Array.blit src 0 dst 0 (min src.Length dst.Length)
      t.NodesByHeight <- dst
      t.HeightLowerBound <- min t.HeightLowerBound dst.Length

    let create maxHeightAllowed =
      {
          Length = 0
          HeightLowerBound = maxHeightAllowed + 1
          NodesByHeight = createNodesByHeight maxHeightAllowed
      }

    let setNext (prev : NodeCrate voption) (next: NodeCrate voption) : unit =
      match prev with
      | ValueSome prev ->
          { new NodeEval<_> with
              member _.Eval node =
                  node.NextInRecomputeHeap <- next
                  FakeUnit.ofUnit ()
          }
          |> prev.Apply
          |> FakeUnit.toUnit
      | ValueNone -> ()

    let setPrev (next : NodeCrate voption) (prev: NodeCrate voption) : unit =
      match next with
      | ValueSome next ->
          { new NodeEval<_> with
              member _.Eval node =
                  node.PrevInRecomputeHeap <- prev
                  FakeUnit.ofUnit ()
          }
          |> next.Apply
          |> FakeUnit.toUnit
      | ValueNone -> ()

    let link<'a> (t: RecomputeHeap) (node : 'a Node) : unit =
      let height = node.Height
      if Debug.globalFlag then assert (height <= maxHeightAllowed t)
      node.HeightInRecomputeHeap <- height
      let next = t.NodesByHeight.[height]
      node.NextInRecomputeHeap <- next
      let node = NodeCrate.make node
      setPrev next (ValueSome node)
      t.NodesByHeight.[height] <- ValueSome node

    let unlink<'a> (t : RecomputeHeap) (node : 'a Node) : unit =
      let prev = node.PrevInRecomputeHeap
      let next = node.NextInRecomputeHeap
      match t.NodesByHeight.[node.HeightInRecomputeHeap] with
      | ValueNone -> ()
      | ValueSome existing ->
          if Object.ReferenceEquals (node, existing) then
             t.NodesByHeight.[node.HeightInRecomputeHeap] <- next
      setPrev next prev
      setNext prev next
      node.PrevInRecomputeHeap <- ValueNone

    // We don't set [node.next_in_recompute_heap] here, but rather after calling [unlink].

    let add<'a> (t : RecomputeHeap) (node : 'a Node) : unit =
      if Debug.globalFlag && (Node.isInRecomputeHeap node || not (Node.needsToBeComputed node)) then
        failwith "incorrect attempt to add node to recompute heap"
      if Debug.globalFlag then assert (node.Height <= maxHeightAllowed t)
      let height = node.Height
      if height < t.HeightLowerBound then t.HeightLowerBound <- height
      link t node
      t.Length <- t.Length + 1

    let remove<'a> (t : RecomputeHeap) (node : 'a Node) : unit =
      if Debug.globalFlag && ((not (Node.isInRecomputeHeap node)) || Node.needsToBeComputed node) then
        failwith "incorrect [remove] of node from recompute heap"
      unlink t node
      node.NextInRecomputeHeap <- ValueNone
      node.HeightInRecomputeHeap <- -1
      t.Length <- t.Length - 1

    let increaseHeight<'a> (t: RecomputeHeap) (node : 'a Node) : unit =
      if Debug.globalFlag then
        assert (node.Height > node.HeightInRecomputeHeap)
        assert (node.Height <= maxHeightAllowed t)
        assert (Node.isInRecomputeHeap node)
      unlink t node
      link t node

    let minHeight (t: RecomputeHeap) : int =
      if t.Length = 0 then
          t.HeightLowerBound <- t.NodesByHeight.Length
      else
        let nodesByHeight = t.NodesByHeight
        while nodesByHeight.[t.HeightLowerBound].IsNone do
          t.HeightLowerBound <- t.HeightLowerBound + 1
      t.HeightLowerBound

    let removeMin t : NodeCrate =
      if Debug.globalFlag then
          if isEmpty t then
              failwith "expected nonempty if there was a min"
      let nodesByHeight = t.NodesByHeight in
      let mutable node = nodesByHeight.[t.HeightLowerBound]
      while node.IsNone do
        t.HeightLowerBound <- t.HeightLowerBound + 1;
        if Debug.globalFlag && t.HeightLowerBound >= t.NodesByHeight.Length then
          failwith "Recompute_heap.remove_min unexpectedly reached end of heap"
        node <- nodesByHeight.[t.HeightLowerBound]
      { new NodeEval<_> with
          member _.Eval node =
              node.HeightInRecomputeHeap <- -1
              t.Length <- t.Length - 1
              let next = node.NextInRecomputeHeap
              t.NodesByHeight.[t.HeightLowerBound] <- next
              setPrev next ValueNone
              if Debug.globalFlag then
                  if node.PrevInRecomputeHeap.IsSome then
                      failwith "expected prev node to be None"
              node.NextInRecomputeHeap <- ValueNone
              NodeCrate.make node
      }
      |> node.Value.Apply
