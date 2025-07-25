namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module NodesByHeight =
    let private asAdjustHeightsList : NodeCrate.AsList =
        {
            Next = NodeCrate.nextInAdjustHeightsHeap
        }

    let invariant (t : NodeCrate voption[]) =
        t
        |> Array.iteri (fun height nodes ->
            NodeCrate.AsList.invariant asAdjustHeightsList nodes

            NodeCrate.AsList.iter
                asAdjustHeightsList
                nodes
                (fun node ->
                    let heightInAdjustHeap = NodeCrate.heightInAdjustHeightsHeap node

                    if height <> heightInAdjustHeap then
                        failwith "invariant failed"

                    if NodeCrate.height node <= heightInAdjustHeap then
                        failwith "invariant failed"

                    if NodeCrate.isInRecomputeHeap node then
                        if heightInAdjustHeap <> NodeCrate.heightInRecomputeHeap node then
                            failwith "invariant failed"
                )
        )

    let create (maxHeightAllowed : int) : NodeCrate voption[] = Array.zeroCreate (maxHeightAllowed + 1)

    let length (t : NodeCrate voption[]) : int =
        let mutable r = 0

        for node in t do
            r <- r + NodeCrate.AsList.length asAdjustHeightsList node

        r

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal AdjustHeightsHeap =
    let maxHeightSeen (t : AdjustHeightsHeap) = t.MaxHeightSeen
    let length (t : AdjustHeightsHeap) = t.Length

    let isEmpty (t : AdjustHeightsHeap) = t.Length = 0
    let maxHeightAllowed (t : AdjustHeightsHeap) = t.NodesByHeight.Length - 1

    let invariant (t : AdjustHeightsHeap) : unit =
        if t.Length <> NodesByHeight.length t.NodesByHeight then
            failwith "invariant failed"

        do
            if t.HeightLowerBound < 0 then
                failwith "invariant failed"

            if t.HeightLowerBound > t.NodesByHeight.Length then
                failwith "invariant failed"

            for height = 0 to t.HeightLowerBound - 1 do
                if t.NodesByHeight.[height].IsSome then
                    failwith "invariant failed"

        do
            if t.MaxHeightSeen < 0 then
                failwith "invariant failed"

            if t.MaxHeightSeen > maxHeightAllowed t then
                failwith "invariant failed"

        NodesByHeight.invariant t.NodesByHeight

    let create (maxHeightAllowed : int) : AdjustHeightsHeap =
        {
            Length = 0
            HeightLowerBound = maxHeightAllowed + 1
            MaxHeightSeen = 0
            NodesByHeight = NodesByHeight.create maxHeightAllowed
        }

    let setMaxHeightAllowed (t : AdjustHeightsHeap) (maxHeightAllowed : int) : unit =
        if maxHeightAllowed < t.MaxHeightSeen then
            failwith "cannot set_max_height_allowed less than the max height already seen"

        if Debug.globalFlag then
            if not (isEmpty t) then
                failwith "setMaxHeightAllowed expected empty heap"

        t.NodesByHeight <- NodesByHeight.create maxHeightAllowed

    let addUnlessMem<'a> (t : AdjustHeightsHeap) (node : 'a Node) : unit =
        if node.HeightInAdjustHeightsHeap = -1 then
            let height = node.Height

            if Debug.globalFlag then
                // We process nodes in increasing order of pre-adjusted height, so it is a bug if we
                // ever try to add a node that would violate that.
                if height < t.HeightLowerBound then
                    failwith "nodes should be processed in increasing pre-adjust height order"
                // Whenever we set a node's height, we use [set_height], which enforces this.
                if height > maxHeightAllowed t then
                    failwith "processing node with higher height than allowed"

            node.HeightInAdjustHeightsHeap <- height
            t.Length <- t.Length + 1
            node.NextInAdjustHeightsHeap <- t.NodesByHeight.[height]
            t.NodesByHeight.[height] <- ValueSome (NodeCrate.make node)

    let removeMinThrowing (t : AdjustHeightsHeap) : NodeCrate =
        if Debug.globalFlag && isEmpty t then
            failwith "removeMin of empty heap"

        let mutable r = t.HeightLowerBound

        while t.NodesByHeight.[r].IsNone do
            r <- r + 1

        let height = r
        t.HeightLowerBound <- height
        let node = t.NodesByHeight.[height].Value
        t.Length <- t.Length - 1

        { new NodeEval<_> with
            member _.Eval node =
                node.HeightInAdjustHeightsHeap <- -1
                t.NodesByHeight.[height] <- node.NextInAdjustHeightsHeap
                node.NextInAdjustHeightsHeap <- ValueNone
                FakeUnit.ofUnit ()
        }
        |> node.Apply
        |> FakeUnit.toUnit

        node

    let setHeight (t : AdjustHeightsHeap) (node : 'a Node) height =
        if height > t.MaxHeightSeen then
            t.MaxHeightSeen <- height

            if height > maxHeightAllowed t then
                failwith "node too large height"

        node.Height <- height

    let ensureHeightRequirement t originalChild originalParent child parent =
        if Debug.globalFlag then
            if not (NodeHelpers.isNecessary child) then
                failwith "expected child to be necessary"

            if not (NodeHelpers.isNecessary parent) then
                failwith "expected parent to be necessary"

        if Node.same parent originalChild then
            failwith "adding edge made graph cyclic"

        if child.Height >= parent.Height then
            addUnlessMem t parent

            // We set [parent.height] after adding [parent] to the heap, so that [parent] goes
            // in the heap with its pre-adjusted height.
            setHeight t parent (child.Height + 1)

    let adjustHeights<'a, 'b>
        (t : AdjustHeightsHeap)
        (recomputeHeap : RecomputeHeap)
        (child : 'a Node)
        (parent : 'b Node)
        =
        if Debug.globalFlag then
            if not (isEmpty t) then
                failwith "expected empty heap in adjustHeights"

            if child.Height < parent.Height then
                failwith "expected child at least as high as parent"

        t.HeightLowerBound <- parent.Height
        ensureHeightRequirement t child parent child parent

        let originalChild = child
        let originalParent = parent

        while t.Length > 0 do
            let child = removeMinThrowing t

            { new NodeEval<FakeUnit> with
                member _.Eval child =
                    if Node.isInRecomputeHeap child then
                        RecomputeHeap.increaseHeight recomputeHeap child

                    if child.NumParents > 0 then
                        let parent = child.Parent0.Value

                        { new NodeEval<_> with
                            member _.Eval parent =
                                ensureHeightRequirement t originalChild originalParent child parent

                                for parentIndex = 1 to child.NumParents - 1 do
                                    let parent = child.Parent1AndBeyond.[parentIndex - 1].Value

                                    { new NodeEval<_> with
                                        member _.Eval parent =
                                            ensureHeightRequirement t originalChild originalParent child parent
                                            |> FakeUnit.ofUnit
                                    }
                                    |> parent.Apply
                                    |> FakeUnit.toUnit

                                FakeUnit.ofUnit ()
                        }
                        |> parent.Apply
                        |> FakeUnit.toUnit

                    match child.Kind with
                    | Kind.BindLhsChange (bind, _) ->
                        { new BindEval<_> with
                            member _.Eval bind =
                                let mutable r = bind.AllNodesCreatedOnRhs

                                while r.IsSome do
                                    let nodeOnRhs = r.Value

                                    { new NodeEval<_> with
                                        member _.Eval nodeOnRhs =
                                            r <- nodeOnRhs.NextNodeInSameScope

                                            if NodeHelpers.isNecessary nodeOnRhs then
                                                ensureHeightRequirement t originalChild originalParent child nodeOnRhs

                                            FakeUnit.ofUnit ()
                                    }
                                    |> nodeOnRhs.Apply
                                    |> FakeUnit.toUnit

                                FakeUnit.ofUnit ()
                        }
                        |> bind.Apply
                    | _ -> FakeUnit.ofUnit ()
            }
            |> child.Apply
            |> FakeUnit.toUnit

        if Debug.globalFlag then
            if not (isEmpty t) then
                failwith "expected empty heap in adjustHeights"

            if originalChild.Height >= originalParent.Height then
                failwith "expected child lower than parent"
