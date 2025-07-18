// [State] defines the global state of which there is one instance for each call to
// [Incremental.Make].
namespace WoofWare.Incremental

open TypeEquality
open System
open WoofWare.TimingWheel

type internal StateStats =
    {
        MaxNumParents : int
        PercentageOfNodesByNumParents : (int * float) list
    }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    let numStabilizes (t : State) =
        StabilizationNum.toInt t.StabilizationNum

    let maxHeightAllowed (t : State) =
        AdjustHeightsHeap.maxHeightAllowed t.AdjustHeightsHeap

    let maxHeightSeen (t : State) =
        AdjustHeightsHeap.maxHeightSeen t.AdjustHeightsHeap

    let iterObservers (t : State) f =
        let mutable r = t.AllObservers

        while r.IsSome do
            let observer = r.Value
            r <- InternalObserverCrate.nextInAll observer
            f observer

    let directlyObserved (t : State) : NodeCrate list =
        let mutable r = []

        iterObservers
            t
            (fun obs ->
                { new InternalObserverEval<_> with
                    member _.Eval obs =
                        r <- NodeCrate.make obs.Observing :: r
                        FakeUnit.ofUnit ()
                }
                |> obs.Apply
                |> FakeUnit.toUnit
            )

        r

    let iterObserverDescendants (t : State) (f : NodeCrate -> unit) : unit =
        NodeCrate.iterDescendants (directlyObserved t) f

    let stats (t : State) : StateStats =
        let mutable maxNumParents = -1
        let mutable numNecessaryNodes = 0

        iterObserverDescendants
            t
            (fun node ->
                numNecessaryNodes <- numNecessaryNodes + 1
                let numParents = NodeCrate.numParents node
                maxNumParents <- max maxNumParents numParents
            )

        let maxNumParents = maxNumParents
        let numNodesByNumParents = Array.zeroCreate (maxNumParents + 1)

        iterObserverDescendants
            t
            (fun node ->
                let numParents = NodeCrate.numParents node
                numNodesByNumParents.[numParents] <- numNodesByNumParents.[numParents] + 1
            )

        let _, percentageOfNodesByNumParents =
            ((0, []), numNodesByNumParents)
            ||> Array.fold (fun (i, acc) numNodes ->
                if numNodes = 0 then
                    i + 1, acc
                else
                    let elt = i, float numNodes / float numNecessaryNodes
                    i + 1, elt :: acc
            )

        {
            MaxNumParents = maxNumParents
            PercentageOfNodesByNumParents = List.rev percentageOfNodesByNumParents
        }

    let amStabilizing (t : State) : bool =
        match t.Status with
        | Status.Running_on_update_handlers
        | Status.Stabilizing -> true
        | Status.Not_stabilizing -> false
        | Status.Stabilize_previously_raised raised_exn ->
            failwith "TODO: reraise exception: 'cannot call am_stabilizing -- stabilize previously raised'"

    let invariant (t : State) : unit =
        match t.Status with
        | Status.Stabilize_previously_raised _ -> ()
        | Status.Running_on_update_handlers
        | Status.Stabilizing
        | Status.Not_stabilizing ->
            iterObservers
                t
                (fun obs ->
                    { new InternalObserverEval<_> with
                        member _.Eval obs =
                            match obs.State with
                            | InternalObserverState.InUse
                            | InternalObserverState.Disallowed -> ()
                            | InternalObserverState.Created
                            | InternalObserverState.Unlinked -> failwith "member of allObservers with unexpected state"

                            InternalObserver.invariant ignore obs
                            FakeUnit.ofUnit ()
                    }
                    |> obs.Apply
                    |> FakeUnit.toUnit
                )

            iterObserverDescendants
                t
                (fun node ->
                    { new NodeEval<_> with
                        member _.Eval node =
                            Node.invariant ignore node

                            if not (amStabilizing t) then
                                if node.OldValueOpt.IsSome then
                                    failwith "invariant failed"

                            if node.Height > AdjustHeightsHeap.maxHeightSeen t.AdjustHeightsHeap then
                                failwith "invariant failed"

                            FakeUnit.ofUnit ()
                    }
                    |> node.Apply
                    |> FakeUnit.toUnit
                )

            if
                AdjustHeightsHeap.maxHeightAllowed t.AdjustHeightsHeap
                <> RecomputeHeap.maxHeightAllowed t.RecomputeHeap
            then
                failwith "invariant failed"

            StabilizationNum.invariant t.StabilizationNum

            if not (Object.ReferenceEquals (t.CurrentScope, Scope.top)) then
                failwith "invariant failed"

            RecomputeHeap.invariant t.RecomputeHeap

            do
                if AdjustHeightsHeap.length t.AdjustHeightsHeap <> 0 then
                    failwith "invariant failed"

                AdjustHeightsHeap.invariant t.AdjustHeightsHeap

            if not (Stack.isEmpty t.PropagateInvalidity) then
                failwith "invariant failed"

            if t.NumActiveObservers < 0 then
                failwith "invariant failed"

            do
                t.NewObservers
                |> Stack.invariant (fun packed ->
                    InternalObserverCrate.invariant packed

                    match InternalObserverCrate.state packed with
                    | InternalObserverState.InUse
                    | InternalObserverState.Disallowed ->
                        // When an observer is added to [new_observers], it has [state = Created].
                        // The only possible transitions from there are to [Unlinked] or to
                        // [In_use], which also removes it from [new_observers], never to be added
                        // again.  Thus it is impossible for an observer in [new_observers] to be
                        // [In_use] or [Disallowed].
                        failwith "invariant failed"
                    | InternalObserverState.Created
                    | InternalObserverState.Unlinked -> ()
                )

            do
                t.DisallowedObservers
                |> Stack.invariant (fun packed ->
                    InternalObserverCrate.invariant packed

                    match InternalObserverCrate.state packed with
                    | InternalObserverState.Disallowed -> ()
                    | _ -> failwith "invariant failed"
                )

            do
                match t.Status with
                | Status.Stabilize_previously_raised _ -> failwith "invariant failed"
                | Status.Running_on_update_handlers
                | Status.Not_stabilizing ->
                    if not (Stack.isEmpty t.SetDuringStabilization) then
                        failwith "invariant failed"
                | Status.Stabilizing ->
                    t.SetDuringStabilization
                    |> Stack.invariant (fun v ->
                        { new VarEval<_> with
                            member _.Eval var =
                                if var.ValueSetDuringStabilization.IsNone then
                                    failwith "invariant failed"

                                FakeUnit.ofUnit ()
                        }
                        |> v.Apply
                        |> FakeUnit.toUnit
                    )

            Stack.invariant NodeCrate.invariant t.HandleAfterStabilization
            Stack.invariant RunOnUpdateHandlers.invariant t.RunOnUpdateHandlers
            OnlyInDebug.invariant t.OnlyInDebug

    let ensureNotStabilizing t name allowInUpdateHandler =
        match t.Status with
        | Status.Not_stabilizing -> ()
        | Status.Running_on_update_handlers ->
            if not allowInUpdateHandler then
                failwith $"cannot %s{name} during on-update handlers"
        | Status.Stabilize_previously_raised raised ->
            RaisedException.reraiseWithMessage raised $"cannot %s{name} -- stabilize previously raised"
        | Status.Stabilizing -> failwith $"cannot %s{name} during stabilization"

    let setHeight (node : 'a Node) (height : int) : unit =
        let t = node.State
        AdjustHeightsHeap.setHeight t.AdjustHeightsHeap node height

    let setMaxHeightAllowed (t : State) (height : int) : unit =
        ensureNotStabilizing t "set_max_height_allowed" true
        AdjustHeightsHeap.setMaxHeightAllowed t.AdjustHeightsHeap height
        RecomputeHeap.setMaxHeightAllowed t.RecomputeHeap height

    let handleAfterStabilization<'a> (node : 'a Node) : unit =
        if not node.IsInHandleAfterStabilization then
            let t = node.State
            node.IsInHandleAfterStabilization <- true
            Stack.push (NodeCrate.make node) t.HandleAfterStabilization

    let rec removeChildren<'a> (parent : 'a Node) : unit =
        Node.iteriChildren
            parent
            (fun childIndex child ->
                { new NodeEval<_> with
                    member _.Eval child =
                        removeChild child parent childIndex |> FakeUnit.ofUnit
                }
                |> child.Apply
                |> FakeUnit.toUnit
            )

    and removeChild<'a, 'b> (child : 'b Node) (parent : 'a Node) (childIndex : int) : unit =
        Node.removeParent child parent childIndex
        checkIfUnnecessary child

    and checkIfUnnecessary<'a> (node : 'a Node) : unit =
        if not (NodeHelpers.isNecessary node) then
            becameUnnecessary node

    and becameUnnecessary<'a> (node : 'a Node) : unit =
        let t = node.State
        t.NumNodesBecameUnnecessary <- t.NumNodesBecameUnnecessary + 1

        if node.NumOnUpdateHandlers > 0 then
            handleAfterStabilization node

        node.Height <- -1
        removeChildren node

        match node.Kind with
        | Kind.UnorderedArrayFold u ->
            { new UnorderedArrayFoldEval<_, _> with
                member _.Eval u =
                    UnorderedArrayFold.forceFullCompute u |> FakeUnit.ofUnit
            }
            |> u.Apply
            |> FakeUnit.toUnit
        | Kind.Expert p -> Expert.observabilityChange p false
        | _ -> ()

        if Debug.globalFlag then
            assert (not (Node.needsToBeComputed node))

        if Node.isInRecomputeHeap node then
            RecomputeHeap.remove t.RecomputeHeap node

    let removeAlarm (clock : Clock) (alarm : TimingWheel.Alarm) : unit =
        if TimingWheel.mem clock.TimingWheel alarm then
            TimingWheel.remove clock.TimingWheel alarm

    (* An invalid node is node whose kind is [Invalid].  A node's kind is set to [Invalid]
       when the lhs of its scope changes, or one if its children propagate the invalidity
       upward (see [Node.should_be_invalidated] to see in which case invalidity propagation
       stops).  Invalidating a node disconnects it from its children, which means:

       1. an invalid node cannot end up on the scheduler (if it is on the scheduler when
       it is invalidated, it is removed)
       2. an invalid node doesn't make its children necessary anymore.

       Invalid nodes usually have no parents, because the upward invalidity propagation means
       that their parents will themselves become invalid and disconnect from their children.
       However, [if], [join] or [bind] are not invalidated by the upward propagation, so an
       invalid node can still have parents.  Invalid nodes can be necessary, in the case where
       they have parents, and also when they are observed.

       The upward propagation of invalidity happens both when a node becomes invalid, and when
       trying to add an edge from an invalid child node to another node.  Because invalidity
       is only propagated upward, and because the rhs of a bind is invalidated before it
       executes, a node cannot be both computed and invalidated in the same stabilization.

       When invalidating, we can't assume much about the nodes we visit.  We cannot assume
       that nodes are valid (the rhs can contain previously invalidated nodes), or that nodes
       are unnecessary (nodes can be made necessary without going through their containing
       binds). *)

    let rec invalidateNode<'a> (node : 'a Node) : unit =
        if not (NodeHelpers.isValid node) then
            ()
        else
            let t = node.State

            if node.NumOnUpdateHandlers > 0 then
                handleAfterStabilization node

            node.ValueOpt <- ValueNone

            if Debug.globalFlag then
                assert node.OldValueOpt.IsNone

            node.ChangedAt <- t.StabilizationNum
            node.RecomputedAt <- t.StabilizationNum
            t.NumNodesInvalidated <- t.NumNodesInvalidated + 1

            if NodeHelpers.isNecessary node then
                removeChildren node
                // The node doesn't have children any more, so we can lower its height as much as
                // possible, to one greater than the scope it was created in.  Also, because we
                // are lowering the height, we don't need to adjust any of its ancestors' heights.
                // We could leave the height alone, but we may as well lower it as much as
                // possible to avoid making the heights of any future ancestors unnecessarily
                // large.
                node.Height <- Scope.height node.CreatedIn + 1
            // We don't set [node.created_in] or [node.next_node_in_same_scope]; we leave [node]
            // in the scope it was created in.  If that scope is ever invalidated, then that
            // will clear [node.next_node_in_same_scope]
            match node.Kind with
            | Kind.At (at, _) -> removeAlarm at.Clock at.Alarm
            | Kind.AtIntervals (atIntervals, _) -> removeAlarm atIntervals.Clock atIntervals.Alarm
            | Kind.BindMain bind ->
                { new BindMainEval<_, _> with
                    member _.Eval bind =
                        invalidateNodesCreatedOnRhs bind.AllNodesCreatedOnRhs |> FakeUnit.ofUnit
                }
                |> bind.Apply
                |> FakeUnit.toUnit
            | Kind.StepFunction sf -> removeAlarm sf.Clock sf.Alarm
            | _ -> ()

            Node.setKind node Kind.Invalid
            (* If we called [propagate_invalidity] right away on the parents, we would get into
           trouble.  The parent would disconnect itself from the current node, thus
           modifying the list of parents we iterate on.  Even if we made a special case, it
           still wouldn't be enough to handle other cases where the list of parents is
           modified (e.g. when [lhs] is invalidated in the example in the comment about
           [can_recompute_now] far below). *)
            for index = 0 to node.NumParents - 1 do
                Stack.push (Node.getParent node index) t.PropagateInvalidity

            if Debug.globalFlag then
                assert (not (Node.needsToBeComputed node))

            if Node.isInRecomputeHeap node then
                RecomputeHeap.remove t.RecomputeHeap node

    and invalidateNodesCreatedOnRhs (node : NodeCrate voption) : unit =
        let mutable r = node

        while r.IsSome do
            { new NodeEval<_> with
                member _.Eval nodeOnRhs =
                    r <- nodeOnRhs.NextNodeInSameScope
                    nodeOnRhs.NextNodeInSameScope <- ValueNone
                    invalidateNode nodeOnRhs
                    FakeUnit.ofUnit ()
            }
            |> r.Value.Apply
            |> FakeUnit.toUnit

    (* When [not t.bind_lhs_change_should_invalidate_rhs] and a bind's lhs changes, we move
       nodes created on the bind's rhs up to its parent bind, as opposed to [Scope.Top].  This
       maintains their dependence on valid bind left-hand sides, and keeps them higher in the
       graph.  This in turn means that we will continue to compute those nodes after the
       parent bind's lhs, which gives them more of a chance to become unnecessary and not be
       computed should the parent bind's lhs change. *)
    let rescopeNodesCreatedOnRhs (_t : State) (firstNodeOnRhs : NodeCrate voption) (newScope : Scope) =
        let mutable r = firstNodeOnRhs

        while r.IsSome do
            { new NodeEval<_> with
                member _.Eval nodeOnRhs =
                    r <- nodeOnRhs.NextNodeInSameScope
                    nodeOnRhs.NextNodeInSameScope <- ValueNone
                    nodeOnRhs.CreatedIn <- newScope
                    Scope.addNode newScope nodeOnRhs
                    FakeUnit.ofUnit ()
            }
            |> r.Value.Apply
            |> FakeUnit.toUnit

    let propagateInvalidity t : unit =
        let mutable node = None

        while (node <- Stack.pop t.PropagateInvalidity
               node.IsSome) do
            { new NodeEval<_> with
                member _.Eval node =
                    if NodeHelpers.isValid node then
                        invalidateNode node |> FakeUnit.ofUnit
                    else
                    // [Node.needs_to_be_computed node] is true because
                    // - node is necessary. This is because children can only point to necessary parents
                    // - node is stale. This is because: For bind, if, join, this is true because
                    // - either the invalidation is caused by the lhs changing (in which case the
                    //   lhs-change node being newer makes us stale).
                    // - or a child became invalid this stabilization cycle, in which case it has
                    //   t.changed_at of [t.stabilization_num], and so [node] is stale
                    // - or [node] just became necessary and tried connecting to an already invalid
                    //   child. In that case, [child.changed_at > node.recomputed_at] for that child,
                    //   because if we had been recomputed when that child changed, we would have been
                    //   made invalid back then.  For expert nodes, the argument is the same, except
                    //   that instead of lhs-change nodes make the expert nodes stale, it's made stale
                    //   explicitly when adding or removing children.
                    if Debug.globalFlag then
                        assert (Node.needsToBeComputed node)

                    match node.Kind with
                    | Kind.Expert expert ->
                        // If multiple children are invalid, they will push us as many times on the
                        // propagation stack, so we count them right.
                        Expert.incrInvalidChildren expert
                    | kind ->
                        if Debug.globalFlag then
                            match kind with
                            | Kind.BindMain _
                            | Kind.IfThenElse _
                            | Kind.JoinMain _ -> ()
                            | _ -> failwith "nodes with no children are never pushed on the stack"
                    // We do not check [Node.needs_to_be_computed node] here, because it should be
                    // true, and because computing it takes O(number of children), node can be pushed
                    // on the stack once per child, and expert nodes can have lots of children.
                    if not (Node.isInRecomputeHeap node) then
                        RecomputeHeap.add t.RecomputeHeap node

                    FakeUnit.ofUnit ()
            }
            |> node.Value.Apply
            |> FakeUnit.toUnit

    /// [add_parent_without_adjusting_heights t ~child ~parent] adds [parent] as a parent of
    /// [child], and makes [child] and all its descendants necessary, ensuring their heights
    /// are accurate.  There is no guarantee about the relative heights of [child] and [parent]
    /// though.
    let rec addParentWithoutAdjustingHeights<'a, 'b> (child : 'a Node) (parent : 'b Node) (childIndex : int) : unit =
        if Debug.globalFlag then
            assert (NodeHelpers.isNecessary parent)

        let t = child.State
        let wasNecessary = NodeHelpers.isNecessary child
        Node.addParent child parent childIndex

        if not (NodeHelpers.isValid child) then
            Stack.push (NodeCrate.make parent) t.PropagateInvalidity

        if not wasNecessary then
            becameNecessary' child

        match parent.Kind with
        | Kind.Expert e -> Expert.runEdgeCallback e childIndex
        | _ -> ()

    and becameNecessary'<'a> (node : 'a Node) : unit =
        // [Scope.is_necessary node.created_in] is true (assuming the scope itself is valid)
        // because [Node.iter_children] below first visits the lhs-change of bind nodes and
        // then the rhs.
        if NodeHelpers.isValid node && not (Scope.isNecessary node.CreatedIn) then
            failwith "Trying to make a node necessary whose defining bind is not necessary"

        let t = node.State
        t.NumNodesBecameNecessary <- t.NumNodesBecameNecessary + 1

        if node.NumOnUpdateHandlers > 0 then
            handleAfterStabilization node
        // Since [node] became necessary, to restore the invariant, we need to:

        // - add parent pointers to [node] from its children.
        // - set [node]'s height.
        // - add [node] to the recompute heap, if necessary.
        setHeight node (Scope.height node.CreatedIn + 1)

        Node.iteriChildren
            node
            (fun childIndex child ->
                { new NodeEval<_> with
                    member _.Eval child =
                        addParentWithoutAdjustingHeights child node childIndex
                        // Now that child is necessary, it should have a valid height.
                        if Debug.globalFlag then
                            assert (child.Height >= 0)

                        if child.Height >= node.Height then
                            setHeight node (child.Height + 1)

                        FakeUnit.ofUnit ()
                }
                |> child.Apply
                |> FakeUnit.toUnit
            )
        // Now that the height is correct, maybe add [node] to the recompute heap.  [node]
        // just became necessary, so it can't have been in the recompute heap.  Since [node]
        // is necessary, we should add it to the recompute heap iff it is stale.
        if Debug.globalFlag then
            assert (not (Node.isInRecomputeHeap node))
            assert (NodeHelpers.isNecessary node)

        if Node.isStale node then
            RecomputeHeap.add t.RecomputeHeap node

        match node.Kind with
        | Kind.Expert p -> Expert.observabilityChange p true
        | _ -> ()

    let becameNecessary node =
        becameNecessary' node
        propagateInvalidity node.State

    let addParent (child : Node<'a>) (parent : Node<'b>) (childIndex : int) : unit =
        if Debug.globalFlag then
            assert (NodeHelpers.isNecessary parent)

        let t = parent.State
        // In the case when the edge being added creates a cycle, it is possible for the
        // recursion in [add_parent_without_adjusting_heights] to reach [parent] as a descendant
        // of [child].  In that case, the recursion terminates, because [Node.is_necessary
        // parent].  We then return here and subsequently detect the cycle in
        // [adjust_heights].
        addParentWithoutAdjustingHeights child parent childIndex
        // We adjust heights so that we ensure there are no cycles before calling
        // [propagate_invalidity].
        if child.Height >= parent.Height then
            AdjustHeightsHeap.adjustHeights t.AdjustHeightsHeap t.RecomputeHeap child parent

        propagateInvalidity t

        if Debug.globalFlag then
            assert (NodeHelpers.isNecessary parent)
        // we only add necessary parents
        if
            (not (Node.isInRecomputeHeap parent))
            && (StabilizationNum.isNone parent.RecomputedAt || Node.edgeIsStale child parent)
        then
            RecomputeHeap.add t.RecomputeHeap parent

    let runWithScope (t : State) (scope : Scope) (f : unit -> 'a) : 'a =
        let saved = t.CurrentScope
        t.CurrentScope <- scope

        try
            f ()
        finally
            t.CurrentScope <- saved

    let withinScope (t : State) (scope : Scope) (f : unit -> 'a) : 'a =
        if not (Scope.isValid scope) then
            failwith "attempt to run within an invalid scope"

        runWithScope t scope f

    let changeChild<'a, 'b>
        (parent : 'a Node)
        (oldChild : 'b Node voption)
        (newChild : 'b Node)
        (childIndex : int)
        : unit
        =
        match oldChild with
        | ValueNone -> addParent newChild parent childIndex
        | ValueSome oldChild ->
            if not (Object.ReferenceEquals (oldChild, newChild)) then
                // We remove [old_child] before adding [new_child], because they share the same child index.
                Node.removeParent oldChild parent childIndex
                // We force [old_child] to temporarily be necessary so that [add_parent] can't
                // mistakenly think it is unnecessary and transition it to necessary (which would
                // add duplicate edges and break things horribly).
                oldChild.ForceNecessary <- true
                addParent newChild parent childIndex
                oldChild.ForceNecessary <- false
                // We [check_if_unnecessary] after [add_parent], so that we don't unnecessarily
                // transition nodes from necessary to unnecessary and then back again.
                checkIfUnnecessary oldChild

    let addAlarm (clock : Clock) (at : TimeNs) alarmValue =
        if Debug.globalFlag then
            assert (at > Clock.now clock)

        TimingWheel.add clock.TimingWheel at alarmValue

    let rec recompute<'a> (node : 'a Node) : unit =
        let t = node.State

        if Debug.globalFlag then
            t.OnlyInDebug.CurrentlyRunningNode <- Some (NodeCrate.make node)
            t.OnlyInDebug.ExpertNodesCreatedByCurrentNode <- []

        t.NumNodesRecomputed <- t.NumNodesRecomputed + 1
        node.RecomputedAt <- t.StabilizationNum

        match node.Kind with
        | Kind.ArrayFold arrayFold ->
            { new ArrayFoldEval<_, _> with
                member _.Eval arrayFold =
                    maybeChangeValue node (ArrayFold.compute arrayFold) |> FakeUnit.ofUnit
            }
            |> arrayFold.Apply
            |> FakeUnit.toUnit
        | Kind.At (at, teq) ->
            // It is a bug if we try to compute an [At] node after [at].  [advance_clock] was
            // supposed to convert it to a [Const] at the appropriate time.
            if Debug.globalFlag then
                assert (at.At > (Clock.now at.Clock))

            maybeChangeValue node (Teq.castFrom teq BeforeOrAfter.Before)
        | Kind.AtIntervals (_, teq) -> maybeChangeValue node (Teq.castFrom teq ())
        | Kind.BindLhsChange (bind, teq) ->
            { new BindEval<_> with
                member _.Eval bind =
                    let oldRhs = bind.Rhs
                    let oldAllNodesCreatedOnRhs = bind.AllNodesCreatedOnRhs
                    // We clear [all_nodes_created_on_rhs] so it will hold just the nodes created by
                    // this call to [f].
                    bind.AllNodesCreatedOnRhs <- ValueNone

                    let rhs =
                        runWithScope t bind.RhsScope (fun () -> bind.F (Node.valueThrowing bind.Lhs))

                    bind.Rhs <- ValueSome rhs
                    // Anticipate what [maybe_change_value] will do, to make sure Bind_main is stale
                    // right away. This way, if the new child is invalid, we'll satisfy the invariant
                    // saying that [needs_to_be_computed bind_main] in [propagate_invalidity]
                    node.ChangedAt <- t.StabilizationNum
                    changeChild bind.Main oldRhs rhs Kind.bindRhsChildIndex

                    if oldRhs.IsSome then
                        // We invalidate after [change_child], because invalidation changes the [kind] of
                        // nodes to [Invalid], which means that we can no longer visit their children.
                        // Also, the [old_rhs] nodes are typically made unnecessary by [change_child], and
                        // so by invalidating afterwards, we will not waste time adding them to the
                        // recompute heap and then removing them.
                        if t.BindLhsChangeShouldInvalidateRhs then
                            invalidateNodesCreatedOnRhs oldAllNodesCreatedOnRhs
                        else
                            rescopeNodesCreatedOnRhs t oldAllNodesCreatedOnRhs bind.Main.CreatedIn

                        propagateInvalidity t
                    (* [node] was valid at the start of the [Bind_lhs_change] branch, and invalidation
                      only visits higher nodes, so [node] is still valid. *)
                    if Debug.globalFlag then
                        assert (NodeHelpers.isValid node)

                    maybeChangeValue node (Teq.castFrom teq ())
                    FakeUnit.ofUnit ()
            }
            |> bind.Apply
            |> FakeUnit.toUnit
        | Kind.BindMain bind ->
            { new BindMainEval<'a, FakeUnit> with
                member _.Eval<'b> (bind : Bind<'a, 'b>) : FakeUnit =
                    copyChild<'b> node bind.Rhs.Value |> FakeUnit.ofUnit
            }
            |> bind.Apply
            |> FakeUnit.toUnit
        | Kind.Const a -> maybeChangeValue node a
        | Kind.Freeze freeze ->
            let value = Node.valueThrowing freeze.Child

            if freeze.OnlyFreezeWhen value then
                removeChildren node
                Node.setKind node (Kind.Const value)

                if NodeHelpers.isNecessary node then
                    setHeight node 0
                else
                    becameUnnecessary node

            maybeChangeValue node value
        | Kind.IfTestChange (ifTestChange, teq) ->
            { new IfThenElseEval<_> with
                member _.Eval ifTestChange =
                    let main = ifTestChange.Main
                    let currentBranch = ifTestChange.CurrentBranch

                    let desiredBranch =
                        if Node.valueThrowing ifTestChange.Test then
                            ifTestChange.Then
                        else
                            ifTestChange.Else

                    ifTestChange.CurrentBranch <- ValueSome desiredBranch
                    // see the comment in BindLhsChange
                    node.ChangedAt <- t.StabilizationNum
                    changeChild main currentBranch desiredBranch Kind.ifBranchChildIndex

                    maybeChangeValue node (Teq.castFrom teq ())
                    FakeUnit.ofUnit ()
            }
            |> ifTestChange.Apply
            |> FakeUnit.toUnit
        | Kind.IfThenElse ite -> copyChild node ite.CurrentBranch.Value
        | Kind.Invalid -> failwith "We never have invalid nodes in the recompute heap; they are never stale."
        | Kind.JoinLhsChange (join, teq) ->
            { new JoinEval<_> with
                member _.Eval join =
                    let lhs = join.Lhs
                    let main = join.Main
                    let oldRhs = join.Rhs
                    let rhs = Node.valueThrowing lhs
                    join.Rhs <- ValueSome rhs
                    // see the comment in BindLhsChange
                    node.ChangedAt <- t.StabilizationNum
                    changeChild main oldRhs rhs Kind.joinRhsChildIndex
                    maybeChangeValue node (Teq.castFrom teq ())
                    FakeUnit.ofUnit ()
            }
            |> join.Apply
            |> FakeUnit.toUnit
        | Kind.JoinMain join -> copyChild node join.Rhs.Value
        | Kind.Map map ->
            { new MapEval<_, _> with
                member _.Eval (f, n1) =
                    maybeChangeValue node (f (Node.valueThrowing n1)) |> FakeUnit.ofUnit
            }
            |> map.Apply
            |> FakeUnit.toUnit
        | Kind.Snapshot snap ->
            // It is a bug if we try to compute a [Snapshot] and the alarm should have fired.
            // [advance_clock] was supposed to convert it to a [Freeze] at the appropriate
            // time.
            if Debug.globalFlag then
                assert (snap.At > (Clock.now snap.Clock))

            maybeChangeValue node snap.Before
        | Kind.StepFunction stepFunctionNode ->
            let clock = stepFunctionNode.Clock
            let child = stepFunctionNode.Child

            match stepFunctionNode.Child with
            | ValueNone -> ()
            | ValueSome child ->
                if child.ChangedAt > stepFunctionNode.ExtractedStepFunctionFromChildAt then
                    stepFunctionNode.ExtractedStepFunctionFromChildAt <- child.ChangedAt
                    removeAlarm clock stepFunctionNode.Alarm
                    let stepFunction = Node.valueThrowing child
                    stepFunctionNode.Value <- ValueSome (StepFunction.init stepFunction)
                    stepFunctionNode.UpcomingSteps <- StepFunction.steps stepFunction
                    (* If the child is a constant, we drop our reference to it, to avoid holding on to
               the entire step function. *)
                    if Node.isConst child then
                        removeChildren node
                        stepFunctionNode.Child <- ValueNone
                        setHeight node (Scope.height node.CreatedIn + 1)

            StepFunctionNode.advance stepFunctionNode (Clock.now clock)
            let stepFunctionValue = stepFunctionNode.Value.Value

            match Sequence.hd stepFunctionNode.UpcomingSteps with
            | None ->
                if child.IsNone then
                    Node.setKind node (Kind.Const stepFunctionValue)
            | Some (at, _) -> stepFunctionNode.Alarm <- addAlarm clock at stepFunctionNode.AlarmValue

            maybeChangeValue node stepFunctionValue
        | Kind.UnorderedArrayFold u ->
            { new UnorderedArrayFoldEval<_, _> with
                member _.Eval u =
                    maybeChangeValue node (UnorderedArrayFold.compute u) |> FakeUnit.ofUnit
            }
            |> u.Apply
            |> FakeUnit.toUnit
        | Kind.Uninitialized -> failwith "expected initialised"
        | Kind.Var var -> maybeChangeValue node var.Value
        | Kind.Map2 map2 ->
            { new Map2Eval<_, _> with
                member _.Eval (f, n1, n2) =
                    maybeChangeValue node (f (Node.valueThrowing n1) (Node.valueThrowing n2))
                    |> FakeUnit.ofUnit
            }
            |> map2.Apply
            |> FakeUnit.toUnit
        | Kind.Expert expert ->
            match Expert.beforeMainComputation expert with
            | BeforeMainComputationResult.Invalid ->
                invalidateNode node
                propagateInvalidity t
            | BeforeMainComputationResult.Ok -> maybeChangeValue node (expert.F ())

    and copyChild<'a> (parent : 'a Node) (child : 'a Node) : unit =
        if NodeHelpers.isValid child then
            maybeChangeValue parent (Node.valueThrowing child)
        else
            invalidateNode parent
            propagateInvalidity parent.State

    and maybeChangeValue<'a> (node : 'a Node) (newValue : 'a) : unit =
        let t = node.State
        let oldValueOpt = node.ValueOpt

        if
            oldValueOpt.IsNone
            || not (Cutoff.shouldCutoff node.Cutoff oldValueOpt.Value newValue)
        then
            node.ValueOpt <- ValueSome newValue
            node.ChangedAt <- t.StabilizationNum
            t.NumNodesChanged <- t.NumNodesChanged + 1

            if node.NumOnUpdateHandlers > 0 then
                node.OldValueOpt <- oldValueOpt
                handleAfterStabilization node

            if node.NumParents >= 1 then
                for parentIndex = 1 to node.NumParents - 1 do
                    let parent = node.Parent1AndBeyond.[parentIndex - 1].Value

                    { new NodeEval<_> with
                        member _.Eval parent =
                            match parent.Kind with
                            | Kind.Expert expert ->
                                let child_index = node.MyChildIndexInParentAtIndex.[parentIndex] in
                                Expert.runEdgeCallback childIndex expert
                            | Kind.Unordered_array_fold u ->
                                UnorderedArrayFold.childChanged
                                    u
                                    node
                                    node.MyChildIndexInParentAtIndex.[parentIndex]
                                    oldValueOpt
                                    newValue
                            | _ -> ()

                            if Debug.globalFlag then
                                assert (Node.needsToBeComputed parent)
                            (* We don't do the [can_recompute_now] optimization.  Since most nodes only have
                       one parent, it is not probably not a big loss.  If we did it anyway, we'd
                       have to be careful, because while we iterate over the list of parents, we
                       would execute them, and in particular we can execute lhs-change nodes who can
                       change the structure of the list of parents we iterate on.  Think about:

                       {[
                         lhs >>= fun b -> if b then lhs >>| Fn.id else const b
                       ]}

                       If the optimization kicks in when we propagate change to the parents of [lhs]
                       (which changes from [true] to [false]), we could execute the [lhs-change]
                       first, which would make disconnect the [map] node from [lhs].  And then we
                       would execute the second child of the [lhs], which doesn't exist anymore and
                       incremental would segfault (there may be a less naive way of making this work
                       though). *)
                            if not (Node.isInRecomputeHeap parent) then
                                RecomputeHeap.add t.RecomputeHeap parent

                            FakeUnit.ofUnit ()
                    }
                    |> parent.Apply
                    |> FakeUnit.toUnit

                { new NodeEval<_> with
                    member _.Eval parent =
                        match parent.Kind with
                        | Kind.Expert p ->
                            let child_index = node.MyChildIndexInParentAtIndex.[0]
                            Expert.runEdgeCallback childIndex p
                        | Kind.UnorderedArrayFold u ->
                            UnorderedArrayFold.childChanged
                                u
                                node
                                node.MyChildIndexInParentAtIndex.[0]
                                old_value_opt
                                new_value
                        | _ -> ()

                        if Debug.globalFlag then
                            assert (Node.needsToBeComputed parent)

                        if not (Node.isInRecomputeHeap parent) then
                            let canRecomputeNow =
                                match parent.Kind with
                                | Kind.Uninitialized -> failwith "expected initialised"
                                | Kind.At _
                                | Kind.AtIntervals _
                                | Kind.Const _
                                | Invalid
                                | Snapshot _
                                | Var _ -> failwith "these nodes aren't parents"
                                (* These nodes have more than one child. *)
                                | Kind.ArrayFold _
                                | Kind.Map2 _
                                | Kind.UnorderedArrayFold _
                                | Kind.Expert _ -> failwith "these nodes have more than one child"
                                (* We can immediately recompute [parent] if no other node needs to be stable
                         before computing it.  If [parent] has a single child (i.e. [node]), then
                         this amounts to checking that [parent] won't be invalidated, i.e. that
                         [parent]'s scope has already stabilized. *)
                                | Kind.BindLhsChange _ -> node.Height > Scope.height parent.CreatedIn
                                | Kind.Freeze _ -> node.Height > Scope.height parent.CreatedIn
                                | Kind.IfTestChange _ -> node.Height > Scope.height parent.CreatedIn
                                | Kind.JoinLhsChange _ -> node.Height > Scope.height parent.CreatedIn
                                | Kind.Map _ -> node.Height > Scope.height parent.CreatedIn
                                | Kind.StepFunction _ -> node.Height > Scope.height parent.CreatedIn
                                (* For these, we need to check that the "_change" child has already been
                         evaluated (if needed).  If so, this also implies:

                         {[
                           node.height > Scope.height parent.created_in
                         ]} *)
                                | Kind.BindMain b -> node.height > b.lhs_change.height
                                | Kind.IfThenElse i -> node.height > i.test_change.height
                                | Kind.JoinMain j -> node.height > j.lhs_change.height

                            if canRecomputeNow then
                                t.NumNodesRecomputedDirectlyBecauseOneChild <-
                                    t.NumNodesRecomputedDirectlyBecauseOneChild + 1

                                recompute parent
                            else if parent.Height <= RecomputeHeap.minHeight t.RecomputeHeap then
                                (* If [parent.height] is [<=] the height of all nodes in the recompute heap
                         (possibly because the recompute heap is empty), then we can recompute
                         [parent] immediately and save adding it to and then removing it from the
                         recompute heap. *)
                                t.NumNodesRecomputedDirectlyBecauseMinHeight <-
                                    t.NumNodesRecomputedDirectlyBecauseMinHeight + 1

                                recompute parent
                            else
                                if Debug.globalFlag then
                                    assert (Node.needsToBeComputed parent)
                                    assert (not (Node.isInRecomputeHeap parent))

                                RecomputeHeap.add t.RecomputeHeap parent
                }
                |> node.Parent0.Value.Apply
                |> FakeUnit.toUnit

        if Debug.globalFlag then
            invariant t

    let[@inline always] recompute_first_node_that_is_necessary r =
      let (T node) = Recompute_heap.remove_min r in
      if debug && not (Node.needs_to_be_computed node)
      then
        failwiths "node unexpectedly does not need to be computed" node [%sexp_of: _ Node.t];
      recompute node
    ;;

    let unlink_disallowed_observers t =
      while Stack.length t.disallowed_observers > 0 do
        let packed = Stack.pop_exn t.disallowed_observers in
        let (T internal_observer) = packed in
        if debug
        then
          assert (
            match internal_observer.state with
            | Disallowed -> true
            | _ -> false);
        internal_observer.state <- Unlinked;
        let (T all_observers) = Uopt.value_exn t.all_observers in
        if Internal_observer.same internal_observer all_observers
        then t.all_observers <- internal_observer.next_in_all;
        Internal_observer.unlink internal_observer;
        check_if_unnecessary internal_observer.observing
      done
    ;;

    let disallow_future_use internal_observer =
      let t = Internal_observer.incr_state internal_observer in
      match internal_observer.state with
      | Disallowed | Unlinked -> ()
      | Created ->
        t.num_active_observers <- t.num_active_observers - 1;
        internal_observer.state <- Unlinked;
        internal_observer.on_update_handlers <- []
      | In_use ->
        t.num_active_observers <- t.num_active_observers - 1;
        internal_observer.state <- Disallowed;
        Stack.push t.disallowed_observers (T internal_observer)
    ;;

    let disallow_finalized_observers t =
      let disallow_if_finalized (Internal_observer.Packed.T internal_observer) =
        if List.is_empty internal_observer.on_update_handlers
        then disallow_future_use internal_observer
      in
      Thread_safe_queue.dequeue_until_empty
        ~f:disallow_if_finalized
        t.finalized_observers [@nontail]
    ;;

    let observer_finalizer t =
      stage (fun observer ->
        let internal_observer = !observer in
        Thread_safe_queue.enqueue t.finalized_observers (T internal_observer))
    ;;

    let create_observer ?(should_finalize = true) (observing : _ Node.t) =
      let t = observing.state in
      let internal_observer : _ Internal_observer.t =
        { state = Created
        ; observing
        ; on_update_handlers = []
        ; prev_in_all = Uopt.none
        ; next_in_all = Uopt.none
        ; prev_in_observing = Uopt.none
        ; next_in_observing = Uopt.none
        }
      in
      Stack.push t.new_observers (T internal_observer);
      let observer = ref internal_observer in
      if should_finalize
      then Gc.Expert.add_finalizer_ignore observer (unstage (observer_finalizer t));
      t.num_active_observers <- t.num_active_observers + 1;
      observer
    ;;

    let add_new_observers t =
      while Stack.length t.new_observers > 0 do
        let packed = Stack.pop_exn t.new_observers in
        let module Packed = Internal_observer.Packed in
        let (T internal_observer) = packed in
        match internal_observer.state with
        | In_use | Disallowed -> assert false
        | Unlinked -> ()
        | Created ->
          internal_observer.state <- In_use;
          let old_all_observers = t.all_observers in
          if Uopt.is_some old_all_observers
          then (
            internal_observer.next_in_all <- old_all_observers;
            Packed.set_prev_in_all (Uopt.unsafe_value old_all_observers) (Uopt.some packed));
          t.all_observers <- Uopt.some packed;
          let observing = internal_observer.observing in
          let was_necessary = Node.is_necessary observing in
          observing.num_on_update_handlers
          <- observing.num_on_update_handlers
             + List.length internal_observer.on_update_handlers;
          let old_observers = observing.observers in
          if Uopt.is_some old_observers
          then (
            internal_observer.next_in_observing <- old_observers;
            (Uopt.unsafe_value old_observers).prev_in_observing <- Uopt.some internal_observer);
          observing.observers <- Uopt.some internal_observer;
          (* By adding [internal_observer] to [observing.observers], we may have added
             on-update handlers to [observing].  We need to handle [observing] after this
             stabilization to give those handlers a chance to run. *)
          handle_after_stabilization observing;
          if debug then assert (Node.is_necessary observing);
          if not was_necessary then became_necessary observing
      done
    ;;

    let observer_value_exn observer =
      let t = Observer.incr_state observer in
      match t.status with
      | Not_stabilizing | Running_on_update_handlers -> Observer.value_exn observer
      | Stabilize_previously_raised raised_exn ->
        Raised_exn.reraise_with_message
          raised_exn
          "Observer.value_exn called after stabilize previously raised"
      | Stabilizing ->
        failwiths
          "Observer.value_exn called during stabilization"
          observer
          [%sexp_of: _ Observer.t]
    ;;

    let observer_value observer =
      try Ok (observer_value_exn observer) with
      | exn -> Error (Error.of_exn exn)
    ;;

    let node_on_update (type a) (node : a Node.t) ~f =
      let t = node.state in
      Node.on_update node (On_update_handler.create f ~at:t.stabilization_num);
      handle_after_stabilization node
    ;;

    let observer_on_update_exn observer ~f =
      let t = Observer.incr_state observer in
      Observer.on_update_exn observer (On_update_handler.create f ~at:t.stabilization_num);
      handle_after_stabilization (Observer.observing observer)
    ;;

    let set_var_while_not_stabilizing var value =
      let t = Var.incr_state var in
      t.num_var_sets <- t.num_var_sets + 1;
      var.value <- value;
      if Stabilization_num.compare var.set_at t.stabilization_num < 0
      then (
        var.set_at <- t.stabilization_num;
        let watch = var.watch in
        if debug then assert (Node.is_stale watch);
        if Node.is_necessary watch && not (Node.is_in_recompute_heap watch)
        then Recompute_heap.add t.recompute_heap watch)
    ;;

    let set_var var value =
      let t = Var.incr_state var in
      match t.status with
      | Running_on_update_handlers | Not_stabilizing ->
        set_var_while_not_stabilizing var value
      | Stabilize_previously_raised raised_exn ->
        Raised_exn.reraise_with_message
          raised_exn
          "cannot set var -- stabilization previously raised"
      | Stabilizing ->
        if Uopt.is_none var.value_set_during_stabilization
        then Stack.push t.set_during_stabilization (T var);
        var.value_set_during_stabilization <- Uopt.some value
    ;;

    let reclaim_space_in_weak_hashtbls t =
      let reclaim (Packed_weak_hashtbl.T weak_hashtbl) =
        Weak_hashtbl.reclaim_space_for_keys_with_unused_data weak_hashtbl
      in
      Thread_safe_queue.dequeue_until_empty ~f:reclaim t.weak_hashtbls [@nontail]
    ;;

    let stabilize_start t =
      t.status <- Stabilizing;
      disallow_finalized_observers t;
      (* Just like for binds, we add new observers before removing disallowed observers to
         potentially avoid switching the observability of some nodes back and forth. *)
      add_new_observers t;
      unlink_disallowed_observers t;
      if debug then invariant t
    ;;

    let stabilize_end t =
      if debug
      then (
        t.only_in_debug.currently_running_node <- None;
        t.only_in_debug.expert_nodes_created_by_current_node <- []);
      (* We increment [t.stabilization_num] before handling variables set during
         stabilization, so that they are treated as set during the new stabilization cycle.
         Also, we increment before running on-update handlers, to avoid running on update
         handlers created during on update handlers. *)
      t.stabilization_num <- Stabilization_num.add1 t.stabilization_num;
      while not (Stack.is_empty t.set_during_stabilization) do
        let (T var) = Stack.pop_exn t.set_during_stabilization in
        let value = Uopt.value_exn var.value_set_during_stabilization in
        var.value_set_during_stabilization <- Uopt.none;
        set_var_while_not_stabilizing var value
      done;
      while not (Stack.is_empty t.handle_after_stabilization) do
        let (T node) = Stack.pop_exn t.handle_after_stabilization in
        node.is_in_handle_after_stabilization <- false;
        let old_value = node.old_value_opt in
        node.old_value_opt <- Uopt.none;
        let node_update : _ Node_update.t =
          if not (Node.is_valid node)
          then Invalidated
          else if not (Node.is_necessary node)
          then Unnecessary
          else (
            let new_value = Uopt.value_exn node.value_opt in
            if Uopt.is_none old_value
            then Necessary new_value
            else Changed (Uopt.unsafe_value old_value, new_value))
        in
        Stack.push t.run_on_update_handlers (T (node, node_update))
      done;
      t.status <- Running_on_update_handlers;
      let now = t.stabilization_num in
      while not (Stack.is_empty t.run_on_update_handlers) do
        let (T (node, node_update)) = Stack.pop_exn t.run_on_update_handlers in
        Node.run_on_update_handlers node node_update ~now
      done;
      t.status <- Not_stabilizing;
      reclaim_space_in_weak_hashtbls t
    ;;

    let raise_during_stabilization t exn =
      let raised = Raised_exn.create exn in
      t.status <- Stabilize_previously_raised raised;
      Raised_exn.reraise raised
    ;;

    let stabilize t =
      ensure_not_stabilizing t ~name:"stabilize" ~allow_in_update_handler:false;
      try
        stabilize_start t;
        let r = t.recompute_heap in
        while Recompute_heap.length r > 0 do
          recompute_first_node_that_is_necessary r
        done;
        stabilize_end t
      with
      | exn -> raise_during_stabilization t exn
    ;;

    module Step_result = struct
      type t =
        | Keep_going
        | Done
      [@@deriving sexp_of]
    end

    let do_one_step_of_stabilize t : Step_result.t =
      try
        match t.status with
        | Not_stabilizing ->
          stabilize_start t;
          Keep_going
        | Stabilizing ->
          let r = t.recompute_heap in
          if Recompute_heap.length r > 0
          then (
            recompute_first_node_that_is_necessary r;
            Keep_going)
          else (
            stabilize_end t;
            Done)
        | Running_on_update_handlers | Stabilize_previously_raised _ ->
          ensure_not_stabilizing t ~name:"step" ~allow_in_update_handler:false;
          assert false
      with
      | exn ->
        (match t.status with
         | Stabilize_previously_raised _ ->
           (* If stabilization has already raised, then [exn] is merely a notification of this
              fact, rather than the original exception itself.  We should just propagate [exn]
              forward; calling [raise_during_stabilization] would store [exn] as the exception
              that initially raised during stabilization. *)
           raise exn
         | _ -> raise_during_stabilization t exn)
    ;;

    let create_node_in t created_in kind =
      t.num_nodes_created <- t.num_nodes_created + 1;
      Node.create t created_in kind
    ;;

    let create_node t kind = create_node_in t t.current_scope kind
    let create_node_top t kind = create_node_in t Scope.top kind

    let create_var t ?(use_current_scope = false) value =
      let scope = if use_current_scope then t.current_scope else Scope.top in
      let watch = create_node_in t scope Uninitialized in
      let var =
        { Var.value
        ; value_set_during_stabilization = Uopt.none
        ; set_at = t.stabilization_num
        ; watch
        }
      in
      Node.set_kind watch (Var var);
      var
    ;;

    (* A [const] value could come from the right-hand side of an outer bind.  So, we create a
       [const] node in the current scope, not in [Scope.top]. *)
    let const t a = create_node t (Const a)
    let map (n : _ Node.t) ~f = create_node n.state (Map (f, n))
    let map2 (n1 : _ Node.t) n2 ~f = create_node n1.state (Map2 (f, n1, n2))

    let both (n1 : _ Node.t) (n2 : _ Node.t) =
      match n1, n2 with
      | { kind = Const a; _ }, { kind = Const b; _ } -> const n1.state (a, b)
      | _ -> map2 n1 n2 ~f:Tuple2.create
    ;;

    let map3 (n1 : _ Node.t) n2 n3 ~f = create_node n1.state (Map3 (f, n1, n2, n3))
    let map4 (n1 : _ Node.t) n2 n3 n4 ~f = create_node n1.state (Map4 (f, n1, n2, n3, n4))

    let map5 (n1 : _ Node.t) n2 n3 n4 n5 ~f =
      create_node n1.state (Map5 (f, n1, n2, n3, n4, n5))
    ;;

    let map6 (n1 : _ Node.t) n2 n3 n4 n5 n6 ~f =
      create_node n1.state (Map6 (f, n1, n2, n3, n4, n5, n6))
    ;;

    let map7 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 ~f =
      create_node n1.state (Map7 (f, n1, n2, n3, n4, n5, n6, n7))
    ;;

    let map8 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 ~f =
      create_node n1.state (Map8 (f, n1, n2, n3, n4, n5, n6, n7, n8))
    ;;

    let map9 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 ~f =
      create_node n1.state (Map9 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9))
    ;;

    let map10 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 ~f =
      create_node n1.state (Map10 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10))
    ;;

    let map11 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 ~f =
      create_node n1.state (Map11 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11))
    ;;

    let map12 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 ~f =
      create_node n1.state (Map12 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12))
    ;;

    let map13 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 ~f =
      create_node n1.state (Map13 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13))
    ;;

    let map14 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 ~f =
      create_node
        n1.state
        (Map14 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14))
    ;;

    let map15 (n1 : _ Node.t) n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 ~f =
      create_node
        n1.state
        (Map15 (f, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15))
    ;;

    let preserve_cutoff ~(input : _ Node.t) ~output =
      Node.set_cutoff
        output
        (Cutoff.create (fun ~old_value:_ ~new_value:_ ->
           Stabilization_num.equal input.changed_at output.changed_at))
    ;;

    let depend_on input ~depend_on =
      let output = map2 input depend_on ~f:(fun a _ -> a) in
      preserve_cutoff ~input ~output;
      output
    ;;

    let necessary_if_alive input =
      (* If [output] is alive, then [observer] is alive, then [input] is necessary.  If
         [output] is unnecessary, then [output] is not a parent of [input], and thus
         [output]'s liveness is dependent solely on user code.  And in particular, if [output]
         dies, then [observer] will be finalized, and then upon the next stabilization,
         [input] will become unnecessary (at least with respect to [output]). *)
      let observer = create_observer input in
      let output =
        map input ~f:(fun a ->
          Gc.keep_alive observer;
          a)
      in
      preserve_cutoff ~input ~output;
      output
    ;;

    let bind (lhs : _ Node.t) ~f =
      let t = lhs.state in
      let lhs_change = create_node t Uninitialized in
      let main = create_node t Uninitialized in
      let bind =
        { Bind.main
        ; f
        ; lhs
        ; lhs_change
        ; rhs = Uopt.none
        ; rhs_scope = Scope.top
        ; all_nodes_created_on_rhs = Uopt.none
        }
      in
      (* We set [lhs_change] to never cutoff so that whenever [lhs] changes, [main] is
         recomputed.  This is necessary to handle cases where [f] returns an existing stable
         node, in which case the [lhs_change] would be the only thing causing [main] to be
         stale. *)
      Node.set_cutoff lhs_change Cutoff.never;
      bind.rhs_scope <- Bind bind;
      Node.set_kind lhs_change (Bind_lhs_change bind);
      Node.set_kind main (Bind_main bind);
      main
    ;;

    let bind2 n1 n2 ~f =
      bind (map2 n1 n2 ~f:(fun v1 v2 -> v1, v2)) ~f:(fun (v1, v2) -> f v1 v2)
    ;;

    let bind3 n1 n2 n3 ~f =
      bind (map3 n1 n2 n3 ~f:(fun v1 v2 v3 -> v1, v2, v3)) ~f:(fun (v1, v2, v3) -> f v1 v2 v3)
    ;;

    let bind4 n1 n2 n3 n4 ~f =
      bind
        (map4 n1 n2 n3 n4 ~f:(fun v1 v2 v3 v4 -> v1, v2, v3, v4))
        ~f:(fun (v1, v2, v3, v4) -> f v1 v2 v3 v4)
    ;;

    let join (lhs : _ Node.t) =
      let t = lhs.state in
      let lhs_change = create_node t Uninitialized in
      let main = create_node t Uninitialized in
      let join = { Join.lhs; lhs_change; rhs = Uopt.none; main } in
      Node.set_cutoff lhs_change Cutoff.never;
      Node.set_kind lhs_change (Join_lhs_change join);
      Node.set_kind main (Join_main join);
      main
    ;;

    let if_ (test : _ Node.t) ~then_ ~else_ =
      let t = test.state in
      let test_change = create_node t Uninitialized in
      let main = create_node t Uninitialized in
      let if_then_else =
        { If_then_else.test; then_; else_; test_change; main; current_branch = Uopt.none }
      in
      Node.set_cutoff test_change Cutoff.never;
      Node.set_kind test_change (If_test_change if_then_else);
      Node.set_kind main (If_then_else if_then_else);
      main
    ;;

    let lazy_from_fun t ~f =
      let scope = t.current_scope in
      Lazy.from_fun (fun () -> within_scope t scope ~f)
    ;;

    let default_hash_table_initial_size = 4

    let memoize_fun_by_key
      ?(initial_size = default_hash_table_initial_size)
      t
      hashable
      project_key
      f
      =
      (* Here's an explanation of why we get [t.current_scope] here, and then call
         [within_scope] below.  Consider this (impossible) alternate implementation of
         [memoize_fun_by_key]:

         {[
           let table =
             Hashtbl.of_alist_exn hashable
               (List.map all_possible_a_values ~f:(fun a -> (project_key a, f a))
           in
           stage (fun key -> Hashtbl.find_exn table (project_key a))
         ]}

         This implementation doesn't use [current_scope] or [within_scope].  All calls to [f]
         naturally occur in [t.current_scope].

         Such an implementation is impossible because we do not have [all_possible_a_values].
         The implementation below uses [within_scope] to call [f a] in the scope that was
         current at the point of the call to [memoize_fun_by_key] so that we can think of the
         [table] as having been created then, when it in reality is created on-demand. *)
      let scope = t.current_scope in
      let table = Hashtbl.create hashable ~size:initial_size in
      stage (fun a ->
        let key = project_key a in
        match Hashtbl.find table key with
        | Some b -> b
        | None ->
          let b = within_scope t scope ~f:(fun () -> f a) in
          Hashtbl.add_exn table ~key ~data:b;
          b)
    ;;

    let array_fold t children ~init ~f =
      if Array.length children = 0
      then const t init
      else create_node t (Array_fold { init; f; children })
    ;;

    let all t ts = array_fold t (Array.of_list_rev ts) ~init:[] ~f:(fun ac a -> a :: ac)

    module Unordered_array_fold_update = Unordered_array_fold.Update

    let unordered_array_fold
      t
      ?(full_compute_every_n_changes = Int.max_value)
      children
      ~init
      ~f
      ~update
      =
      if Array.length children = 0
      then const t init
      else if full_compute_every_n_changes <= 0
      then
        failwiths
          "unordered_array_fold got non-positive full_compute_every_n_changes"
          full_compute_every_n_changes
          [%sexp_of: int]
      else (
        let main = create_node t Uninitialized in
        Node.set_kind
          main
          (Unordered_array_fold
             (Unordered_array_fold.create
                ~init
                ~f
                ~update
                ~full_compute_every_n_changes
                ~children
                ~main));
        main)
    ;;

    let opt_unordered_array_fold t ?full_compute_every_n_changes ts ~init ~f ~f_inverse =
      let f (accum, num_invalid) x =
        match x with
        | None -> accum, num_invalid + 1
        | Some x -> f accum x, num_invalid
      in
      let f_inverse (accum, num_invalid) x =
        match x with
        | None -> accum, num_invalid - 1
        | Some x -> f_inverse accum x, num_invalid
      in
      map
        (unordered_array_fold
           t
           ts
           ~init:(init, 0)
           ~f
           ~update:(F_inverse f_inverse)
           ?full_compute_every_n_changes)
        ~f:(fun (accum, num_invalid) -> if num_invalid = 0 then Some accum else None)
    ;;

    let at_least_k_of t nodes ~k =
      let bool_to_int b = if b then 1 else 0 in
      map
        ~f:(fun i -> i >= k)
        (unordered_array_fold
           t
           nodes
           ~init:0
           ~f:(fun num_true b -> num_true + bool_to_int b)
           ~update:(F_inverse (fun num_true b -> num_true - bool_to_int b)))
    ;;

    let exists t nodes = at_least_k_of t nodes ~k:1
    let for_all t nodes = at_least_k_of t nodes ~k:(Array.length nodes)

    let sum t ?full_compute_every_n_changes nodes ~zero ~add ~sub =
      unordered_array_fold
        t
        nodes
        ~init:zero
        ~f:add
        ~update:(F_inverse sub)
        ?full_compute_every_n_changes
    ;;

    let opt_sum t ?full_compute_every_n_changes nodes ~zero ~add ~sub =
      opt_unordered_array_fold
        t
        nodes
        ~init:zero
        ~f:add
        ~f_inverse:sub
        ?full_compute_every_n_changes
    ;;

    let sum_int t nodes = sum t nodes ~zero:0 ~add:( + ) ~sub:( - )

    let sum_float t nodes =
      sum
        t
        nodes
        ~zero:0.
        ~add:( +. )
        ~sub:( -. )
        ~full_compute_every_n_changes:(Array.length nodes)
    ;;

    let set_freeze (node : _ Node.t) ~child ~only_freeze_when =
      if debug then assert (Scope.is_top node.created_in);
      (* By making [node.kind] be [Freeze], we are making [Node.is_necessary node]. *)
      let was_necessary = Node.is_necessary node in
      Node.set_kind node (Freeze { main = node; child; only_freeze_when });
      if was_necessary
      then add_parent ~child ~parent:node ~child_index:Kind.freeze_child_index
      else became_necessary node
    ;;

    let freeze (child : _ Node.t) ~only_freeze_when =
      let t = child.state in
      let node = create_node_top t Uninitialized in
      set_freeze node ~child ~only_freeze_when;
      node
    ;;

    let at clock time =
      let t = Clock.incr_state clock in
      if Time_ns.( <= ) time (now clock)
      then const t Before_or_after.After
      else (
        let main = create_node t Uninitialized in
        let at = { At.at = time; main; alarm = Alarm.null; clock } in
        Node.set_kind main (At at);
        at.alarm <- add_alarm clock ~at:time (Alarm_value.create (At at));
        main)
    ;;

    let after clock span = at clock (Time_ns.add (now clock) span)

    let next_interval_alarm_strict (clock : Clock.t) ~base ~interval =
      let after = now clock in
      let at = Time_ns.next_multiple ~base ~after ~interval ~can_equal_after:false () in
      if debug then assert (Time_ns.( > ) at after);
      at
    ;;

    let at_intervals (clock : Clock.t) interval =
      let t = Clock.incr_state clock in
      if Time_ns.Span.( < ) interval (Timing_wheel.alarm_precision clock.timing_wheel)
      then failwiths "at_intervals got too small interval" interval [%sexp_of: Time_ns.Span.t];
      let main = create_node t Uninitialized in
      let base = now clock in
      let at_intervals = { At_intervals.main; base; interval; alarm = Alarm.null; clock } in
      Node.set_kind main (At_intervals at_intervals);
      (* [main : unit Node.t], so we make it never cutoff so it changes each time it is
         recomputed. *)
      Node.set_cutoff main Cutoff.never;
      at_intervals.alarm
      <- add_alarm
           clock
           ~at:(next_interval_alarm_strict clock ~base ~interval)
           (Alarm_value.create (At_intervals at_intervals));
      main
    ;;

    let snapshot clock value_at ~at ~before =
      let t = Clock.incr_state clock in
      if Time_ns.( <= ) at (now clock)
      then
        if Time_ns.( < ) at (now clock)
        then Or_error.error "cannot take snapshot in the past" at [%sexp_of: Time_ns.t]
        else Ok (freeze value_at ~only_freeze_when:(Fn.const true))
      else (
        let main = create_node_top t Uninitialized in
        let snapshot = { Snapshot.main; at; before; value_at; clock } in
        Node.set_kind main (Snapshot snapshot);
        (* Unlike other time-based incrementals, a snapshot is created in [Scope.top] and
           cannot be invalidated by its scope.  Thus, there is no need to keep track of the
           alarm that is added, because it will never need to be removed early. *)
        ignore (add_alarm clock ~at (Alarm_value.create (Snapshot snapshot)) : Alarm.t);
        Ok main)
    ;;

    let incremental_step_function clock child =
      let t = Clock.incr_state clock in
      let main = create_node t Uninitialized in
      let step_function_node =
        { Step_function_node.main
        ; value = Uopt.none
        ; child = Uopt.some child
        ; extracted_step_function_from_child_at = Stabilization_num.none
        ; upcoming_steps = Sequence.empty
        ; alarm = Alarm.null
        ; alarm_value = Obj.magic None (* set below *)
        ; clock
        }
      in
      step_function_node.alarm_value <- Alarm_value.create (Step_function step_function_node);
      Node.set_kind main (Step_function step_function_node);
      main
    ;;

    let make_stale (node : _ Node.t) =
      let t = node.state in
      node.recomputed_at <- Stabilization_num.none;
      (* force the node to be stale *)
      if Node.needs_to_be_computed node && not (Node.is_in_recompute_heap node)
      then Recompute_heap.add t.recompute_heap node
    ;;

    let advance_clock (clock : Clock.t) ~to_ =
      let t = Clock.incr_state clock in
      ensure_not_stabilizing t ~name:"advance_clock" ~allow_in_update_handler:true;
      if debug then invariant t;
      if Time_ns.( > ) to_ (now clock)
      then (
        set_var_while_not_stabilizing clock.now to_;
        Timing_wheel.advance_clock clock.timing_wheel ~to_ ~handle_fired:clock.handle_fired;
        Timing_wheel.fire_past_alarms clock.timing_wheel ~handle_fired:clock.handle_fired;
        while Uopt.is_some clock.fired_alarm_values do
          let alarm_value = Uopt.unsafe_value clock.fired_alarm_values in
          clock.fired_alarm_values <- alarm_value.next_fired;
          alarm_value.next_fired <- Uopt.none;
          match alarm_value.action with
          | At { main; _ } ->
            if Node.is_valid main
            then (
              Node.set_kind main (Const After);
              make_stale main)
          | At_intervals ({ main; base; interval; _ } as at_intervals) ->
            if Node.is_valid main
            then (
              at_intervals.alarm
              <- add_alarm
                   clock
                   ~at:(next_interval_alarm_strict clock ~base ~interval)
                   alarm_value;
              make_stale main)
          | Snapshot { main; value_at; _ } ->
            if debug then assert (Node.is_valid main);
            set_freeze main ~child:value_at ~only_freeze_when:(fun _ -> true);
            make_stale main
          | Step_function { main; _ } -> if Node.is_valid main then make_stale main
        done;
        if debug then invariant t)
    ;;

    let create_clock t ~timing_wheel_config ~start =
      let timing_wheel = Timing_wheel.create ~config:timing_wheel_config ~start in
      let rec clock : Clock.t =
        { now = create_var t start
        ; handle_fired
        ; fired_alarm_values = Uopt.none
        ; timing_wheel
        }
      and handle_fired alarm =
        let alarm_value = Timing_wheel.Alarm.value clock.timing_wheel alarm in
        alarm_value.next_fired <- clock.fired_alarm_values;
        clock.fired_alarm_values <- Uopt.some alarm_value
      in
      clock
    ;;

    let create (module Config : Config.Incremental_config) ~max_height_allowed =
      let adjust_heights_heap = Adjust_heights_heap.create ~max_height_allowed in
      let recompute_heap = Recompute_heap.create ~max_height_allowed in
      let t =
        { status = Not_stabilizing
        ; bind_lhs_change_should_invalidate_rhs = Config.bind_lhs_change_should_invalidate_rhs
        ; stabilization_num = Stabilization_num.zero
        ; current_scope = Scope.top
        ; adjust_heights_heap
        ; recompute_heap
        ; propagate_invalidity = Stack.create ()
        ; num_active_observers = 0
        ; all_observers = Uopt.none
        ; finalized_observers = Thread_safe_queue.create ()
        ; disallowed_observers = Stack.create ()
        ; new_observers = Stack.create ()
        ; set_during_stabilization = Stack.create ()
        ; handle_after_stabilization = Stack.create ()
        ; run_on_update_handlers = Stack.create ()
        ; only_in_debug = Only_in_debug.create ()
        ; weak_hashtbls = Thread_safe_queue.create ()
        ; keep_node_creation_backtrace = false
        ; num_nodes_became_necessary = 0
        ; num_nodes_became_unnecessary = 0
        ; num_nodes_changed = 0
        ; num_nodes_invalidated = 0
        ; num_nodes_created = 0
        ; num_nodes_recomputed = 0
        ; num_nodes_recomputed_directly_because_one_child = 0
        ; num_nodes_recomputed_directly_because_min_height = 0
        ; num_var_sets = 0
        }
      in
      t
    ;;

    let weak_memoize_fun_by_key
      ?(initial_size = default_hash_table_initial_size)
      t
      hashable
      project_key
      f
      =
      let scope = t.current_scope in
      let table = Weak_hashtbl.create ~size:initial_size hashable in
      let packed = Packed_weak_hashtbl.T table in
      Weak_hashtbl.set_run_when_unused_data table ~thread_safe_f:(fun () ->
        Thread_safe_queue.enqueue t.weak_hashtbls packed);
      stage (fun a ->
        let key = project_key a in
        match Weak_hashtbl.find table key with
        | Some b -> b
        | None ->
          let b = within_scope t scope ~f:(fun () -> f a) in
          Weak_hashtbl.add_exn table ~key ~data:b;
          b)
    ;;

    module Expert = struct
    /// Given that invalid node are at attempt at avoiding breaking the entire incremental
    /// computation on problems, let's just ignore any operation on an invalid incremental
    /// rather than raising.
    let expertKindOfNode (node : 'a Node) =
        match node.Kind with
        | Kind.Expert e -> Some e
        | Kind.Invalid -> None
        | k -> failwith $"unexpected kind {k} for expert node"

    let create' state onObservabilityChange f =
        let e = Expert.create f onObservabilityChange
        let node = createNode state (Expert e) in
        if Debug.globalFlag then
          if Option.isSome state.OnlyInDebug.CurrentlyRunningNode
          then
            state.OnlyInDebug.ExpertNodesCreatedByCurrentNode <- T node :: state.OnlyInDebug.ExpertNodesCreatedByCurrentNode
        node

  let currently_running_node_exn state name =
    match state.only_in_debug.currently_running_node with
    | None -> raise_s [%sexp ("can only call " ^ name ^ " during stabilization" : string)]
    | Some current -> current
  ;;

  (* Note that the two following functions are not symmetric of one another: in [let y =
     map x], [x] is always a child of [y] (assuming [x] doesn't become invalid) but [y] in
     only a parent of [x] if y is necessary. *)
  let assert_currently_running_node_is_child state node name =
    let (T current) = currently_running_node_exn state name in
    if not (Node.has_child node ~child:current)
    then
      raise_s
        [%sexp
          ("can only call " ^ name ^ " on parent nodes" : string)
          , ~~(node.kind : _ Kind.t)
          , ~~(current.kind : _ Kind.t)]
  ;;

  let assert_currently_running_node_is_parent state node name =
    let (T current) = currently_running_node_exn state name in
    if not (Node.has_parent ~parent:current node)
    then
      raise_s
        [%sexp
          ("can only call " ^ name ^ " on children nodes" : string)
          , ~~(node.kind : _ Kind.t)
          , ~~(current.kind : _ Kind.t)]
  ;;

  let make_stale (node : _ Node.t) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug then assert_currently_running_node_is_child state node "make_stale";
      let e = Uopt.unsafe_value e_opt in
      match Expert.make_stale e with
      | `Already_stale -> ()
      | `Ok ->
        if Node.is_necessary node && not (Node.is_in_recompute_heap node)
        then Recompute_heap.add state.recompute_heap node)
  ;;

  let invalidate (node : _ Node.t) =
    let state = node.state in
    if debug then assert_currently_running_node_is_child state node "invalidate";
    invalidate_node node;
    propagate_invalidity state
  ;;

  let add_dependency (node : _ Node.t) (dep : _ Expert.edge) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug
      then
        if am_stabilizing state
           && not
                (List.mem
                   ~equal:phys_equal
                   state.only_in_debug.expert_nodes_created_by_current_node
                   (T node))
        then assert_currently_running_node_is_child state node "add_dependency";
      let e = Uopt.unsafe_value e_opt in
      let new_child_index = Expert.add_child_edge e (E dep) in
      (* [node] is not guaranteed to be necessary, even if we are running in a child of
         [node], because we could be running due to a parent other than [node] making us
         necessary. *)
      if Node.is_necessary node
      then (
        add_parent ~child:dep.child ~parent:node ~child_index:new_child_index;
        if debug then assert (Node.needs_to_be_computed node);
        if not (Node.is_in_recompute_heap node)
        then Recompute_heap.add state.recompute_heap node))
  ;;

  let remove_dependency (node : _ Node.t) (edge : _ Expert.edge) =
    let state = node.state in
    let e_opt = expert_kind_of_node node in
    if Uopt.is_some e_opt
    then (
      if debug then assert_currently_running_node_is_child state node "remove_dependency";
      let e = Uopt.unsafe_value e_opt in
      (* [node] is not guaranteed to be necessary, for the reason stated in
         [add_dependency] *)
      let edge_index = Uopt.value_exn edge.index in
      let (E last_edge) = Expert.last_child_edge_exn e in
      let last_edge_index = Uopt.value_exn last_edge.index in
      if edge_index <> last_edge_index
      then (
        if Node.is_necessary node
        then
          Node.swap_children_except_in_kind
            node
            ~child1:edge.child
            ~child_index1:edge_index
            ~child2:last_edge.child
            ~child_index2:last_edge_index;
        Expert.swap_children e ~child_index1:edge_index ~child_index2:last_edge_index;
        if debug then Node.invariant ignore node);
      Expert.remove_last_child_edge_exn e;
      if debug then assert (Node.is_stale node);
      if Node.is_necessary node
      then (
        remove_child ~child:edge.child ~parent:node ~child_index:last_edge_index;
        if not (Node.is_in_recompute_heap node)
        then Recompute_heap.add state.recompute_heap node;
        if not (Node.is_valid edge.child) then Expert.decr_invalid_children e))
