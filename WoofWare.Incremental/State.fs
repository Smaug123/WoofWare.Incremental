// [State] defines the global state of which there is one instance for each call to
// [Incremental.Make].
namespace WoofWare.Incremental

open System.Collections.Generic
open TypeEquality
open System
open WoofWare.TimingWheel
open WoofWare.WeakHashTable

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

    let internal stats (t : State) : StateStats =
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
            { new BindMainEval<_, _> with
                member _.Eval bind : FakeUnit =
                    copyChild node bind.Rhs.Value |> FakeUnit.ofUnit
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

            match Sequence.head stepFunctionNode.UpcomingSteps with
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
                                let childIndex = node.MyChildIndexInParentAtIndex.[parentIndex] in
                                Expert.runEdgeCallback expert childIndex
                            | Kind.UnorderedArrayFold u ->
                                { new UnorderedArrayFoldEval<_, _> with
                                    member _.Eval u =
                                        UnorderedArrayFold.childChanged
                                            u
                                            node
                                            node.MyChildIndexInParentAtIndex.[parentIndex]
                                            oldValueOpt
                                            newValue
                                        |> FakeUnit.ofUnit
                                }
                                |> u.Apply
                                |> FakeUnit.toUnit
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
                            let childIndex = node.MyChildIndexInParentAtIndex.[0]
                            Expert.runEdgeCallback p childIndex
                        | Kind.UnorderedArrayFold u ->
                            { new UnorderedArrayFoldEval<_, _> with
                                member _.Eval u =
                                    UnorderedArrayFold.childChanged
                                        u
                                        node
                                        node.MyChildIndexInParentAtIndex.[0]
                                        oldValueOpt
                                        newValue
                                    |> FakeUnit.ofUnit
                            }
                            |> u.Apply
                            |> FakeUnit.toUnit
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
                                // These nodes have more than one child.
                                | Kind.ArrayFold _
                                | Kind.Map2 _
                                | Kind.UnorderedArrayFold _
                                | Kind.Expert _ -> failwith "these nodes have more than one child"
                                // We can immediately recompute [parent] if no other node needs to be stable
                                // before computing it.  If [parent] has a single child (i.e. [node]), then
                                // this amounts to checking that [parent] won't be invalidated, i.e. that
                                // [parent]'s scope has already stabilized.
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
                                | Kind.BindMain b ->
                                    { new BindMainEval<_, _> with
                                        member _.Eval b = node.Height > b.LhsChange.Height
                                    }
                                    |> b.Apply
                                | Kind.IfThenElse i -> node.Height > i.TestChange.Height
                                | Kind.JoinMain j -> node.Height > j.LhsChange.Height

                            if canRecomputeNow then
                                t.NumNodesRecomputedDirectlyBecauseOneChild <-
                                    t.NumNodesRecomputedDirectlyBecauseOneChild + 1

                                recompute parent
                            else if parent.Height <= RecomputeHeap.minHeight t.RecomputeHeap then
                                // If [parent.height] is [<=] the height of all nodes in the recompute heap
                                // (possibly because the recompute heap is empty), then we can recompute
                                // [parent] immediately and save adding it to and then removing it from the
                                // recompute heap.
                                t.NumNodesRecomputedDirectlyBecauseMinHeight <-
                                    t.NumNodesRecomputedDirectlyBecauseMinHeight + 1

                                recompute parent
                            else
                                if Debug.globalFlag then
                                    assert (Node.needsToBeComputed parent)
                                    assert (not (Node.isInRecomputeHeap parent))

                                RecomputeHeap.add t.RecomputeHeap parent

                        FakeUnit.ofUnit ()
                }
                |> node.Parent0.Value.Apply
                |> FakeUnit.toUnit

        if Debug.globalFlag then
            invariant t

    let recomputeFirstNodeThatIsNecessary (r: RecomputeHeap) : unit =
        let node = RecomputeHeap.removeMin r

        { new NodeEval<_> with
            member _.Eval node =
                if Debug.globalFlag && not (Node.needsToBeComputed node) then
                    failwith "node unexpectedly does not need to be computed"

                recompute node |> FakeUnit.ofUnit
        }
        |> node.Apply
        |> FakeUnit.toUnit

    let unlinkDisallowedObservers (t : State) : unit =
        while not (Stack.isEmpty t.DisallowedObservers) do
            let packed = Stack.pop(t.DisallowedObservers).Value

            { new InternalObserverEval<_> with
                member _.Eval internalObserver =
                    if Debug.globalFlag then
                        assert internalObserver.State.IsDisallowed

                    internalObserver.State <- InternalObserverState.Unlinked

                    { new InternalObserverEval<_> with
                        member _.Eval allObservers =
                            if InternalObserver.same internalObserver allObservers then
                                t.AllObservers <- internalObserver.NextInAll

                            InternalObserver.unlink internalObserver
                            checkIfUnnecessary internalObserver.Observing
                            FakeUnit.ofUnit ()
                    }
                    |> t.AllObservers.Value.Apply
            }
            |> packed.Apply
            |> FakeUnit.toUnit

    let disallowFutureUse (internalObserver : InternalObserver<'a>) : unit =
        let t = InternalObserver.incrState internalObserver in

        match internalObserver.State with
        | InternalObserverState.Disallowed
        | InternalObserverState.Unlinked -> ()
        | InternalObserverState.Created ->
            t.NumActiveObservers <- t.NumActiveObservers - 1
            internalObserver.State <- InternalObserverState.Unlinked
            internalObserver.OnUpdateHandlers <- []
        | InternalObserverState.InUse ->
            t.NumActiveObservers <- t.NumActiveObservers - 1
            internalObserver.State <- InternalObserverState.Disallowed
            Stack.push (InternalObserverCrate.make internalObserver) t.DisallowedObservers

    let disallowFinalizedObservers (t : State) : unit =
        let disallowIfFinalized (internalObserver : InternalObserverCrate) =
            { new InternalObserverEval<_> with
                member _.Eval internalObserver =
                    if List.isEmpty internalObserver.OnUpdateHandlers then
                        disallowFutureUse internalObserver

                    FakeUnit.ofUnit ()
            }
            |> internalObserver.Apply
            |> FakeUnit.toUnit

        t.FinalizedObservers |> ThreadSafeQueue.dequeueUntilEmpty disallowIfFinalized

    // This gets called within a finalizer.
    let private observerFinalizer (t : State) =
        Staged.stage (fun observer ->
            let internalObserver = !observer

            t.FinalizedObservers
            |> ThreadSafeQueue.enqueue (InternalObserverCrate.make internalObserver)
        )

    let createObserver (shouldFinalize : bool option) (observing : 'a Node) : InternalObserver<'a> ref =
        let shouldFinalize = defaultArg shouldFinalize true
        let t = observing.State

        let internalObserver : InternalObserver<_> =
            {
                State = InternalObserverState.Created
                Observing = observing
                OnUpdateHandlers = []
                PrevInAll = ValueNone
                NextInAll = ValueNone
                PrevInObserving = ValueNone
                NextInObserving = ValueNone
            }

        Stack.push (InternalObserverCrate.make internalObserver) t.NewObservers
        let observer = ref internalObserver

        if shouldFinalize then
            Gc.addFinalizerIgnore observer (Staged.unstage (observerFinalizer t))

        t.NumActiveObservers <- t.NumActiveObservers + 1
        observer

    let addNewObservers (t : State) : unit =
        while not (Stack.isEmpty t.NewObservers) do
            let packed = Stack.pop t.NewObservers |> Option.get

            { new InternalObserverEval<_> with
                member _.Eval internalObserver =
                    match internalObserver.State with
                    | InternalObserverState.InUse
                    | InternalObserverState.Disallowed -> failwith "oh no"
                    | InternalObserverState.Unlinked -> ()
                    | InternalObserverState.Created ->
                        internalObserver.State <- InternalObserverState.InUse
                        let oldAllObservers = t.AllObservers

                        match oldAllObservers with
                        | ValueSome oldAllObservers' ->
                            internalObserver.NextInAll <- oldAllObservers
                            InternalObserverCrate.setPrevInAll oldAllObservers' (ValueSome packed)
                        | ValueNone -> ()

                        t.AllObservers <- ValueSome packed
                        let observing = internalObserver.Observing
                        let wasNecessary = NodeHelpers.isNecessary observing

                        observing.NumOnUpdateHandlers <-
                            observing.NumOnUpdateHandlers + List.length internalObserver.OnUpdateHandlers

                        let oldObservers = observing.Observers

                        match oldObservers with
                        | ValueSome oldObservers' ->
                            internalObserver.NextInObserving <- oldObservers
                            oldObservers'.PrevInObserving <- ValueSome internalObserver
                        | ValueNone -> ()

                        observing.Observers <- ValueSome internalObserver
                        // By adding [internal_observer] to [observing.observers], we may have added
                        // on-update handlers to [observing].  We need to handle [observing] after this
                        // stabilization to give those handlers a chance to run.
                        handleAfterStabilization observing

                        if Debug.globalFlag then
                            assert (NodeHelpers.isNecessary observing)

                        if not wasNecessary then
                            becameNecessary observing

                    FakeUnit.ofUnit ()
            }
            |> packed.Apply
            |> FakeUnit.toUnit

    let observerValueThrowing (observer : Observer'<'a>) : 'a =
        let t = Observer'.incrState observer

        match t.Status with
        | Status.Not_stabilizing
        | Status.Running_on_update_handlers -> Observer'.valueThrowing observer
        | Status.Stabilize_previously_raised exn ->
            RaisedException.reraiseWithMessage exn "Observer.valueThrowing called after stabilize previously raised"
        | Status.Stabilizing -> failwith "Observer.valueThrowing called during stabilization"

    let observerValue (observer : Observer'<'a>) : Result<'a, exn> =
        try
            Ok (observerValueThrowing observer)
        with exn ->
            Error exn

    let nodeOnUpdate<'a> (node : 'a Node) (f : NodeUpdate<'a> -> unit) : unit =
        let t = node.State
        Node.onUpdate node (OnUpdateHandler.create f t.StabilizationNum)
        handleAfterStabilization node

    let observerOnUpdateThrowing observer f =
        let t = Observer'.incrState observer
        Observer'.onUpdateThrowing observer (OnUpdateHandler.create f t.StabilizationNum)
        handleAfterStabilization (Observer'.observing observer)

    let setVarWhileNotStabilizing (var : Var<'a>) (value : 'a) : unit =
        let t = Var.incrState var
        t.NumVarSets <- t.NumVarSets + 1
        var.Value <- value

        if var.SetAt < t.StabilizationNum then
            var.SetAt <- t.StabilizationNum
            let watch = var.Watch

            if Debug.globalFlag then
                assert (Node.isStale watch)

            if NodeHelpers.isNecessary watch && not (Node.isInRecomputeHeap watch) then
                RecomputeHeap.add t.RecomputeHeap watch

    let setVar (var : Var<'a>) (value : 'a) : unit =
        let t = Var.incrState var in

        match t.Status with
        | Status.Running_on_update_handlers
        | Status.Not_stabilizing -> setVarWhileNotStabilizing var value
        | Status.Stabilize_previously_raised exn ->
            RaisedException.reraiseWithMessage exn "cannot set var -- stabilization previously raised"
        | Status.Stabilizing ->
            if var.ValueSetDuringStabilization.IsNone then
                Stack.push (VarCrate.make var) t.SetDuringStabilization

            var.ValueSetDuringStabilization <- Some value

    let reclaimSpaceInWeakHashTables (t : State) =
        let reclaim (w : WeakHashTableCrate) =
            { new WeakHashTableEval<_> with
                member _.Eval w =
                    WeakHashTable.reclaimSpaceForKeysWithUnusedData w |> FakeUnit.ofUnit
            }
            |> w.Apply
            |> FakeUnit.toUnit

        ThreadSafeQueue.dequeueUntilEmpty reclaim t.WeakHashTables

    let stabilizeStart (t : State) : unit =
        t.Status <- Status.Stabilizing
        disallowFinalizedObservers t
        // Just like for binds, we add new observers before removing disallowed observers to
        // potentially avoid switching the observability of some nodes back and forth.
        addNewObservers t
        unlinkDisallowedObservers t

        if Debug.globalFlag then
            invariant t

    let stabilizeEnd (t : State) : unit =
        if Debug.globalFlag then
            t.OnlyInDebug.CurrentlyRunningNode <- None
            t.OnlyInDebug.ExpertNodesCreatedByCurrentNode <- []
        // We increment [t.stabilization_num] before handling variables set during
        // stabilization, so that they are treated as set during the new stabilization cycle.
        // Also, we increment before running on-update handlers, to avoid running on update
        // handlers created during on update handlers.
        t.StabilizationNum <- StabilizationNum.add1 t.StabilizationNum

        while not (Stack.isEmpty t.SetDuringStabilization) do
            let var = Stack.pop t.SetDuringStabilization |> Option.get

            { new VarEval<_> with
                member _.Eval var =
                    let value = var.ValueSetDuringStabilization.Value
                    var.ValueSetDuringStabilization <- None
                    setVarWhileNotStabilizing var value
                    FakeUnit.ofUnit ()
            }
            |> var.Apply
            |> FakeUnit.toUnit

        while not (Stack.isEmpty t.HandleAfterStabilization) do
            let node = t.HandleAfterStabilization |> Stack.pop |> Option.get

            { new NodeEval<_> with
                member _.Eval node =
                    node.IsInHandleAfterStabilization <- false
                    let oldValue = node.OldValueOpt in
                    node.OldValueOpt <- ValueNone

                    let node_update : _ NodeUpdate =
                        if not (NodeHelpers.isValid node) then
                            Invalidated
                        else if not (NodeHelpers.isNecessary node) then
                            Unnecessary
                        else
                            let new_value = node.ValueOpt.Value in

                            match oldValue with
                            | ValueNone -> Necessary new_value
                            | ValueSome oldValue -> Changed (oldValue, new_value)

                    Stack.push (RunOnUpdateHandlers.make node node_update) t.RunOnUpdateHandlers
                    FakeUnit.ofUnit ()
            }
            |> node.Apply
            |> FakeUnit.toUnit

        t.Status <- Status.Running_on_update_handlers
        let now = t.StabilizationNum

        while not (Stack.isEmpty t.RunOnUpdateHandlers) do
            let rouh = t.RunOnUpdateHandlers |> Stack.pop |> Option.get

            { new RunOnUpdateHandlersEval<_> with
                member _.Eval node nodeUpdate =
                    Node.runOnUpdateHandlers node nodeUpdate now |> FakeUnit.ofUnit
            }
            |> rouh.Apply
            |> FakeUnit.toUnit

        t.Status <- Status.Not_stabilizing
        reclaimSpaceInWeakHashTables t

    let raiseDuringStabilization (t : State) (exn : exn) : 'a =
        let raised = RaisedException.create exn in
        t.Status <- Stabilize_previously_raised raised
        RaisedException.reraise raised

    let stabilize (t : State) : unit =
        ensureNotStabilizing t "stabilize" false

        try
            stabilizeStart t
            let r = t.RecomputeHeap

            while RecomputeHeap.length r > 0 do
                recomputeFirstNodeThatIsNecessary r

            stabilizeEnd t
        with exn ->
            raiseDuringStabilization t exn

    type StepResult =
        | KeepGoing
        | Done

    let doOneStepOfStabilize (t : State) : StepResult =
        try
            match t.Status with
            | Status.Not_stabilizing ->
                stabilizeStart t
                StepResult.KeepGoing
            | Status.Stabilizing ->
                let r = t.RecomputeHeap in

                if RecomputeHeap.length r > 0 then
                    recomputeFirstNodeThatIsNecessary r
                    StepResult.KeepGoing
                else
                    stabilizeEnd t
                    StepResult.Done
            | Status.Running_on_update_handlers
            | Status.Stabilize_previously_raised _ ->
                ensureNotStabilizing t "step" false
                failwith "assert false"
        with exn ->
            (match t.Status with
             | Status.Stabilize_previously_raised _ ->
                 (* If stabilization has already raised, then [exn] is merely a notification of this
              fact, rather than the original exception itself.  We should just propagate [exn]
              forward; calling [raise_during_stabilization] would store [exn] as the exception
              that initially raised during stabilization. *)
                 raise exn
             | _ -> raiseDuringStabilization t exn)

    let createNodeIn (t : State) (createdIn : Scope) (kind : Kind<'a>) : Node<'a> =
        t.NumNodesCreated <- t.NumNodesCreated + 1
        Node.create t createdIn kind

    let createNode (t : State) (kind : Kind<'a>) : Node<'a> = createNodeIn t t.CurrentScope kind
    let createNodeTop (t : State) (kind : Kind<'a>) : Node<'a> = createNodeIn t Scope.top kind

    let createVar (t : State) (useCurrentScope : bool option) (value : 'a) : Var<'a> =
        let useCurrentScope = defaultArg useCurrentScope false
        let scope = if useCurrentScope then t.CurrentScope else Scope.top in
        let watch = createNodeIn t scope Kind.Uninitialized in

        let var =
            {
                Value = value
                ValueSetDuringStabilization = None
                SetAt = t.StabilizationNum
                Watch = watch
            }

        Node.setKind watch (Kind.Var var)
        var

    // A [const] value could come from the right-hand side of an outer bind.  So, we create a
    // [const] node in the current scope, not in [Scope.top].
    let konst t a = createNode t (Kind.Const a)

    let map (f : 'a -> 'b) (n : 'a Node) : Node<'b> =
        createNode n.State (Kind.Map (MapCrate.make f n))

    let map2 (n1 : _ Node) n2 f =
        createNode n1.State (Kind.Map2 (Map2Crate.make f n1 n2))

    let both (n1 : _ Node) (n2 : _ Node) : Node<'a * 'b> =
        match n1.Kind, n2.Kind with
        | Kind.Const a, Kind.Const b -> konst n1.State (a, b)
        | _ -> map2 n1 n2 (fun a b -> (a, b))

    let preserveCutoff (input : 'a Node) (output : Node<'b>) : unit =
        Node.setCutoff output (Cutoff.create (fun _ _ -> input.ChangedAt = output.ChangedAt))

    let dependOn (input : Node<'a>) (dependOn : Node<'b>) : Node<'a> =
        let output = map2 input dependOn (fun a _ -> a) in
        preserveCutoff input output
        output

    let necessaryIfAlive (input : Node<'a>) : Node<'a> =
        // If [output] is alive, then [observer] is alive, then [input] is necessary.  If
        // [output] is unnecessary, then [output] is not a parent of [input], and thus
        // [output]'s liveness is dependent solely on user code.  And in particular, if [output]
        // dies, then [observer] will be finalized, and then upon the next stabilization,
        // [input] will become unnecessary (at least with respect to [output]).
        let observer = createObserver None input

        let output =
            input
            |> map (fun a ->
                GC.KeepAlive observer
                a
            )

        preserveCutoff input output
        output

    let bind (lhs : 'a Node) f =
        let t = lhs.State
        let lhsChange = createNode t Kind.Uninitialized
        let main = createNode t Kind.Uninitialized

        let bind =
            {
                Main = main
                F = f
                Lhs = lhs
                LhsChange = lhsChange
                Rhs = ValueNone
                RhsScope = Scope.top
                AllNodesCreatedOnRhs = ValueNone
            }
        // We set [lhs_change] to never cutoff so that whenever [lhs] changes, [main] is
        // recomputed.  This is necessary to handle cases where [f] returns an existing stable
        // node, in which case the [lhs_change] would be the only thing causing [main] to be
        // stale.
        Node.setCutoff lhsChange Cutoff.never
        let bind' = BindCrate.make bind
        bind.RhsScope <- Scope.Bind bind'
        Node.setKind lhsChange (Kind.BindLhsChange (bind', Teq.refl))
        Node.setKind main (Kind.BindMain (BindMainCrate.make bind))
        main

    let bind2 (n1 : Node<'a>) (n2 : Node<'b>) (f : 'a -> 'b -> Node<'a * 'b>) : Node<'a * 'b> =
        bind (map2 n1 n2 (fun v1 v2 -> v1, v2)) (fun (v1, v2) -> f v1 v2)

    let join (lhs : 'a Node Node) =
        let t = lhs.State
        let lhsChange = createNode t Kind.Uninitialized
        let main = createNode t Kind.Uninitialized

        let join =
            {
                Join.Lhs = lhs
                LhsChange = lhsChange
                Rhs = ValueNone
                Main = main
            }

        Node.setCutoff lhsChange Cutoff.never
        Node.setKind lhsChange (Kind.JoinLhsChange (JoinCrate.make join, Teq.refl))
        Node.setKind main (Kind.JoinMain join)
        main

    let if_ (test : bool Node) (then_ : Node<'a>) (else_ : Node<'a>) : Node<'a> =
        let t = test.State
        let testChange = createNode t Kind.Uninitialized
        let main = createNode t Kind.Uninitialized

        let iTE =
            {
                IfThenElse.Test = test
                Then = then_
                Else = else_
                TestChange = testChange
                Main = main
                CurrentBranch = ValueNone
            }

        Node.setCutoff testChange Cutoff.never
        Node.setKind testChange (Kind.IfTestChange (IfThenElseCrate.make iTE, Teq.refl))
        Node.setKind main (Kind.IfThenElse iTE)
        main

    let lazyFromFun (t : State) (f : unit -> 'a) : Lazy<'a> =
        let scope = t.CurrentScope
        Lazy<_>.Create (fun () -> withinScope t scope f)

    [<Literal>]
    let DEFAULT_HASH_TABLE_INITIAL_SIZE = 4

    let memoize_fun_by_key (initialSize : int option) (t : State) (projectKey : 'key -> 'b) (f : 'key -> 'c) =
        let initialSize = defaultArg initialSize DEFAULT_HASH_TABLE_INITIAL_SIZE
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
        let scope = t.CurrentScope
        let table = Dictionary initialSize

        Staged.stage (fun a ->
            let key = projectKey a

            match table.TryGetValue key with
            | true, v -> v
            | false, _ ->
                let b = withinScope t scope (fun () -> f a) in
                table.Add (key, b)
                b
        )

    let arrayFold (t : State) (children : Node<'a> array) (init : 'acc) (f : 'acc -> 'a -> 'acc) : Node<'acc> =
        if Array.length children = 0 then
            konst t init
        else
            {
                Init = init
                Children = children
                F = f
            }
            |> ArrayFoldCrate.make
            |> Kind.ArrayFold
            |> createNode t

    let all (t : State) (ts : Node<'a> list) : Node<'a list> =
        arrayFold t (Array.ofList (List.rev ts)) [] (fun ac a -> a :: ac)

    let internal unorderedArrayFold
        (t : State)
        (fullComputeEveryNChanges : int option)
        (children : Node<'b> array)
        (init : 'acc)
        (f : 'acc -> 'b -> 'acc)
        (update : Update<'b, 'acc>)
        : Node<'acc>
        =
        let fullComputeEveryNChanges = defaultArg fullComputeEveryNChanges Int32.MaxValue

        if Array.length children = 0 then
            konst t init
        else if fullComputeEveryNChanges <= 0 then
            invalidArg
                "fullComputeEveryNChanges"
                $"unordered_array_fold got non-positive full_compute_every_n_changes %i{fullComputeEveryNChanges}"
        else
            let main = createNode t Kind.Uninitialized

            UnorderedArrayFold.create init f update fullComputeEveryNChanges children main
            |> UnorderedArrayFoldCrate.make
            |> Kind.UnorderedArrayFold
            |> Node.setKind main

            main

    let optUnorderedArrayFold
        (t : State)
        (fullComputeEveryNChanges : int option)
        (ts : Node<'a option> array)
        (init : 'b)
        (f : 'b -> 'a -> 'b)
        (fInverse : 'b -> 'a -> 'b)
        : Node<'b option>
        =
        let f (accum, num_invalid) x =
            match x with
            | None -> accum, num_invalid + 1
            | Some x -> f accum x, num_invalid

        let fInverse (accum, num_invalid) x =
            match x with
            | None -> accum, num_invalid - 1
            | Some x -> fInverse accum x, num_invalid

        unorderedArrayFold t fullComputeEveryNChanges ts (init, 0) f (Update.FInverse fInverse)
        |> map (fun (accum, numInvalid) -> if numInvalid = 0 then Some accum else None)

    let atLeastKOf (t : State) (nodes : Node<bool> array) (k : int) : Node<bool> =
        let boolToInt b = if b then 1 else 0

        unorderedArrayFold
            t
            None
            nodes
            0
            (fun numTrue b -> numTrue + boolToInt b)
            (Update.FInverse (fun numTrue b -> numTrue - boolToInt b))
        |> map (fun i -> i >= k)

    let exists (t : State) (nodes : Node<bool> array) : Node<bool> = atLeastKOf t nodes 1
    let forAll t nodes = atLeastKOf t nodes (Array.length nodes)

    let sum
        (t : State)
        (fullComputeEveryNChanges : int option)
        (nodes : Node<'a> array)
        (zero : 'b)
        (add : 'b -> 'a -> 'b)
        (sub : 'b -> 'a -> 'b)
        : Node<'b>
        =
        unorderedArrayFold t fullComputeEveryNChanges nodes zero add (Update.FInverse sub)

    let optSum
        (t : State)
        (fullComputeEveryNChanges : int option)
        (nodes : Node<'a option> array)
        (zero : 'b)
        (add : 'b -> 'a -> 'b)
        (sub : 'b -> 'a -> 'b)
        : Node<'b option>
        =
        optUnorderedArrayFold t fullComputeEveryNChanges nodes zero add sub

    let inline sum'<'T
        when 'T : (static member (+) : 'T * 'T -> 'T)
        and 'T : (static member (-) : 'T * 'T -> 'T)
        and 'T : (static member Zero : 'T)>
        (t : State)
        (nodes : Node<'T> array)
        : Node<'T>
        =
        sum t None nodes LanguagePrimitives.GenericZero (+) (-)

    let setFreeze (node : 'a Node) (child : Node<'a>) (onlyFreezeWhen : 'a -> bool) : unit =
        if Debug.globalFlag then
            assert node.CreatedIn.IsTop
        // By making [node.kind] be [Freeze], we are making [Node.is_necessary node].
        let wasNecessary = NodeHelpers.isNecessary node in

        Node.setKind
            node
            (Kind.Freeze
                {
                    Main = node
                    Child = child
                    OnlyFreezeWhen = onlyFreezeWhen
                })

        if wasNecessary then
            addParent child node Kind.freezeChildIndex
        else
            becameNecessary node

    let freeze (child : 'a Node) (onlyFreezeWhen : 'a -> bool) : Node<'a> =
        let t = child.State
        let node = createNodeTop t Kind.Uninitialized
        setFreeze node child onlyFreezeWhen
        node

    let at (clock : Clock) (time : TimeNs) : Node<BeforeOrAfter> =
        let t = Clock.incrState clock

        if time <= (Clock.now clock) then
            konst t BeforeOrAfter.After
        else
            let main = createNode t Kind.Uninitialized

            let at =
                {
                    At = time
                    Main = main
                    Alarm = TimingWheel.Alarm.null'
                    Clock = clock
                }

            Node.setKind main (Kind.At (at, Teq.refl))
            at.Alarm <- addAlarm clock time (AlarmValue.create (AlarmValueAction.At at))
            main

    let after (clock : Clock) (span : TimeNs.Span) : Node<BeforeOrAfter> =
        at clock (TimeNs.add (Clock.now clock) span)

    let nextIntervalAlarmStrict (clock : Clock) base_ interval : TimeNs =
        let after = Clock.now clock
        let at = TimeNs.nextMultiple (Some false) base_ after interval

        if Debug.globalFlag then
            assert (at > after)

        at

    let at_intervals (clock : Clock) interval =
        let t = Clock.incrState clock

        if interval < (TimingWheel.alarmPrecision clock.TimingWheel) then
            failwith $"at_intervals got too small interval: {interval}"

        let main = createNode t Kind.Uninitialized
        let base_ = Clock.now clock

        let atIntervals =
            {
                AtIntervals.Main = main
                Base = base_
                Interval = interval
                Alarm = TimingWheel.Alarm.null'
                Clock = clock
            }

        Node.setKind main (Kind.AtIntervals (atIntervals, Teq.refl))
        // [main : unit Node.t], so we make it never cutoff so it changes each time it is
        // recomputed.
        Node.setCutoff main Cutoff.never

        atIntervals.Alarm <-
            addAlarm
                clock
                (nextIntervalAlarmStrict clock base_ interval)
                (AlarmValue.create (AlarmValueAction.AtIntervals atIntervals))

        main

    let snapshot (clock : Clock) (valueAt : Node<'a>) (at : TimeNs) (before : 'a) : Result<Node<'a>, string> =
        let t = Clock.incrState clock in

        if at <= Clock.now clock then
            if at < Clock.now clock then
                Error "cannot take snapshot in the past"
            else
                Ok (freeze valueAt (fun _ -> true))
        else
            let main = createNodeTop t Kind.Uninitialized in

            let snapshot =
                {
                    Snapshot.Main = main
                    At = at
                    Before = before
                    ValueAt = valueAt
                    Clock = clock
                }

            Node.setKind main (Kind.Snapshot snapshot)
            // Unlike other time-based incrementals, a snapshot is created in [Scope.top] and
            // cannot be invalidated by its scope.  Thus, there is no need to keep track of the
            // alarm that is added, because it will never need to be removed early.
            addAlarm clock at (AlarmValue.create (AlarmValueAction.Snapshot (SnapshotCrate.make snapshot)))
            |> ignore<TimingWheel.Alarm>

            Ok main

    let incremental_step_function clock child =
        let t = Clock.incrState clock
        let main = createNode t Kind.Uninitialized

        let stepFunctionNode =
            {
                StepFunctionNode.Main = main
                Value = ValueNone
                Child = ValueSome child
                ExtractedStepFunctionFromChildAt = StabilizationNum.none
                UpcomingSteps = Sequence.empty ()
                Alarm = TimingWheel.Alarm.null'
                AlarmValue = Unchecked.defaultof<_> (* set below *)
                Clock = clock
            }

        stepFunctionNode.AlarmValue <-
            AlarmValue.create (AlarmValueAction.StepFunction (StepFunctionNodeCrate.make stepFunctionNode))

        Node.setKind main (Kind.StepFunction stepFunctionNode)
        main

    let makeStale (node : 'a Node) : unit =
        let t = node.State
        node.RecomputedAt <- StabilizationNum.none
        // force the node to be stale
        if Node.needsToBeComputed node && not (Node.isInRecomputeHeap node) then
            RecomputeHeap.add t.RecomputeHeap node

    let advanceClock (clock : Clock) to_ =
        let t = Clock.incrState clock
        ensureNotStabilizing t "advance_clock" true

        if Debug.globalFlag then
            invariant t

        if to_ > (Clock.now clock) then
            setVarWhileNotStabilizing clock.Now to_
            TimingWheel.advanceClock clock.TimingWheel to_ clock.HandleFired
            TimingWheel.firePastAlarms clock.TimingWheel clock.HandleFired

            while clock.FiredAlarmValues.IsSome do
                let alarmValue = clock.FiredAlarmValues.Value
                clock.FiredAlarmValues <- alarmValue.NextFired
                alarmValue.NextFired <- ValueNone

                match alarmValue.Action with
                | AlarmValueAction.At action ->
                    let main = action.Main

                    if NodeHelpers.isValid main then
                        Node.setKind main (Kind.Const BeforeOrAfter.After)
                        makeStale main
                | AlarmValueAction.AtIntervals atIntervals ->
                    let main = atIntervals.Main
                    let base_ = atIntervals.Base
                    let interval = atIntervals.Interval

                    if NodeHelpers.isValid main then
                        atIntervals.Alarm <- addAlarm clock (nextIntervalAlarmStrict clock base_ interval) alarmValue
                        makeStale main
                | AlarmValueAction.Snapshot snap ->
                    { new SnapshotEval<_> with
                        member _.Eval snap =
                            let main = snap.Main
                            let valueAt = snap.ValueAt

                            if Debug.globalFlag then
                                assert (NodeHelpers.isValid main)

                            setFreeze main valueAt (fun _ -> true)
                            makeStale main |> FakeUnit.ofUnit
                    }
                    |> snap.Apply
                    |> FakeUnit.toUnit
                | AlarmValueAction.StepFunction sf ->
                    { new StepFunctionNodeEval<_> with
                        member _.Eval sf =
                            let main = sf.Main

                            if NodeHelpers.isValid main then
                                makeStale main

                            FakeUnit.ofUnit ()
                    }
                    |> sf.Apply
                    |> FakeUnit.toUnit

            if Debug.globalFlag then
                invariant t

    let createClock t timingWheelConfig start =
        let timingWheel = TimingWheel.create timingWheelConfig start
        let now = createVar t None start
        let mutable clock' = Unchecked.defaultof<_>

        let handleFired (alarm : TimingWheel.Alarm) : unit =
            let alarmValue = TimingWheel.Alarm.value timingWheel alarm
            alarmValue.NextFired <- clock'.FiredAlarmValues
            clock'.FiredAlarmValues <- ValueSome alarmValue

        let clock : Clock =
            {
                Clock.Now = now
                HandleFired = handleFired
                FiredAlarmValues = ValueNone
                TimingWheel = timingWheel
            }

        clock' <- clock

        clock

    let create (maxHeightAllowed : int) : State =
        let adjustHeightsHeap = AdjustHeightsHeap.create maxHeightAllowed
        let recomputeHeap = RecomputeHeap.create maxHeightAllowed

        let t =
            {
                Status = Status.Not_stabilizing
                BindLhsChangeShouldInvalidateRhs = true
                StabilizationNum = StabilizationNum.zero
                CurrentScope = Scope.top
                AdjustHeightsHeap = adjustHeightsHeap
                RecomputeHeap = recomputeHeap
                PropagateInvalidity = Stack.create ()
                NumActiveObservers = 0
                AllObservers = ValueNone
                FinalizedObservers = ThreadSafeQueue.create ()
                DisallowedObservers = Stack.create ()
                NewObservers = Stack.create ()
                SetDuringStabilization = Stack.create ()
                HandleAfterStabilization = Stack.create ()
                RunOnUpdateHandlers = Stack.create ()
                OnlyInDebug = OnlyInDebug.create ()
                WeakHashTables = ThreadSafeQueue.create ()
                KeepNodeCreationBacktrace = false
                NumNodesBecameNecessary = 0
                NumNodesBecameUnnecessary = 0
                NumNodesChanged = 0
                NumNodesInvalidated = 0
                NumNodesCreated = 0
                NumNodesRecomputed = 0
                NumNodesRecomputedDirectlyBecauseOneChild = 0
                NumNodesRecomputedDirectlyBecauseMinHeight = 0
                NumVarSets = 0
            }

        t

    let weakMemoizeFunByKey (initialSize : int option) (t : State) project_key f =
        let initialSize = defaultArg initialSize DEFAULT_HASH_TABLE_INITIAL_SIZE
        let scope = t.CurrentScope
        let table = WeakHashTable.create (Some initialSize)
        let packed = WeakHashTableCrate.make table
        WeakHashTable.setRunWhenUnusedData table (fun () -> t.WeakHashTables |> ThreadSafeQueue.enqueue packed)

        Staged.stage (fun a ->
            let key = project_key a in

            match WeakHashTable.find table key with
            | Some b -> b
            | None ->
                let b = withinScope t scope (fun () -> f a) in
                WeakHashTable.addThrowing table key b
                b
        )

    [<RequireQualifiedAccess>]
    module Expert =
        /// Given that invalid node are at attempt at avoiding breaking the entire incremental
        /// computation on problems, let's just ignore any operation on an invalid incremental
        /// rather than raising.
        let expertKindOfNode (node : 'a Node) : Expert<'a> option =
            match node.Kind with
            | Kind.Expert e -> Some e
            | Kind.Invalid -> None
            | k -> failwith $"unexpected kind {k} for expert node"

        let create (state : State) onObservabilityChange f =
            let e = Expert.create f onObservabilityChange
            let node = createNode state (Expert e) in

            if Debug.globalFlag then
                if Option.isSome state.OnlyInDebug.CurrentlyRunningNode then
                    state.OnlyInDebug.ExpertNodesCreatedByCurrentNode <-
                        NodeCrate.make node :: state.OnlyInDebug.ExpertNodesCreatedByCurrentNode

            node

        let currentlyRunningNodeThrowing state name =
            match state.OnlyInDebug.CurrentlyRunningNode with
            | None -> failwith "can only call currentlyRunningNode during stabilization"
            | Some current -> current

        (* Note that the two following functions are not symmetric of one another: in [let y =
             map x], [x] is always a child of [y] (assuming [x] doesn't become invalid) but [y] in
             only a parent of [x] if y is necessary. *)

        let assertCurrentlyRunningNodeIsChild (state : State) (node : Node<'a>) (name : string) : unit =
            let current = currentlyRunningNodeThrowing state name

            { new NodeEval<_> with
                member _.Eval current =
                    if not (Node.hasChild node current) then
                        failwith $"can only call %s{name} on parent nodes"

                    FakeUnit.ofUnit ()
            }
            |> current.Apply
            |> FakeUnit.toUnit

        let assertCurrentlyRunningNodeIsParent state node name =
            let current = currentlyRunningNodeThrowing state name

            { new NodeEval<_> with
                member _.Eval current =
                    if not (Node.hasParent node current) then
                        failwith $"can only call %s{name} on children nodes"

                    FakeUnit.ofUnit ()
            }
            |> current.Apply
            |> FakeUnit.toUnit

        let makeStale (node : 'a Node) : unit =
            let state = node.State

            match expertKindOfNode node with
            | None -> ()
            | Some e ->
                if Debug.globalFlag then
                    assertCurrentlyRunningNodeIsChild state node "makeStale"

                match Expert.makeStale e with
                | StaleResult.AlreadyStale -> ()
                | StaleResult.Ok ->
                    if NodeHelpers.isNecessary node && not (Node.isInRecomputeHeap node) then
                        RecomputeHeap.add state.RecomputeHeap node

        let invalidate (node : 'a Node) : unit =
            let state = node.State

            if Debug.globalFlag then
                assertCurrentlyRunningNodeIsChild state node "invalidate"

            invalidateNode node
            propagateInvalidity state

        let addDependency (node : 'a Node) (dep : 'b ExpertEdge) : unit =
            let state = node.State

            match expertKindOfNode node with
            | None -> ()
            | Some e ->
                if Debug.globalFlag then
                    let target = NodeCrate.make node

                    if
                        amStabilizing state
                        && not (
                            List.exists
                                (fun n -> Object.ReferenceEquals (n, target))
                                state.OnlyInDebug.ExpertNodesCreatedByCurrentNode
                        )
                    then
                        assertCurrentlyRunningNodeIsChild state node "add_dependency"

                let newChildIndex = Expert.addChildEdge e (ExpertEdgeCrate.make dep)
                // [node] is not guaranteed to be necessary, even if we are running in a child of
                // [node], because we could be running due to a parent other than [node] making us
                // necessary.
                if NodeHelpers.isNecessary node then
                    addParent dep.Child node newChildIndex

                    if Debug.globalFlag then
                        assert (Node.needsToBeComputed node)

                    if not (Node.isInRecomputeHeap node) then
                        RecomputeHeap.add state.RecomputeHeap node

        let removeDependency (node : 'a Node) (edge : 'b ExpertEdge) : unit =
            let state = node.State

            match expertKindOfNode node with
            | None -> ()
            | Some e ->
                if Debug.globalFlag then
                    assertCurrentlyRunningNodeIsChild state node "remove_dependency"
                // [node] is not guaranteed to be necessary, for the reason stated in
                // [add_dependency]
                let edgeIndex = edge.Index.Value
                let lastEdge = Expert.lastChildEdgeThrowing e

                { new ExpertEdgeEval<_> with
                    member _.Eval lastEdge =
                        let lastEdgeIndex = lastEdge.Index.Value

                        if edgeIndex <> lastEdgeIndex then
                            if NodeHelpers.isNecessary node then
                                Node.swapChildrenExceptInKind node edge.Child edgeIndex lastEdge.Child lastEdgeIndex

                            Expert.swapChildren e edgeIndex lastEdgeIndex

                            if Debug.globalFlag then
                                Node.invariant ignore node

                        Expert.removeLastChildEdgeThrowing e

                        if Debug.globalFlag then
                            assert (Node.isStale node)

                        if NodeHelpers.isNecessary node then
                            removeChild edge.Child node lastEdgeIndex

                            if not (Node.isInRecomputeHeap node) then
                                RecomputeHeap.add state.RecomputeHeap node

                            if not (NodeHelpers.isValid edge.Child) then
                                Expert.decrInvalidChildren e

                        FakeUnit.ofUnit ()
                }
                |> lastEdge.Apply
                |> FakeUnit.toUnit
