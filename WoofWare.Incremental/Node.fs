namespace WoofWare.Incremental

open System
open TypeEquality

[<RequireQualifiedAccess>]
module internal Node =

    let nodeIsInjective<'a, 'b> (t : Teq<'a Node, 'b Node>) : Teq<'a, 'b> = Teq.Cong.believeMe t

    let same (t1 : Node<'a>) (t2 : Node<'b>) = Type.referenceEqual' t1 t2

    let private nodeAsObjEval : NodeEval<obj> =
        { new NodeEval<_> with
            member _.Eval n = n :> obj
        }

    let private changedAtEval : NodeEval<StabilizationNum> =
        { new NodeEval<_> with
            member _.Eval n = n.ChangedAt
        }

    let private isValidEval : NodeEval<bool> =
        { new NodeEval<_> with
            member _.Eval n = NodeHelpers.isValid n
        }

    // Extract the underlying node as obj. Since Node<'a> is a reference type,
    // this doesn't allocate - it just returns the same reference.
    let private nodeAsObj (t : NodeCrate) = t.Apply nodeAsObjEval
    let private changedAtOfCrate (t : NodeCrate) = t.Apply changedAtEval
    let private isValidCrate (t : NodeCrate) = t.Apply isValidEval

    let packedSame (t1 : NodeCrate) (t2 : NodeCrate) =
        Type.referenceEqual' (nodeAsObj t1) (nodeAsObj t2)

    let packedSameAsUnpacked (packed : NodeCrate) (unpacked : Node<'a>) =
        Type.referenceEqual' (nodeAsObj packed) (unpacked :> obj)

    let initialNumChildren (n : Node<_>) : int = Kind.initialNumChildren n.Kind
    let iteriChildren (t : Node<'a>) (f : int -> NodeCrate -> unit) : unit = Kind.iteriChildren t.Kind f

    /// Allocation-free version of iteriChildren using a struct visitor.
    let inline iteriChildrenWithVisitor<'a, 'TVisitor, 'TState
        when 'TVisitor : struct and 'TVisitor :> IChildVisitor<'TState>>
        (t : Node<'a>)
        (visitor : 'TVisitor)
        (state : 'TState)
        : unit
        =
        Kind.iteriChildrenWithVisitor t.Kind visitor state

    let userInfo (t : Node<'a>) : string option =
        match t.UserInfo with
        | None -> None
        | Some (DotUserInfo.Info i) -> Some i
        | Some other -> Some (failwith "TODO: this was Info.create_s (Dot_user_info.sexp_of_t other)")

    let setUserInfo (t : Node<'a>) (info : string option) : unit =
        let desired =
            match info with
            | None -> None
            | Some i -> Some (DotUserInfo.Info i)

        t.UserInfo <- desired

    let appendUserInfoGraphviz (t : Node<'a>) (label : string list) (attrs : Map<string, string>) : unit =
        let new' = DotUserInfo.dot label attrs

        let newUserInfo =
            match t.UserInfo with
            | None -> new'
            | Some other -> DotUserInfo.append other new'

        t.UserInfo <- Some newUserInfo

    let edgeIsStale (child : Node<'a>) (parent : Node<'b>) : bool = child.ChangedAt > parent.RecomputedAt

    let isStaleWithRespectToAChild (t : Node<'a>) : bool =
        let mutable isStale = false
        let parentRecomputedAt = t.RecomputedAt

        iteriChildren
            t
            (fun _ child ->
                if changedAtOfCrate child > parentRecomputedAt then
                    isStale <- true
            )

        isStale

    let isStale<'a> (t : Node<'a>) : bool =
        match t.Kind with
        | Kind.Uninitialized -> failwith "should not have called"
        | Kind.Const _ ->
            // A const node is stale only at initialization.
            StabilizationNum.isNone t.RecomputedAt
        | At _
        | Kind.AtIntervals _
        | Kind.Snapshot _ ->
            // Time-based nodes are considered stale when [t.recomputed_at] is none, which happens
            // at initialization and when the alarm mechanism makes a node stale (it sets the
            // [t.recomputed_at] to [Stabilization_num.none]).
            StabilizationNum.isNone t.RecomputedAt
        | Kind.Invalid ->
            // We never consider an invalidated node to be stale -- when we invalidate a node, we immediately
            // propagate invalidity to its ancestors.
            false
        | Kind.Var node ->
            // A [Var] node is stale if it was set since it was recomputed.
            node.SetAt > t.RecomputedAt
        // Nodes that have children.
        | Kind.BindLhsChange _
        | Kind.IfTestChange _
        | Kind.JoinLhsChange _ -> StabilizationNum.isNone t.RecomputedAt || isStaleWithRespectToAChild t
        | Kind.ArrayFold _
        | Kind.BindMain _
        | Kind.Freeze _
        | Kind.IfThenElse _
        | Kind.JoinMain _
        | Kind.Map _
        | Kind.Map2 _
        | Kind.StepFunction _
        | Kind.UnorderedArrayFold _ -> StabilizationNum.isNone t.RecomputedAt || isStaleWithRespectToAChild t
        | Expert node ->
            node.ForceStale
            || StabilizationNum.isNone t.RecomputedAt
            || isStaleWithRespectToAChild t

    let needsToBeComputed (t : Node<'a>) : bool = NodeHelpers.isNecessary t && isStale t

    let isInRecomputeHeap (t : Node<'a>) : bool = t.HeightInRecomputeHeap >= 0
    let isInAdjustHeightsHeap (t : Node<'a>) : bool = t.HeightInAdjustHeightsHeap >= 0

    let getParent (t : Node<'a>) (index : int) : NodeCrate =
        if index = 0 then
            t.Parent0
        else
            t.Parent1AndBeyond.[index - 1]
        |> ValueOption.get

    let iteriParents (t : Node<'a>) (f : int -> NodeCrate -> unit) : unit =
        if t.NumParents > 0 then
            f 0 t.Parent0.Value

            for index = 1 to t.NumParents - 1 do
                f index t.Parent1AndBeyond.[index - 1].Value

    let hasChild (t : Node<'a>) (child : Node<'b>) : bool =
        let mutable has = false

        iteriChildren
            t
            (fun _ child' ->
                if packedSameAsUnpacked child' child then
                    has <- true
            )

        has

    let hasInvalidChild (t : Node<'a>) : bool =
        let mutable has = false

        iteriChildren
            t
            (fun _ child ->
                if not (isValidCrate child) then
                    has <- true
            )

        has

    let hasParent (t : Node<'a>) (parent : Node<'b>) : bool =
        let mutable has = false

        iteriParents
            t
            (fun _ parent' ->
                if packedSameAsUnpacked parent' parent then
                    has <- true
            )

        has

    let shouldBeInvalidated<'a> (t : Node<'a>) : bool =
        match t.Kind with
        (* nodes with no children *)
        | Kind.Uninitialized -> failwith "should not call"
        | Kind.At _ -> false
        | Kind.AtIntervals _ -> false
        | Kind.Const _
        | Kind.Snapshot _
        | Kind.Var _ -> false
        | Kind.Invalid -> false
        (* Nodes with a fixed set of children are invalid if any child is invalid. *)
        | Kind.ArrayFold _
        | Kind.Freeze _
        | Kind.Map _
        | Kind.Map2 _
        | Kind.StepFunction _
        | Kind.UnorderedArrayFold _ -> hasInvalidChild t
        (* A *_change node is invalid if the node it is watching for changes is invalid (same
             reason as above).  This is equivalent to [has_invalid_child t]. *)
        | Kind.BindLhsChange (cr, _) ->
            { new BindEval<_> with
                member _.Eval cr = NodeHelpers.isValid cr.Lhs |> not
            }
            |> cr.Apply
        | Kind.IfTestChange (cr, _) ->
            { new IfThenElseEval<_> with
                member _.Eval cr = not (NodeHelpers.isValid cr.Test)
            }
            |> cr.Apply
        | Kind.JoinLhsChange (cr, _) ->
            { new JoinEval<_> with
                member _.Eval cr = not (NodeHelpers.isValid cr.Lhs)
            }
            |> cr.Apply
        (* [Bind_main], [If_then_else], and [Join_main] are invalid if their *_change child is,
             but not necessarily if their other children are -- the graph may be restructured to
             avoid the invalidity of those. *)
        | Kind.BindMain cr ->
            { new BindMainEval<_, _> with
                member _.Eval cr = not (NodeHelpers.isValid cr.LhsChange)
            }
            |> cr.Apply
        | Kind.IfThenElse ite -> not (NodeHelpers.isValid ite.TestChange)
        | Kind.JoinMain join -> not (NodeHelpers.isValid join.LhsChange)
        | Kind.Expert _ ->
            (* This is similar to what we do for bind above, except that any invalid child can be
               removed, so we can only tell if an expert node becomes invalid when all its
               dependencies have fired (which in practice means when we are about to run it). *)
            false

    let foldObservers (t : Node<'a>) (init : 'acc) (f : 'acc -> InternalObserver<'a> -> 'acc) : 'acc =
        let mutable r = t.Observers
        let mutable ac = init

        while r.IsSome do
            let observer = r.Value
            r <- observer.NextInObserving
            ac <- f ac observer

        ac

    let iterObservers (t : Node<'a>) (f : InternalObserver<'a> -> unit) : unit = foldObservers t () (fun () -> f)

    let unsafeValue (t : Node<'a>) : 'a = t.ValueOpt.Value

    let valueThrowing (t : Node<'a>) : 'a =
        match t.ValueOpt with
        | ValueNone -> failwith "attempt to get value of an invalid node"
        | ValueSome v -> v

    let internal getCutoff (t : Node<'a>) : Cutoff<'a> = t.Cutoff
    let internal setCutoff (t : Node<'a>) (cutoff : Cutoff<'a>) : unit = t.Cutoff <- cutoff

    let isConst (t : Node<'a>) : bool =
        match t.Kind with
        | Kind.Const _ -> true
        | _ -> false

    let onUpdate (t : Node<'a>) (onUpdateHandler : OnUpdateHandler<'a>) : unit =
        t.OnUpdateHandlers <- onUpdateHandler :: t.OnUpdateHandlers
        t.NumOnUpdateHandlers <- t.NumOnUpdateHandlers + 1

    let runOnUpdateHandlers (t : Node<'a>) (nodeUpdate : NodeUpdate<'a>) (now : StabilizationNum) : unit =
        let mutable r = t.OnUpdateHandlers

        while not r.IsEmpty do
            match r with
            | [] -> failwith "LOGIC ERROR"
            | onUpdateHandler :: rest ->
                r <- rest
                OnUpdateHandler.run onUpdateHandler nodeUpdate now

        let mutable r = t.Observers

        while r.IsSome do
            let observer = r.Value
            r <- observer.NextInObserving
            let mutable r = observer.OnUpdateHandlers

            while not r.IsEmpty do
                match r with
                | [] -> failwith "LOGIC ERROR"
                | onUpdateHandler :: rest ->
                    r <- rest
                    // We have to test [state] before each on-update handler, because an on-update
                    // handler might disable its own observer, which should prevent other on-update
                    // handlers in the same observer from running.
                    match observer.State with
                    | InternalObserverState.Created
                    | InternalObserverState.Unlinked -> failwith "unexpected"
                    | InternalObserverState.Disallowed -> ()
                    | InternalObserverState.InUse -> OnUpdateHandler.run onUpdateHandler nodeUpdate now

    let setKind (t : Node<'a>) (kind : Kind<'a>) : unit =
        t.Kind <- kind
        t.MyParentIndexInChildAtIndex <- Array.create (Kind.initialNumChildren kind) -1

    let invariant<'a> (invA : 'a -> unit) (t : Node<'a>) : unit =
        if needsToBeComputed t <> isInRecomputeHeap t then
            failwith "invariant failure"

        if NodeHelpers.isNecessary t then
            if t.Height <= Scope.height t.CreatedIn then
                failwith "invariant failure"

            iteriChildren
                t
                (fun _ child ->
                    { new NodeEval<_> with
                        member _.Eval child =
                            if t.Height <= child.Height then
                                failwith "invariant failure"

                            if not (hasParent child t) then
                                failwith "invariant failure"

                            FakeUnit.ofUnit ()
                    }
                    |> child.Apply
                    |> FakeUnit.toUnit
                )

            if shouldBeInvalidated t then
                failwith "invariant failure"

        iteriParents
            t
            (fun _ parent ->
                { new NodeEval<_> with
                    member _.Eval parent =
                        if not (hasChild parent t) then
                            failwith "invariant failure"

                        if not (NodeHelpers.isNecessary parent) then
                            failwith "invariant failure"

                        if t.Height >= parent.Height then
                            failwith "invariant failure"

                        FakeUnit.ofUnit ()
                }
                |> parent.Apply
                |> FakeUnit.toUnit
            )

        NodeId.invariant t.Id
        StabilizationNum.invariant t.RecomputedAt

        do
            if NodeHelpers.isValid t && not (isStale t) then
                if t.ValueOpt.IsNone then
                    failwith "invariant failure"

            t.ValueOpt |> ValueOption.iter invA

        do
            Kind.invariant invA t.Kind

            match t.Kind with
            | Kind.Expert e -> Expert.invariantAboutNumInvalidChildren e (NodeHelpers.isNecessary t)
            | _ -> ()

        Cutoff.invariant invA t.Cutoff

        do
            StabilizationNum.invariant t.ChangedAt

            if StabilizationNum.isSome t.RecomputedAt then
                if t.ChangedAt > t.RecomputedAt then
                    failwith "invariant failure"

        do
            let expected =
                List.length t.OnUpdateHandlers
                + foldObservers t 0 (fun n h -> n + List.length h.OnUpdateHandlers)

            if expected <> t.NumOnUpdateHandlers then
                failwith "invariant failure"

        do
            if t.NumParents < 0 then
                failwith "invariant failure"

            if t.NumParents > 1 + t.Parent1AndBeyond.Length then
                failwith "invariant failure"

        do
            for parentIndex = 1 to t.Parent1AndBeyond.Length do
                if (parentIndex < t.NumParents) <> t.Parent1AndBeyond.[parentIndex - 1].IsSome then
                    failwith "invariant failure"

        if (t.NumParents > 0) <> t.Parent0.IsSome then
            failwith "invariant failure"

        Scope.invariant t.CreatedIn

        if Scope.isTop t.CreatedIn || not (NodeHelpers.isValid t) then
            if t.NextNodeInSameScope.IsSome then
                failwith "invariant failure"

        do
            if NodeHelpers.isNecessary t then
                if t.Height < 0 then
                    failwith "invariant failed"
            else if t.Height <> -1 then
                failwith "invariant failed"

        do
            if t.HeightInRecomputeHeap < -1 then
                failwith "invariant failed"

            if t.HeightInRecomputeHeap > t.Height then
                failwith "invariant failed"

        do
            if not (isInRecomputeHeap t) then
                if t.PrevInRecomputeHeap.IsSome then
                    failwith "invariant failed"

            match t.PrevInRecomputeHeap with
            | ValueNone -> ()
            | ValueSome prev ->
                { new NodeEval<_> with
                    member _.Eval prev =
                        if not (packedSame (NodeCrate.make t) prev.NextInRecomputeHeap.Value) then
                            failwith "invariant failure"

                        if t.HeightInRecomputeHeap <> prev.HeightInRecomputeHeap then
                            failwith "invariant failure"

                        FakeUnit.ofUnit ()
                }
                |> prev.Apply
                |> FakeUnit.toUnit

        do
            if not (isInRecomputeHeap t) then
                if t.NextInRecomputeHeap.IsSome then
                    failwith "invariant failed"

            match t.NextInRecomputeHeap with
            | ValueNone -> ()
            | ValueSome next ->
                { new NodeEval<_> with
                    member _.Eval next =
                        if not (packedSame (NodeCrate.make t) next.PrevInRecomputeHeap.Value) then
                            failwith "invariant failure"

                        if t.HeightInRecomputeHeap <> next.HeightInRecomputeHeap then
                            failwith "invariant failure"

                        FakeUnit.ofUnit ()
                }
                |> next.Apply
                |> FakeUnit.toUnit

        do
            if t.HeightInAdjustHeightsHeap >= 0 then
                if t.HeightInAdjustHeightsHeap >= t.Height then
                    failwith "invariant failure"

        do
            if not (isInAdjustHeightsHeap t) then
                if t.NextInAdjustHeightsHeap.IsSome then
                    failwith "invariant failure"
            else
                match t.NextInAdjustHeightsHeap with
                | ValueNone -> ()
                | ValueSome next ->
                    { new NodeEval<_> with
                        member _.Eval next =
                            if not (isInAdjustHeightsHeap next) then
                                failwith "invariant failure"

                            if t.HeightInAdjustHeightsHeap <> next.HeightInAdjustHeightsHeap then
                                failwith "invariant failure"

                            FakeUnit.ofUnit ()
                    }
                    |> next.Apply
                    |> FakeUnit.toUnit

        t.OldValueOpt |> ValueOption.iter invA

        iterObservers
            t
            (fun obs ->
                if not (Type.referenceEqual t obs.Observing) then
                    failwith "invariant failure"

                match obs.State with
                | InternalObserverState.Created
                | InternalObserverState.Unlinked -> failwith "invariant failure"
                | InternalObserverState.InUse
                | InternalObserverState.Disallowed -> ()
            )

        do
            match t.Kind with
            | Kind.Expert _ -> ()
            | _ ->
                if t.MyParentIndexInChildAtIndex.Length <> initialNumChildren t then
                    failwith "invariant failure"

            if NodeHelpers.isNecessary t then
                iteriChildren
                    t
                    (fun childIndex child ->
                        { new NodeEval<_> with
                            member _.Eval child =
                                if
                                    not (
                                        packedSame
                                            (NodeCrate.make t)
                                            (getParent child t.MyParentIndexInChildAtIndex.[childIndex])
                                    )
                                then
                                    failwith "invariant failure"

                                FakeUnit.ofUnit ()
                        }
                        |> child.Apply
                        |> FakeUnit.toUnit
                    )

            if Debug.globalFlag && not (NodeHelpers.isNecessary t) then
                for x in t.MyParentIndexInChildAtIndex do
                    if x <> -1 then
                        failwith "invariant failure"

        do
            if t.MyChildIndexInParentAtIndex.Length <> t.Parent1AndBeyond.Length + 1 then
                failwith "invariant failure"

            iteriParents
                t
                (fun parentIndex parent ->
                    { new NodeEval<_> with
                        member _.Eval parent =
                            if
                                not (
                                    packedSame
                                        (NodeCrate.make t)
                                        (Kind.slowGetChild parent.Kind t.MyChildIndexInParentAtIndex.[parentIndex])
                                )
                            then
                                failwith "invariant failure"

                            FakeUnit.ofUnit ()
                    }
                    |> parent.Apply
                    |> FakeUnit.toUnit
                )

            if Debug.globalFlag && not (NodeHelpers.isNecessary t) then
                for x in t.MyChildIndexInParentAtIndex do
                    if x <> -1 then
                        failwith "invariant failure"

    let maxNumParents (t : Node<'a>) : int = 1 + t.Parent1AndBeyond.Length

    let makeSpaceForParentIfNecessary t =
        if t.NumParents = maxNumParents t then
            let newMaxNumParents = 2 * maxNumParents t

            t.Parent1AndBeyond <-
                let realloc = Array.zeroCreate (newMaxNumParents - 1)
                Array.blit t.Parent1AndBeyond 0 realloc 0 t.Parent1AndBeyond.Length
                realloc

            t.MyChildIndexInParentAtIndex <-
                let realloc = Array.create newMaxNumParents -1
                Array.blit t.MyChildIndexInParentAtIndex 0 realloc 0 t.MyChildIndexInParentAtIndex.Length
                realloc

        if Debug.globalFlag then
            if t.NumParents >= maxNumParents t then
                failwith "invariant failure"

    let makeSpaceForChildIfNecessary t childIndex =
        let maxNumChildren = Array.length t.MyParentIndexInChildAtIndex

        if childIndex >= maxNumChildren then
            if Debug.globalFlag then
                if childIndex <> maxNumChildren then
                    failwith "invariant failure"

            let newMaxNumChildren = max 2 (2 * maxNumChildren)

            t.MyParentIndexInChildAtIndex <-
                let realloc = Array.create newMaxNumChildren -1
                Array.blit t.MyParentIndexInChildAtIndex 0 realloc 0 t.MyParentIndexInChildAtIndex.Length
                realloc

        if Debug.globalFlag then
            if childIndex >= t.MyParentIndexInChildAtIndex.Length then
                failwith "invariant failure"

    let setParent<'a> (child : 'a Node) (parent : NodeCrate voption) (parentIndex : int) : unit =
        if parentIndex = 0 then
            child.Parent0 <- parent
        else
            child.Parent1AndBeyond.[parentIndex - 1] <- parent

    let link<'a, 'b> (child : 'a Node) (childIndex : int) (parent : 'b Node) (parentIndex : int) : unit =
        setParent child (ValueSome (NodeCrate.make parent)) parentIndex
        child.MyChildIndexInParentAtIndex.[parentIndex] <- childIndex
        parent.MyParentIndexInChildAtIndex.[childIndex] <- parentIndex

    let unlink<'a, 'b> (child : 'a Node) (childIndex : int) (parent : 'b Node) (parentIndex : int) : unit =
        setParent child ValueNone parentIndex

        if Debug.globalFlag then
            child.MyChildIndexInParentAtIndex.[parentIndex] <- -1
            parent.MyParentIndexInChildAtIndex.[childIndex] <- -1

    let addParent<'a, 'b> (child : 'a Node) (parent : 'b Node) (childIndex : int) : unit =
        makeSpaceForParentIfNecessary child
        makeSpaceForChildIfNecessary parent childIndex
        link child childIndex parent child.NumParents
        child.NumParents <- child.NumParents + 1

    let removeParent<'a, 'b> (child : 'a Node) (parent : 'b Node) (childIndex : int) : unit =
        if Debug.globalFlag then
            if child.NumParents < 1 then
                failwith "invariant failure"

        let parentIndex = parent.MyParentIndexInChildAtIndex.[childIndex]

        if Debug.globalFlag then
            assert (packedSame (NodeCrate.make parent) (getParent child parentIndex))

        let lastParentIndex = child.NumParents - 1

        if parentIndex < lastParentIndex then
            { new NodeEval<_> with
                member _.Eval parent =
                    link child child.MyChildIndexInParentAtIndex.[lastParentIndex] parent parentIndex
                    |> FakeUnit.ofUnit
            }
            |> child.Parent1AndBeyond.[lastParentIndex - 1].Value.Apply
            |> FakeUnit.toUnit

        unlink child childIndex parent lastParentIndex
        child.NumParents <- child.NumParents - 1

    let swapChildrenExceptInKind parent child1 childIndex1 child2 childIndex2 : unit =
        if Debug.globalFlag then
            assert (packedSame (NodeCrate.make child1) (Kind.slowGetChild parent.Kind childIndex1))
            assert (packedSame (NodeCrate.make child2) (Kind.slowGetChild parent.Kind childIndex2))

        let indexOfParentInChild1 = parent.MyParentIndexInChildAtIndex.[childIndex1]
        let indexOfParentInChild2 = parent.MyParentIndexInChildAtIndex.[childIndex2]

        if Debug.globalFlag then
            assert (child1.MyChildIndexInParentAtIndex.[indexOfParentInChild1] = childIndex1)
            assert (child2.MyChildIndexInParentAtIndex.[indexOfParentInChild2] = childIndex2)

        (* now start swapping *)
        child1.MyChildIndexInParentAtIndex.[indexOfParentInChild1] <- childIndex2
        child2.MyChildIndexInParentAtIndex.[indexOfParentInChild2] <- childIndex1
        parent.MyParentIndexInChildAtIndex.[childIndex1] <- indexOfParentInChild2
        parent.MyParentIndexInChildAtIndex.[childIndex2] <- indexOfParentInChild1

    let create (state : State) (createdIn : Scope) (kind : Kind<'a>) : Node<'a> =
        let t =
            {
                Id = NodeId.next ()
                State = state
                RecomputedAt = StabilizationNum.none
                ValueOpt = ValueNone
                Kind = kind
                Cutoff = Cutoff.physEqual
                ChangedAt = StabilizationNum.none
                NumOnUpdateHandlers = 0
                NumParents = 0
                Parent1AndBeyond = [||]
                Parent0 = ValueNone
                CreatedIn = createdIn
                NextNodeInSameScope = ValueNone
                Height = -1
                HeightInRecomputeHeap = -1
                PrevInRecomputeHeap = ValueNone
                NextInRecomputeHeap = ValueNone
                HeightInAdjustHeightsHeap = -1
                NextInAdjustHeightsHeap = ValueNone
                OldValueOpt = ValueNone
                Observers = ValueNone
                IsInHandleAfterStabilization = false
                OnUpdateHandlers = []
                MyParentIndexInChildAtIndex = Array.create (Kind.initialNumChildren kind) -1
                (* [my_child_index_in_parent_at_index] has one element because it may need to hold
           the child index of [parent0]. *)
                MyChildIndexInParentAtIndex = [| -1 |]
                ForceNecessary = false
                UserInfo = None
                CreationBacktrace =
                    (if state.KeepNodeCreationBacktrace then
                         Some Environment.StackTrace
                     else
                         None)
            }

        Scope.addNode createdIn t
        // [invariant] does not yet hold here because many uses of [Node.create] use [kind = Uninitialized], and then mutate [t.kind] later.
        t

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal NodeCrate =
    open System.Collections.Generic

    let private invariantEval : NodeEval<FakeUnit> =
        { new NodeEval<_> with
            member _.Eval x =
                Node.invariant ignore x |> FakeUnit.ofUnit
        }

    let invariant (t : NodeCrate) =
        t.Apply invariantEval |> FakeUnit.toUnit

    type AsList =
        {
            Next : NodeCrate -> NodeCrate voption
        }

    module AsList =
        let fold (m : AsList) (t : NodeCrate voption) (init : 'acc) (f : 'acc -> NodeCrate -> 'acc) : 'acc =
            let mutable ac = init
            let mutable r = t

            while r.IsSome do
                let packedNode = r.Value
                r <- m.Next packedNode
                ac <- f ac packedNode

            ac

        let iter (m : AsList) (t : NodeCrate voption) (f : NodeCrate -> unit) : unit = fold m t () (fun () n -> f n)

        let length (m : AsList) (t : NodeCrate voption) : int = fold m t 0 (fun n _ -> n + 1)

        let toList (m : AsList) (t : NodeCrate voption) : NodeCrate list =
            fold m t [] (fun ac n -> n :: ac) |> List.rev

        let invariant (m : AsList) (t : NodeCrate voption) = iter m t invariant

    let iterDescendantsInternal (ts : NodeCrate list) (f : NodeCrate -> unit) : HashSet<NodeId> =
        let seen = HashSet<NodeId> ()

        let rec iterDescendants (nodeCrate : NodeCrate) =
            { new NodeEval<_> with
                member _.Eval (t : Node<'a>) =
                    if not (seen.Contains t.Id) then
                        seen.Add t.Id |> ignore
                        f nodeCrate
                        Node.iteriChildren t (fun _ childCrate -> iterDescendants childCrate)

                    FakeUnit.ofUnit ()
            }
            |> nodeCrate.Apply
            |> FakeUnit.toUnit

        List.iter iterDescendants ts
        seen

    let iterDescendants (ts : NodeCrate list) (f : NodeCrate -> unit) : unit = iterDescendantsInternal ts f |> ignore

    let appendUserInfoGraphviz (nodeCrate : NodeCrate) (label : string list) (attrs : Map<string, string>) : unit =
        { new NodeEval<_> with
            member _.Eval (t : Node<'a>) =
                Node.appendUserInfoGraphviz t label attrs |> FakeUnit.ofUnit
        }
        |> nodeCrate.Apply
        |> FakeUnit.toUnit

    let private numParentsEval : NodeEval<int> =
        { new NodeEval<_> with
            member _.Eval n = n.NumParents
        }

    let numParents (c : NodeCrate) : int = c.Apply numParentsEval

    let private recomputedAtEval : NodeEval<StabilizationNum> =
        { new NodeEval<_> with
            member _.Eval n = n.RecomputedAt
        }

    let recomputedAt (c : NodeCrate) = c.Apply recomputedAtEval

    let private changedAtEval : NodeEval<StabilizationNum> =
        { new NodeEval<_> with
            member _.Eval n = n.ChangedAt
        }

    let changedAt (c : NodeCrate) = c.Apply changedAtEval

    let private heightEval : NodeEval<int> =
        { new NodeEval<_> with
            member _.Eval n = n.Height
        }

    let height (c : NodeCrate) = c.Apply heightEval

    let iteriChildren (c : NodeCrate) (f : int -> NodeCrate -> unit) : unit =
        { new NodeEval<_> with
            member _.Eval node =
                Node.iteriChildren node f |> FakeUnit.ofUnit
        }
        |> c.Apply
        |> FakeUnit.toUnit

    let private userInfoEval =
        { new NodeEval<_> with
            member _.Eval n = n.UserInfo
        }

    let userInfo (c : NodeCrate) : DotUserInfo option = c.Apply userInfoEval

    let private nodeIdEval =
        { new NodeEval<_> with
            member _.Eval n = n.Id
        }

    let nodeId (c : NodeCrate) : NodeId = c.Apply nodeIdEval

    let private nextInAdjustHeightsHeapEval =
        { new NodeEval<_> with
            member _.Eval n = n.NextInAdjustHeightsHeap
        }

    let nextInAdjustHeightsHeap (n : NodeCrate) : NodeCrate voption = n.Apply nextInAdjustHeightsHeapEval

    let private nextInRecomputeHeapEval =
        { new NodeEval<_> with
            member _.Eval n = n.NextInRecomputeHeap
        }

    let nextInRecomputeHeap (n : NodeCrate) : NodeCrate voption = n.Apply nextInRecomputeHeapEval

    let private heightInAdjustHeightsHeapEval =
        { new NodeEval<_> with
            member _.Eval n = n.HeightInAdjustHeightsHeap
        }

    let heightInAdjustHeightsHeap (n : NodeCrate) : int = n.Apply heightInAdjustHeightsHeapEval

    let private heightInRecomputeHeapEval =
        { new NodeEval<_> with
            member _.Eval n = n.HeightInRecomputeHeap
        }

    let heightInRecomputeHeap (n : NodeCrate) : int = n.Apply heightInRecomputeHeapEval

    let private prevInRecomputeHeapEval =
        { new NodeEval<_> with
            member _.Eval n = n.PrevInRecomputeHeap
        }

    let prevInRecomputeHeap (n : NodeCrate) : NodeCrate voption = n.Apply prevInRecomputeHeapEval

    let private isInRecomputeHeapEval =
        { new NodeEval<_> with
            member _.Eval n = Node.isInRecomputeHeap n
        }

    let isInRecomputeHeap (n : NodeCrate) : bool = n.Apply isInRecomputeHeapEval

    let private needsToBeComputedEval =
        { new NodeEval<_> with
            member _.Eval n = Node.needsToBeComputed n
        }

    let needsToBeComputed (n : NodeCrate) : bool = n.Apply needsToBeComputedEval

    let private isNecessaryEval =
        { new NodeEval<_> with
            member _.Eval n = NodeHelpers.isNecessary n
        }

    let isNecessary (n : NodeCrate) : bool = n.Apply isNecessaryEval

    let private isValidEval =
        { new NodeEval<_> with
            member _.Eval n = NodeHelpers.isValid n
        }

    let isValid (n : NodeCrate) : bool = n.Apply isValidEval

    let private nextNodeInSameScopeEval =
        { new NodeEval<_> with
            member _.Eval n = n.NextNodeInSameScope
        }

    let nextNodeInSameScope (n : NodeCrate) : NodeCrate voption = n.Apply nextNodeInSameScopeEval
