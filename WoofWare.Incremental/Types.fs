namespace WoofWare.PlayFetch

open System
open System.Collections.Concurrent
open System.Runtime.ExceptionServices
open TypeEquality
open WoofWare.TimingWheel
open WoofWare.WeakHashTable

type Status =
    | Stabilizing
    | Running_on_update_handlers
    | Not_stabilizing
    | Stabilize_previously_raised of RaisedException

type AdjustHeightsHeap =
    {
        mutable Length : int
        mutable HeightLowerBound : int
        mutable MaxHeightSeen : int
        mutable NodesByHeight : NodeCrate option array
    }

and Alarm = TimingWheel<AlarmValue>

and AlarmValueAction =
    | At of At
    | AtIntervals of AtIntervals
    | Snapshot of SnapshotCrate
    | StepFunction of StepFunctionNodeCrate

and AlarmValue =
    {
        Action : AlarmValueAction
        mutable NextFired : Option<AlarmValue>
    }

and ArrayFold<'a, 'acc> =
    {
        Init : 'acc
        F : 'acc -> 'a -> 'acc
        Children : 'a Node[]
    }

and At =
    {
        Main : Node<BeforeOrAfter>
        At : TimeNs
        mutable Alarm : Alarm
        Clock : Clock
    }

and AtIntervals =
    {
        Main : Node<unit>
        Base : TimeNs
        Interval : TimeSpan
        Alarm : Alarm
        Clock : Clock
    }

and Bind<'a, 'b> =
    {
        Main : Node<'b>
        /// [f] is the user-supplied function that we run each time [t.lhs] changes.  It is
        /// mutable only so we can clear it when [t] is invalidated.
        mutable F : 'a -> Node<'b>
        Lhs : 'a Node
        LhsChange : unit Node
        /// [rhs] is initially [none], and after that is [some] of the result of the most recent
        /// call to [f].
        mutable Rhs : 'b Node voption
        /// [rhs_scope] is the scope in which [t.f] is run, i.e. it is [Scope.Bind t].  It is
        /// [mutable] only to avoid a [let rec] during creation.
        mutable RhsScope : Scope
        /// [all_nodes_created_on_rhs] is the head of the singly-linked list of nodes created on
        /// the right-hand side of [t], i.e. in [t.rhs_scope].
        mutable AllNodesCreatedOnRhs : NodeCrate voption
    }

and Clock =
    {
        TimingWheel : TimingWheel<AlarmValue>
        Now : Var<TimeNs>
        HandleFired : Alarm -> unit
        mutable FiredAlarmValues : AlarmValue option
    }

and ExpertEdge<'a> =
    {
        Child : 'a Node
        OnChange : 'a -> unit
        mutable Index : int voption
    }

and ExpertEdgeEval<'ret> =
    abstract Eval<'a> : 'a ExpertEdge -> 'ret

and ExpertEdgeCrate =
    abstract Apply : ExpertEdgeEval<'ret> -> 'ret

and Expert<'a> =
    {
        F : unit -> 'a
        /// bool is `is_now_observable`
        OnObservabilityChange : bool -> unit
        mutable Children : ExpertEdgeCrate voption[]
        mutable NumChildren : int
        mutable ForceStale : bool
        mutable NumInvalidChildren : int
        mutable WillFireAllCallbacks : bool
    }

and Freeze<'a> =
    {
        Main : Node<'a>
        Child : Node<'a>
        OnlyFreezeWhen : 'a -> bool
    }

and IfThenElse<'a> =
    {
        Main : Node<'a>
        Test : Node<bool>
        TestChange : Node<unit>
        mutable CurrentBranch : Node<'a> voption
        Then : Node<'a>
        Else : Node<'a>
    }

and InternalObserverState =
    | Created
    | InUse
    | Disallowed
    | Unlinked

and InternalObserver<'a> =
    {
        mutable State : State
        Observing : Node<'a>
        mutable OnUpdateHandlers : 'a OnUpdateHandler list
        mutable PrevInAll : InternalObserverCrate option
        mutable NextInAll : InternalObserverCrate option
        mutable PrevInObserving : InternalObserver<'a> option
        mutable NextInObserving : InternalObserver<'a> option
    }

and InternalObserverEval<'ret> =
    abstract Eval<'a> : 'a InternalObserver -> 'ret

and InternalObserverCrate =
    abstract Apply : InternalObserverEval<'ret> -> 'ret

and Join<'a> =
    {
        Main : 'a Node
        Lhs : 'a Node Node
        LhsChange : unit Node
        mutable Rhs : 'a Node voption
    }

and ArrayFoldEval<'a, 'ret> =
    abstract Eval<'b> : ArrayFold<'b, 'a> -> 'ret

and ArrayFoldCrate<'a> =
    abstract Apply<'ret> : ArrayFoldEval<'a, 'ret> -> 'ret

and BindEval<'ret> =
    abstract Eval<'a, 'b> : Bind<'a, 'b> -> 'ret

and BindCrate =
    abstract Apply<'ret> : BindEval<'ret> -> 'ret

and BindMainEval<'a, 'ret> =
    abstract Eval<'b> : Bind<'a, 'b> -> 'ret

and BindMainCrate<'a> =
    abstract Apply<'ret> : BindMainEval<'a, 'ret> -> 'ret

and IfThenElseEval<'ret> =
    abstract Eval<'a> : IfThenElse<'a> -> 'ret

and IfThenElseCrate =
    abstract Apply<'ret> : IfThenElseEval<'ret> -> 'ret

and JoinEval<'ret> =
    abstract Eval<'a> : Join<'a> -> 'ret

and JoinCrate =
    abstract Apply<'ret> : JoinEval<'ret> -> 'ret

and MapEval<'a, 'ret> =
    abstract Eval<'a1> : ('a1 -> 'a) * 'a1 Node -> 'ret

and MapCrate<'a> =
    abstract Apply<'ret> : MapEval<'a, 'ret> -> 'ret

and UnorderedArrayFoldEval<'a, 'ret> =
    abstract Eval<'b> : UnorderedArrayFold<'b, 'a> -> 'ret

and UnorderedArrayFoldCrate<'a> =
    abstract Apply<'ret> : UnorderedArrayFoldEval<'a, 'ret> -> 'ret

and Map2Eval<'a, 'ret> =
    abstract Eval<'a1, 'a2> : ('a1 -> 'a2 -> 'a) * 'a1 Node * 'a2 Node -> 'ret

and Map2Crate<'a> =
    abstract Apply<'ret> : Map2Eval<'a, 'ret> -> 'ret

and Kind<'a> =
    | ArrayFold of ArrayFoldCrate<'a>
    | At of At * Teq<'a, BeforeOrAfter>
    | AtIntervals of AtIntervals * Teq<'a, unit>
    | BindLhsChange of BindCrate * Teq<'a, unit>
    | BindMain of BindMainCrate<'a>
    | Const of 'a
    | Expert of Expert<'a>
    | Freeze of Freeze<'a>
    | IfTestChange of IfThenElseCrate * Teq<'a, unit>
    | IfThenElse of IfThenElse<'a>
    | Invalid
    | JoinLhsChange of JoinCrate * Teq<'a, unit>
    | JoinMain of Join<'a>
    | Map of MapCrate<'a>
    | Snapshot of 'a Snapshot
    | StepFunction of 'a StepFunctionNode
    | Uninitialized
    | UnorderedArrayFold of UnorderedArrayFoldCrate<'a>
    | Var of Var<'a>
    | Map2 of Map2Crate<'a>

and Node<'a> =
    internal
        {
            /// A unique ID for the node.
            Id : NodeId
            State : State
            /// The fields from [recomputed_at] to [created_in] are grouped together and are in the
            /// same order as they are used by [State.recompute] This has a positive performance
            /// impact due to cache effects.  Don't change the order of these nodes without
            /// performance testing.
            /// [recomputed_at] is the last stabilization when [t]'s value was recomputed, even if
            /// it was cut off.
            mutable RecomputedAt : StabilizationNum
            /// [value_opt] starts as [none], and the first time [t] is computed it is set to
            /// [some], and remains [some] thereafter, until [t] is invalidated, if ever.
            mutable ValueOpt : 'a voption
            /// [kind] is the kind of DAG node [t] is.  [kind] is mutable both for initialization
            /// and because it can change, e.g. if [t] is invalidated.
            mutable Kind : 'a Kind
            mutable Cutoff : 'a Cutoff
            /// [changed_at] is the last stabilization when this node was computed and not cut off.
            /// It is used to detect when [t]'s parents are stale and (because all parents are
            /// necessary) need to be recomputed.
            mutable ChangedAt : StabilizationNum
            /// [num_on_update_handlers] is [List.length t.on_update_handlers] plus the number of
            /// on-update handlers summed over all observers in [t.observers].  It is used to
            /// quickly decide whether [t] needs to be added to [state.handle_after_stabilization]
            /// when [t] changes.  [num_on_update_handlers] will decrease when an observer is
            /// removed from [t.observers], if the observer has on-update handlers.
            mutable NumOnUpdateHandlers : int
            /// The parents of [t] are the nodes that depend on it, and should be computed when [t]
            /// changes, once all of their other children are up to date.  [num_parents] is the
            /// number of parents.  If [num_parents >= 1], then [parent0] is the first parent.
            /// [parent1_and_beyond] holds the remaining parents.  The order of the parents doesn't
            /// matter.  One node may occur multiple times as a parent of another (e.g. consider
            /// [map2 n1 n1 ~f]).

            /// This representation is optimized for the overwhelmingly common case that a node has
            /// only one parent.
            mutable NumParents : int
            mutable Parent1AndBeyond : NodeCrate voption[]
            mutable Parent0 : NodeCrate option
            /// [created_in] is initially the scope that the node is created in.  If a node is
            /// later "rescoped", then created_in will be adjusted to the new scope that the node
            /// is part of.
            mutable CreatedIn : Scope
            /// [next_node_in_same_scope] singly links all nodes created in [t.created_in].
            mutable NextNodeInSameScope : NodeCrate voption
            /// [height] is used to visit nodes in topological order.  If [is_necessary t], then
            /// [height > c.height] for all children [c] of [t], and [height > Scope.height
            /// t.created_in].  If [not (is_necessary t)], then [height = -1]
            mutable Height : int
            /// [height_in_recompute_heap] is the height at which [t] is stored in the recompute
            /// heap, and is non-negative iff [t] is in the recompute heap.  If [t] is the
            /// recompute heap, then typically [t.height = t.height_in_recompute_heap]; however,
            /// while height is being adjusted, one can temporarily have [t.height >
            /// t.height_in_recompute_heap].  When height adjustment finishes, equality is restored
            /// by increasing [t.height_in_recompute_heap] to [t.height] and shifting [t]'s
            /// position in the recompute heap.
            mutable HeightInRecomputeHeap : int
            /// [prev_in_recompute_heap] and [next_in_recompute_heap] doubly link all nodes of the
            /// same height in the recompute heap.
            mutable PrevInRecomputeHeap : NodeCrate voption
            /// [prev_in_recompute_heap] and [next_in_recompute_heap] doubly link all nodes of the
            /// same height in the recompute heap.
            mutable NextInRecomputeHeap : NodeCrate voption
            /// [height_in_adjust_heights_heap] is used only during height adjustment, and is
            /// non-negative iff [t] is in the adjust-heights heap.  It holds the pre-adjusted
            /// height of [t].
            mutable HeightInAdjustHeightsHeap : int
            /// [next_in_adjust_heights_heap] singly links all nodes of the same height in the
            /// adjust-heights heap.
            mutable NextInAdjustHeightsHeap : NodeCrate voption
            /// [old_value_opt] is used only during stabilization, and only if
            /// [t.num_on_update_handlers > 0].  It holds the pre-stabilization value of [t].  It
            /// is cleared when running [t]'s on-update handlers, and so is always [Uopt.none]
            /// between stabilizations.
            mutable OldValueOpt : 'a voption
            /// [observers] is the head of the doubly-linked list of observers of [t], or
            /// [Uopt.none] if there are no observers.
            mutable Observers : 'a InternalObserver voption
            /// [is_in_handle_after_stabilization] is used to avoid pushing the same node multiple
            /// times onto [state.handle_after_stabilization].
            mutable IsInHandleAfterStabilization : bool
            /// [on_update_handlers] is the functions supplied to [Incremental.on_update] to be run
            /// as described in the module [On_update_handler].  [on_update_handlers] does not
            /// contain the on-update handlers in [t.observers].  [on_update_handlers] only ever
            /// gets longer; there is no way to remove elements.
            mutable OnUpdateHandlers : 'a OnUpdateHandler list
            mutable MyParentIndexInChildAtIndex : int array
            mutable MyChildIndexInParentAtIndex : int array
            mutable ForceNecessary : bool
            mutable UserInfo : DotUserInfo option
            mutable CreationBacktrace : ExceptionDispatchInfo option
        }

and NodeEval<'ret> =
    abstract Eval<'a> : 'a Node -> 'ret

and NodeCrate =
    abstract Apply<'ret> : NodeEval<'ret> -> 'ret

and Observer<'a> = 'a InternalObserver ref

and OnlyInDebug =
    {
        mutable CurrentlyRunningNode : NodeCrate option
        mutable ExpertNodesCreatedByCurrentNode : NodeCrate list
    }

and RecomputeHeap =
    {
        mutable Length : int
        mutable HeightLowerBound : int
        mutable NodesByHeight : NodeCrate voption[]
    }

and RunOnUpdateHandlersEval<'ret> =
    abstract Eval<'a> : 'a Node -> 'a NodeUpdate -> 'ret

and RunOnUpdateHandlers =
    abstract Apply<'ret> : RunOnUpdateHandlersEval<'ret> -> 'ret

and Scope =
    | Top
    | Bind of BindCrate

and Snapshot<'a> =
    {
        Main : 'a Node
        At : TimeNs
        Before : 'a
        ValueAt : 'a Node
        Clock : Clock
    }

and SnapshotEval<'ret> =
    abstract Eval<'a> : Snapshot<'a> -> 'ret

and SnapshotCrate =
    abstract Apply<'ret> : SnapshotEval<'ret> -> 'ret

and State =
    {
        mutable Status : Status
        BindLhsChangeShouldInvalidateRhs : bool
        /// Starts at zer, and is incremented at the end of each stabilization.
        mutable StabilizationNum : StabilizationNum
        mutable CurrentScope : Scope
        RecomputeHeap : RecomputeHeap
        AdjustHeightsHeap : AdjustHeightsHeap
        /// Holds nodes that have invalid children that should be considered for invalidation.
        /// It is only used during graph restructuring: [invalidate_node] and [add_parent].
        /// Once an element is added to the stack, we then iterate until invalidity has propagated to all ancestors
        /// as necessary, according to [Node.should_be_invalidated].
        PropagateInvalidity : NodeCrate Stack
        /// The number of observers whose state is Created or InUse.
        mutable NumActiveObservers : int
        /// The doubly-linked list of all observers in effect, or that have been disallowed since the most recent
        /// start of a stabilization. These have state InUse or Disallowed.
        mutable AllObservers : InternalObserverCrate voption
        /// We enqueue finalized observers in a thread-safe queue, for handling during stabilization.
        /// We use a thread-safe queue because OCaml finalizers can run in any thread.
        FinalizedObservers : InternalObserverCrate ConcurrentQueue
        /// Observers created since the most recent start of a stabilization.
        /// These have state Created or Unlinked.
        /// At the start of stabilization, we link into AllObservers all observers in NewObservers whose state
        /// is Created, and add them to the Observers of the node they are observing.
        /// We structure things this way to allow observers to be created during stabilization
        /// while running user code ([map], [bind], etc), but to not have to deal with nodes
        /// becoming necessary and the graph changing during such code.
        NewObservers : InternalObserverCrate Stack
        /// [disallowed_observers] holds all observers that have been disallowed since the most
        ///    recent start of a stabilization -- these have [state = Disallowed].  At the start
        ///    of stabilization, these are unlinked from [all_observers] and their state is
        ///    changed to [Unlinked].  We structure things this way to allow user code running
        ///    during stabilization to call [disallow_future_use], but to not have to deal with
        ///    nodes becoming unnecessary and the graph changing during such code.
        DisallowedObservers : InternalObserverCrate Stack
        /// We delay all [Var.set] calls that happen during stabilization so that they take
        ///  effect after stabilization.  All variables set during stabilization are pushed on
        ///  [set_during_stabilization] rather than setting them.  Then, after the graph has
        ///  stabilized, we do all the sets, so that they take effect at the start of the next
        ///  stabilization.
        SetDuringStabilization : VarCrate Stack
        /// [handle_after_stabilization] has all nodes with handlers to consider running at the
        ///  end of the next stabilization.  At the end of stabilization, we consider each node
        ///  in [handle_after_stabilization], and if we decide to run its on-update handlers,
        ///  push it on [run_on_update_handlers].  Then, once we've considered all nodes in
        ///  [handle_after_stabilization], we iterate through [run_on_update_handlers] and
        ///  actually run the handlers.

        ///  These two passes are essential for correctness.  During the first pass, we haven't
        ///  run any user handlers, so we know that the state is exactly as it was when
        ///  stabilization finished.  In particular, we know that if a node is necessary, then
        ///  it has a stable value; once user handlers run, we don't know this.  During the
        ///  second pass, user handlers can make calls to any incremental function except for
        ///  [stabilize].  In particular, some functions push nodes on
        ///  [handle_after_stabilization].  But no functions (except for [stabilize]) modify
        ///  [run_on_update_handlers].
        HandleAfterStabilization : NodeCrate Stack
        RunOnUpdateHandlers : RunOnUpdateHandlers Stack
        mutable OnlyInDebug : OnlyInDebug
        WeakHashTables : WeakHashTableCrate ConcurrentQueue
        (* Stats.  These are all incremented at the appropriate place, and never decremented. *)
        mutable KeepNodeCreationBacktrace : bool
        mutable NumNodesBecameNecessary : int
        mutable NumNodesBecameUnnecessary : int
        mutable NumNodesChanged : int
        mutable NumNodesCreated : int
        mutable NumNodesInvalidated : int
        mutable NumNodesRecomputed : int
        mutable NumNodesRecomputedDirectlyBecauseOneChild : int
        mutable NumNodesRecomputedDirectlyBecauseMinHeight : int
        mutable NumVarSets : int
    }

and StepFunctionNode<'a> =
    {
        Main : 'a Node
        mutable Child : 'a StepFunction Node voption
        mutable ExtractedStepFunctionFromChildAt : StabilizationNum
        mutable Value : 'a voption
        mutable UpcomingSteps : (TimeNs * 'a) seq
        mutable Alarm : Alarm
        mutable AlarmValue : AlarmValue
        Clock : Clock
    }

and StepFunctionNodeEval<'ret> =
    abstract Eval<'a> : StepFunctionNode<'a> -> 'ret

and StepFunctionNodeCrate =
    abstract Apply<'ret> : StepFunctionNodeEval<'ret> -> 'ret

and UnorderedArrayFold<'a, 'acc> =
    {
        Main : 'acc Node
        Init : 'acc
        F : 'acc -> 'a -> 'acc
        /// old value first, then new value
        Update : 'acc -> 'a -> 'a -> 'acc
        FullComputeEveryNChanges : int
        Children : 'a Node[]
        mutable FoldValue : 'acc voption
        mutable NumChangesSinceLastFullCompute : int
    }

and Var<'a> =
    {
        mutable Value : 'a
        mutable ValueSetDuringStabilization : 'a option
        mutable SetAt : StabilizationNum
        Watch : 'a Node
    }

and VarEval<'ret> =
    abstract Eval<'a> : Var<'a> -> 'ret

and VarCrate =
    abstract Apply<'ret> : VarEval<'ret> -> 'ret

[<RequireQualifiedAccess>]
module NodeCrate =
    let make (node : Node<'a>) =
        { new NodeCrate with
            member _.Apply e = e.Eval node
        }

[<RequireQualifiedAccess>]
module BindCrate =
    let make (bind : Bind<'a, 'b>) =
        { new BindCrate with
            member _.Apply e = e.Eval bind
        }
