namespace WoofWare.Incremental

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
type Update<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

/// An entity which is observing a node in an Incremental graph, and can tell you e.g. the value of the node.
type Observer<'a>

/// An entity which is observing a node in an Incremental graph, and can tell you e.g. the value of the node.
[<RequireQualifiedAccess>]
module Observer =
    type Update<'a> =
        | Initialized of 'a
        | Changed of 'a * 'a
        | Invalidated

    val disallowFutureUse<'a> : 'a Observer -> unit
    val useIsAllowed<'a> : 'a Observer -> bool
    val value'<'a> : 'a Observer -> Result<'a, exn>
    val value<'a> : 'a Observer -> 'a
    val onUpdateThrowing<'a> : 'a Observer -> (Update<'a> -> unit) -> unit
    /// The Incremental DAG node which this observer is observing.
    val observing<'a> : 'a Observer -> 'a Node
    /// Human-readable string representation, showing the value of the observer (if it's in a valid state)
    /// or the observer's state (if the state is not valid for observation).
    val toString<'a> : 'a Observer -> string

type IClock =
    abstract DefaultTimingWheelConfig : TimingWheelConfig
    abstract Create' : TimingWheelConfig -> TimeNs -> Clock
    abstract Create : TimeNs -> Clock
    abstract After : Clock -> TimeNs.Span -> Node<BeforeOrAfter>
    abstract At : Clock -> TimeNs -> Node<BeforeOrAfter>
    abstract AtIntervals : Clock -> TimeNs.Span -> Node<unit>
    abstract AdvanceClock : Clock -> TimeNs -> unit
    abstract AdvanceClockBy : Clock -> TimeNs.Span -> unit
    abstract Snapshot<'a> : Clock -> Node<'a> -> at : TimeNs -> before : 'a -> Result<Node<'a>, string>
    abstract WatchNow : Clock -> Node<TimeNs>
    abstract AlarmPrecision : Clock -> TimeNs.Span
    abstract NextAlarmFiresAt : Clock -> TimeNs voption
    abstract StepFunction : Clock -> init : 'a -> (TimeNs * 'a) list -> Node<'a>

    abstract IncrementalStepFunction<'a> : Clock -> StepFunction<'a> Node -> 'a Node

type IVar =
    abstract Create<'a> : 'a -> Var<'a>
    abstract Create'<'a> : useCurrentScope : bool -> 'a -> Var<'a>
    abstract Watch<'a> : Var<'a> -> Node<'a>
    abstract Set<'a> : Var<'a> -> 'a -> unit
    abstract Replace<'a> : Var<'a> -> ('a -> 'a) -> unit
    abstract Value<'a> : Var<'a> -> 'a
    abstract LatestValue<'a> : Var<'a> -> 'a

type IExpertIncremental =
    abstract DoOneStepOfStabilize : unit -> StepResult

type Incremental =
    /// An incremental whose value never changes; this is a synonym for Const.
    abstract Return<'a> : 'a -> Node<'a>
    /// An incremental whose value never changes; this is a synonym for Return.
    abstract Const<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    /// Functorial map. You should not create incremental nodes within the input `f`;
    /// if you want to create nodes, use Bind, because the invalidation machinery of Bind is not
    /// used in Map.
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    /// Applicative map2. You should not create incremental nodes within the input `f`;
    /// if you want to create nodes, use Bind, because the invalidation machinery of Bind is not
    /// used in Map.
    abstract Map2<'a, 'b, 'c> : ('a -> 'b -> 'c) -> Node<'a> -> Node<'b> -> Node<'c>
    /// Applicative zip.
    ///
    /// Note that `Map f (Both t1 t2)` and `Map2 (fun a1 a2 -> f (a1, a2)) t1 t2` have the same behaviour,
    /// but the `Map2` version is more efficient because it creates a single node; the `Both` version creates two
    /// nodes.
    abstract Both<'a, 'b> : 'a Node -> 'b Node -> ('a * 'b) Node
    /// Monadic join.
    abstract Join<'a> : 'a Node Node -> 'a Node
    abstract Var : IVar
    abstract Clock : IClock
    /// Monadic bind. Behaves like `f v`; when the input changes, we apply `f` to the new value
    /// and recompute the structure and contents of the graph.
    ///
    /// `Bind` nodes are significantly more expensive than `Map` during stabilization, because
    /// they require modifying the shape of the DAG. Use only the applicative/functorial `Map2` and `Map` where you can.
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract Observe'<'a> : shouldFinalize : bool -> Node<'a> -> Observer<'a>
    /// The underlying state of the Incremental. You should not need to deal with this type, except perhaps to
    /// pass it to visualization functions; it is opaque.
    abstract State : State
    abstract SaveDot : writeChunk : (string -> unit) -> unit
    abstract SaveDot' : stableNodeIds : bool -> renderBindEdges : bool -> writeChunk : (string -> unit) -> unit
    abstract CurrentScope : Scope
    abstract Expert : IExpertIncremental
    abstract WithinScope : Scope -> (unit -> 'a) -> 'a
    abstract OnUpdate<'a> : 'a Node -> (NodeUpdate<'a> -> unit) -> unit
    abstract GetCutoff<'a> : 'a Node -> 'a Cutoff
    abstract SetCutoff<'a> : 'a Cutoff -> 'a Node -> unit
    abstract AmStabilizing : bool
    /// Returns a node that has the same value and cutoff as the input, such that as long as the output is alive,
    /// the input is necessary.
    abstract NecessaryIfAlive<'a> : 'a Node -> 'a Node
    /// An incremental whose value is the input until the first stabilization.
    /// From that point onward, the freeze node's value becomes constant for all time.
    ///
    /// Calling `Freeze` on a node forces that node to be necessary until it freezes, regardless of whether
    /// the Freeze node is necessary; but once the Freeze node has frozen, the input is no longer necessary (though
    /// other parts of the graph could cause it still to be necessary).
    ///
    /// The result of Freeze, once frozen, will never be invalidated, even if the input is invalidated, and even if
    /// the scope in which the freeze was created is invalidated.
    /// However, prior to first stabilization, `freeze` can be invalidated.
    abstract Freeze<'a> : 'a Node -> 'a Node
    /// An incremental whose value is the input until the first stabilization in which `onlyWhen` holds.
    /// From that point onward, the freeze node's value becomes constant for all time.
    ///
    /// Calling `Freeze` on a node forces that node to be necessary until it freezes, regardless of whether
    /// the Freeze node is necessary; but once the Freeze node has frozen, the input is no longer necessary (though
    /// other parts of the graph could cause it still to be necessary).
    ///
    /// The result of Freeze, once frozen, will never be invalidated, even if the input is invalidated, and even if
    /// the scope in which the freeze was created is invalidated.
    /// However, prior to `onlyWhen` becoming true, `freeze` can be invalidated.
    abstract Freeze'<'a> : onlyWhen : ('a -> bool) -> 'a Node -> 'a Node
    /// Returns a node `output` whose value is the same as the input value, but also such that `dependOn` is necessary
    /// so long as `output` is necessary.
    /// It is like `map2 (fun a _ -> a) input dependOn`, but with a cutoff so that `output`'s value only changes when
    /// the input value changes, not merely when `dependOn` changes.
    abstract DependOn<'a, 'b> : dependOn : 'a Node -> 'b Node -> 'b Node
    /// Traversable sequence. In any stabilization where any of the inputs changes, the entire list is recreated (once
    /// all the inputs have stabilized). This is essentially an ArrayFold over the inputs.
    abstract All<'a> : 'a Node list -> 'a list Node
    /// An incremental that is true iff all the inputs are true.
    abstract ForAll : bool Node[] -> bool Node
    /// An incremental that is true iff at least one input is true.
    abstract Exists : bool Node[] -> bool Node
    /// <summary>Sets the maximum allowed height of nodes.</summary>
    /// <exception cref="Exception">Raises if called during stabilization, or if the input height is less than the max height we've already seen.</exception>
    abstract SetMaxHeightAllowed : int -> unit
    /// The maximum height of any node in this graph.
    /// This is a constant-time-access counter that is automatically updated during execution anyway.
    abstract MaxHeightAllowed : int

    /// Like `Lazy.Create`, except that the nodes created within the function will be created in the scope in which
    /// `LazyFromFun` was called, rather than in the scope of the piece of code that first forces the resulting Lazy.
    ///
    /// Not using this function when defining lazy values is likely to result in exceptions being thrown by Incremental.
    /// As a rule of thumb, all Lazy that might create incremental nodes should be built with `LazyFromFun`.
    ///
    /// As usual with `Lazy`, if the function raises, then that exception will be raised when calling `Lazy.Force`.
    abstract LazyFromFun<'a> : (unit -> 'a) -> 'a Lazy

    /// An incremental whose value is `Array.fold init (fun acc t -> f acc (value t)) ts`.
    ///
    /// In a stabilization during which any of the input nodes changes, the entire fold will be computed once all
    /// the inputs have been computed.
    abstract ArrayFold<'a, 'acc> : init : 'acc -> f : ('acc -> 'a -> 'acc) -> 'a Node[] -> Node<'acc>

    abstract UnorderedArrayFold<'a, 'acc> :
        init : 'acc -> f : ('acc -> 'a -> 'acc) -> update : FoldUpdate<'a, 'acc> -> 'a Node[] -> Node<'acc>

    abstract UnorderedArrayFold'<'a, 'acc> :
        fullUpdateEveryNUpdates : int ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        update : FoldUpdate<'a, 'acc> ->
        'a Node[] ->
            Node<'acc>

    abstract OptUnorderedArrayFold<'a, 'acc> :
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
        'a option Node[] ->
            Node<'acc option>

    abstract OptUnorderedArrayFold'<'a, 'acc> :
        fullUpdateEveryNUpdates : int ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
        'a option Node[] ->
            Node<'acc option>

    abstract VoptUnorderedArrayFold<'a, 'acc> :
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
        'a voption Node[] ->
            Node<'acc voption>

    abstract VoptUnorderedArrayFold'<'a, 'acc> :
        fullUpdateEveryNUpdates : int ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
        'a voption Node[] ->
            Node<'acc voption>

    abstract Sum<'a, 'b> :
        fullComputeEveryNChanges : int option ->
        zero : 'b ->
        add : ('b -> 'a -> 'b) ->
        sub : ('b -> 'a -> 'b) ->
        Node<'a>[] ->
            Node<'b>

    abstract OptSum<'a, 'b> :
        fullComputeEveryNChanges : int option ->
        zero : 'b ->
        add : ('b -> 'a -> 'b) ->
        sub : ('b -> 'a -> 'b) ->
        Node<'a voption>[] ->
            Node<'b voption>

    /// Selective If. The resulting node only depends on one of `trueCase` and `falseCase` at a time.
    /// This is more efficient than a full-blown `Bind`, and more efficient than zipping the three Nodes together
    /// (because that would have a dependency on all three nodes at once).
    abstract If<'a> : Node<bool> -> trueCase : Node<'a> -> falseCase : Node<'a> -> Node<'a>

    /// A fold-like operation over the input nodes, much like ArrayFold. Unlike ArrayFold, the operation will be
    /// computed in `O(min(n, k log(n))` time, where `n` is the length of the node array and `k` is the number of
    /// elements that have changed since the last stabilization.
    ///
    /// Generally, if most or all of the inputs are changing between stabilizations, using ArrayFold will have
    /// better constant factors.
    ///
    /// `reduce` must be associative: `reduce a (reduce b c) = (reduce a b) c`.
    ///
    /// `None` is returned iff you supply an empty array.
    abstract ReduceBalanced<'a, 'b> : f : ('a -> 'b) -> reduce : ('b -> 'b -> 'b) -> Node<'a>[] -> Node<'b> option

[<RequireQualifiedAccess>]
module Incremental =
    /// Create the empty Incremental graph.
    val make : unit -> Incremental
