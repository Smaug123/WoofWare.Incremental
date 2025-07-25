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
    val value<'a> : 'a Observer -> Result<'a, exn>
    val valueThrowing<'a> : 'a Observer -> 'a
    val onUpdateThrowing<'a> : 'a Observer -> (Update<'a> -> unit) -> unit
    val observing<'a> : 'a Observer -> 'a Node
    val toString<'a> : 'a Observer -> string

type IClock =
    abstract DefaultTimingWheelConfig : TimingWheelConfig
    abstract Create' : TimingWheelConfig -> TimeNs -> Clock
    abstract Create : TimeNs -> Clock
    abstract At : Clock -> TimeNs -> Node<BeforeOrAfter>
    abstract AtIntervals : Clock -> TimeNs.Span -> Node<unit>
    abstract AdvanceClock : Clock -> TimeNs -> unit
    abstract AdvanceClockBy : Clock -> TimeNs.Span -> unit
    abstract Snapshot<'a> : Clock -> Node<'a> -> at : TimeNs -> before : 'a -> Result<Node<'a>, string>

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
    abstract Return<'a> : 'a -> Node<'a>
    abstract Const<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    abstract Map2<'a, 'b, 'c> : ('a -> 'b -> 'c) -> Node<'a> -> Node<'b> -> Node<'c>
    abstract Both<'a, 'b> : 'a Node -> 'b Node -> ('a * 'b) Node
    abstract Join<'a> : 'a Node Node -> 'a Node
    abstract Var : IVar
    abstract Clock : IClock
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract Observe'<'a> : shouldFinalize : bool -> Node<'a> -> Observer<'a>
    abstract State : State
    abstract SaveDot : writeChunk : (string -> unit) -> unit
    abstract SaveDot' : stableNodeIds : bool -> renderBindEdges : bool -> writeChunk : (string -> unit) -> unit
    abstract CurrentScope : Scope
    abstract Expert : IExpertIncremental
    abstract WithinScope : Scope -> (unit -> 'a) -> 'a
    abstract OnUpdate<'a> : 'a Node -> (NodeUpdate<'a> -> unit) -> unit
    abstract SetCutoff<'a> : 'a Node -> 'a Cutoff -> unit
    abstract AmStabilizing : bool
    abstract NecessaryIfAlive<'a> : 'a Node -> 'a Node
    abstract Freeze<'a> : 'a Node -> 'a Node
    abstract Freeze'<'a> : onlyWhen : ('a -> bool) -> 'a Node -> 'a Node
    abstract DependOn<'a, 'b> : dependOn : 'a Node -> 'b Node -> 'b Node
    abstract All<'a> : 'a Node list -> 'a list Node
    abstract ForAll : bool Node[] -> bool Node
    abstract Exists : bool Node[] -> bool Node
    abstract SetMaxHeightAllowed : int -> unit
    abstract MaxHeightAllowed : int

    abstract OptUnorderedArrayFold<'a, 'acc> :
        'a option Node[] ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
            Node<'acc option>

    abstract OptUnorderedArrayFold'<'a, 'acc> :
        fullUpdateEveryNUpdates : int ->
        'a option Node[] ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
            Node<'acc option>

    abstract VoptUnorderedArrayFold<'a, 'acc> :
        'a voption Node[] ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
            Node<'acc voption>

    abstract VoptUnorderedArrayFold'<'a, 'acc> :
        fullUpdateEveryNUpdates : int ->
        'a voption Node[] ->
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        fInverse : ('acc -> 'a -> 'acc) ->
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

    abstract If<'a> : Node<bool> -> trueCase : Node<'a> -> falseCase : Node<'a> -> Node<'a>

[<RequireQualifiedAccess>]
module Incremental =
    /// Create the empty Incremental graph.
    val make : unit -> Incremental
