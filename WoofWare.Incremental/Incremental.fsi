namespace WoofWare.Incremental

open WoofWare.TimingWheel

type Update<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

type Observer<'a>

[<RequireQualifiedAccess>]
module Observer =
    val disallowFutureUse<'a> : 'a Observer -> unit
    val value<'a> : 'a Observer -> Result<'a, exn>
    val valueThrowing<'a> : 'a Observer -> 'a
    val onUpdateThrowing<'a> : 'a Observer -> (Update<'a> -> unit) -> unit

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
    abstract Watch<'a> : Var<'a> -> Node<'a>
    abstract Set<'a> : Var<'a> -> 'a -> unit
    abstract Replace<'a> : Var<'a> -> ('a -> 'a) -> unit
    abstract Value<'a> : Var<'a> -> 'a
    abstract LatestValue<'a> : Var<'a> -> 'a

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
    abstract State : State
    abstract SaveDot : writeChunk : (string -> unit) -> unit
    abstract SaveDot' : renderBindEdges : bool -> writeChunk : (string -> unit) -> unit
    abstract CurrentScope : Scope
    abstract WithinScope : Scope -> (unit -> 'a) -> 'a

[<RequireQualifiedAccess>]
module Incremental =
    val make : unit -> Incremental
