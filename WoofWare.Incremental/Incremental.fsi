namespace WoofWare.Incremental

open WoofWare.TimingWheel

type Observer<'a>

type IClock =
    abstract DefaultTimingWheelConfig : TimingWheelConfig
    abstract Create' : TimingWheelConfig -> TimeNs -> Clock
    abstract Create : TimeNs -> Clock
    abstract At : Clock -> TimeNs -> Node<BeforeOrAfter>
    abstract AdvanceClock : Clock -> TimeNs -> unit

type IVar =
    abstract Create<'a> : 'a -> Var<'a>
    abstract Watch<'a> : Var<'a> -> Node<'a>
    abstract Set<'a> : Var<'a> -> 'a -> unit

type Incremental =
    abstract Return<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    abstract Var : IVar
    abstract Clock : IClock
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract State : State

[<RequireQualifiedAccess>]
module Incremental =
    val make : unit -> Incremental
