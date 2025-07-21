namespace WoofWare.Incremental

open WoofWare.TimingWheel

type Observer<'a> = | Observer of Observer'<'a>

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

type IncrementalImpl (state : State) =
    let var =
        { new IVar with
            member this.Create x = State.createVar state None x
            member this.Watch v = v.Watch
            member this.Set var a = State.setVar var a
        }

    let clock =
        let defaultConfig =
            let alarmPrecision = AlarmPrecision.aboutOneMillisecond
            let levelBits = [ 14 ; 13 ; 5 ] |> LevelBits.createThrowing' true
            TimingWheelConfig.create None levelBits alarmPrecision

        let create (config : TimingWheelConfig) (start : TimeNs) =
            // Make sure [start] is rounded to the nearest microsecond.  Otherwise, if you
            // feed [Clock.now ()] to a time function, it can be rounded down to a time in
            // the past, causing errors.
            let start = failwith ""
            // Time_ns.of_time_float_round_nearest_microsecond
            //   (Time_ns.to_time_float_round_nearest_microsecond start)
            State.createClock state config start

        { new IClock with
            member this.DefaultTimingWheelConfig = defaultConfig
            member this.Create' config start = create config start
            member this.Create start = create defaultConfig start
            member this.At clock time = State.at clock time
            member this.AdvanceClock clock time = State.advanceClock clock time
        }

    interface Incremental with
        member this.Return a = State.konst state a
        member this.Pack a = NodeCrate.make a
        member this.Map f a = State.map f a
        member this.Bind f n = State.bind n f
        member this.Stabilize () = State.stabilize state
        member this.Var = var
        member this.Clock = clock
        member this.Observe n = State.createObserver None n |> Observer
        member this.State = state

[<RequireQualifiedAccess>]
module Incremental =

    let make () : Incremental =
        let state = State.create 128
        IncrementalImpl state :> _
