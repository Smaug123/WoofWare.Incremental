namespace WoofWare.Incremental

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
type Update<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

type Observer<'a> =
    | Observer of Observer'<'a>

    override this.ToString () =
        match this with
        | Observer this ->
            match this.Value.State with
            | InternalObserverState.Created -> "<unstabilized>"
            | InternalObserverState.Disallowed
            | InternalObserverState.Unlinked -> "<disallowed>"
            | InternalObserverState.InUse ->
                match this.Value.Observing.ValueOpt with
                | ValueNone -> "<invalid>"
                | ValueSome v -> v.ToString ()

[<RequireQualifiedAccess>]
module Observer =
    type Update<'a> =
        | Initialized of 'a
        | Changed of 'a * 'a
        | Invalidated

    let toString (a : Observer<'a>) = a.ToString ()

    let disallowFutureUse<'a> (Observer o : 'a Observer) : unit = State.disallowFutureUse o.Value
    let useIsAllowed<'a> (Observer o : 'a Observer) : bool = Observer'.useIsAllowed o

    let observing<'a> (Observer o : 'a Observer) = InternalObserver.observing o.Value

    let value (Observer o) = State.observerValue o

    let valueThrowing (Observer o) = State.observerValueThrowing o

    let onUpdateThrowing (Observer a) (f : Update<'a> -> unit) =
        fun x ->
            match x with
            | NodeUpdate.Changed (a, b) -> f (Update.Changed (a, b))
            | NodeUpdate.Necessary m -> f (Update.Initialized m)
            | NodeUpdate.Invalidated -> f Update.Invalidated
            | NodeUpdate.Unnecessary ->
                failwith
                    "Logic error in WoofWare.Incremental: Observer.onUpdateThrowing got unexpected Unnecessary update"
        |> State.observerOnUpdateThrowing a

type IExpertIncremental =
    abstract DoOneStepOfStabilize : unit -> StepResult

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

type Incremental =
    abstract Return<'a> : 'a -> Node<'a>
    abstract Const<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    abstract Map2<'a, 'b, 'c> : ('a -> 'b -> 'c) -> Node<'a> -> Node<'b> -> Node<'c>
    abstract Join<'a> : 'a Node Node -> 'a Node
    abstract Both<'a, 'b> : 'a Node -> 'b Node -> ('a * 'b) Node
    abstract Var : IVar
    abstract Clock : IClock
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract Observe'<'a> : shouldFinalize : bool -> Node<'a> -> Observer<'a>
    abstract State : State
    abstract SaveDot' : stableNodeIds : bool -> renderBindEdges : bool -> writeChunk : (string -> unit) -> unit
    abstract SaveDot : writeChunk : (string -> unit) -> unit
    abstract CurrentScope : Scope
    abstract Expert : IExpertIncremental
    abstract WithinScope : Scope -> (unit -> 'a) -> 'a
    abstract OnUpdate<'a> : 'a Node -> (NodeUpdate<'a> -> unit) -> unit
    abstract SetCutoff<'a> : 'a Node -> 'a Cutoff -> unit
    abstract AmStabilizing : bool

type IncrementalImpl (state : State) =
    let var =
        { new IVar with
            member this.Create x = State.createVar state None x

            member this.Create' useCurrentScope x =
                State.createVar state (Some useCurrentScope) x

            member this.Watch v = v.Watch
            member this.Set var a = State.setVar var a

            member this.Replace var f =
                State.setVar var (f (Var.latestValue var))

            member this.Value var = var.Value
            member this.LatestValue var = Var.latestValue var
        }

    let expert =
        { new IExpertIncremental with
            member _.DoOneStepOfStabilize () = State.doOneStepOfStabilize state
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
            let divided = float<int64<timeNs>> start / 1000.0
            let start = int64<float> (System.Math.Round divided) * 1000L<timeNs>
            State.createClock state config start

        { new IClock with
            member this.DefaultTimingWheelConfig = defaultConfig
            member this.Create' config start = create config start
            member this.Create start = create defaultConfig start
            member this.At clock time = State.at clock time
            member this.AtIntervals clock time = State.atIntervals clock time
            member this.AdvanceClock clock time = State.advanceClock clock time

            member this.AdvanceClockBy clock span =
                State.advanceClock clock (TimeNs.add (Clock.now clock) span)

            member this.Snapshot clock v at before = State.snapshot clock v at before
        }

    interface Incremental with
        member this.Return a = State.konst state a
        member this.Const a = State.konst state a
        member this.Pack a = NodeCrate.make a
        member this.Map f a = State.map f a
        member this.Map2 f a b = State.map2 a b f
        member this.Both a b = State.both a b
        member this.Join n = State.join n
        member this.Bind f n = State.bind n f
        member this.Stabilize () = State.stabilize state
        member this.Var = var
        member this.Clock = clock
        member this.Observe n = State.createObserver None n |> Observer

        member this.Observe' (shouldFinalize : bool) (n : Node<'a>) =
            State.createObserver (Some shouldFinalize) n |> Observer

        member this.State = state
        member this.CurrentScope = state.CurrentScope
        member this.WithinScope scope f = State.withinScope state scope f
        member this.Expert = expert
        member this.SetCutoff node cutoff = Node.setCutoff node cutoff

        member this.SaveDot' stableNodeIds renderBindEdges writeChunk =
            NodeToDot.renderDot stableNodeIds renderBindEdges writeChunk (State.directlyObserved state)

        member this.SaveDot writeChunk =
            (this :> Incremental).SaveDot' false true writeChunk

        member this.OnUpdate n f = State.nodeOnUpdate n f

        member this.AmStabilizing = State.amStabilizing state


[<RequireQualifiedAccess>]
module Incremental =

    let make () : Incremental =
        let state = State.create 128
        IncrementalImpl state :> _
