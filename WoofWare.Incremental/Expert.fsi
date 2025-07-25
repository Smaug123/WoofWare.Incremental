namespace WoofWare.Incremental

(** A module internal to Incremental. Users should see {!Incremental_intf}.

    An [Expert.t] is the only kind of node that can update its value and set of children
    incrementally. The operations to change the set of children and to react to various
    events (new value in a child etc) are exposed to the user. *)

[<RequireQualifiedAccess>]
type internal StaleResult =
    | AlreadyStale
    | Ok

[<RequireQualifiedAccess>]
type internal BeforeMainComputationResult =
    | Invalid
    | Ok

[<RequireQualifiedAccess>]
module internal Expert =
    val invariantAboutNumInvalidChildren : 'a Expert -> isNecessary : bool -> unit

    /// onObservabilityChange : isNowAvailable -> unit
    val create : f : (unit -> 'a) -> onObservabilityChange : (bool -> unit) -> 'a Expert

    val makeStale : _ Expert -> StaleResult
    val incrInvalidChildren : _ Expert -> unit
    val decrInvalidChildren : _ Expert -> unit

    /// Returns the index of this new edge.
    val addChildEdge : _ Expert -> ExpertEdgeCrate -> int

    val swapChildren : _ Expert -> childIndex1 : int -> childIndex2 : int -> unit
    val lastChildEdgeThrowing : _ Expert -> ExpertEdgeCrate
    val removeLastChildEdgeThrowing : _ Expert -> unit
    val beforeMainComputation : _ Expert -> BeforeMainComputationResult
    val observabilityChange : _ Expert -> isNowObservable : bool -> unit
    val runEdgeCallback : _ Expert -> childIndex : int -> unit

    val invariant<'a> : ('a -> unit) -> Expert<'a> -> unit
