namespace WoofWare.Incremental

/// Unit of measure used to distinguish the StabilizationNum type.
[<Measure>]
type stab

/// A (zero-indexed) stabilization wave identifier.
type StabilizationNum = int<stab>

/// A (zero-indexed) stabilization wave identifier.
[<RequireQualifiedAccess>]
module StabilizationNum =
    /// No stabilizations have ever been performed on the graph of which this node is a part:
    /// we're still in initialization.
    val none : StabilizationNum
    /// The first stabilization. An Incremental graph, before Stabilize has ever been called,
    /// is on this wave.
    val zero : StabilizationNum
    /// "Are we in the initialization phase?"
    val isNone : StabilizationNum -> bool
    /// "Are we strictly past the initialization phase?"
    val isSome : StabilizationNum -> bool
    /// Increment
    val add1 : StabilizationNum -> StabilizationNum
    /// Get an integer, for display purposes
    val toInt : StabilizationNum -> int

    /// Check that consistency invariants hold.
    val invariant : StabilizationNum -> unit
