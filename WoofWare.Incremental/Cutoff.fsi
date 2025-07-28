namespace WoofWare.Incremental

/// A Cutoff<'a> is a function that returns [true] if propagation of changes should be
/// cut off at a node based on the old value and the (possible) new value of the node.
[<NoEquality ; NoComparison>]
type Cutoff<'a> =
    internal
    | Always
    | Never
    | PhysEqual
    | Compare of ('a -> 'a -> int)
    | Equal of ('a -> 'a -> bool)
    /// old -> new -> bool
    | F of ('a -> 'a -> bool)

/// A Cutoff<'a> is a function that returns [true] if propagation of changes should be
/// cut off at a node based on the old value and the (possible) new value of the node.
[<RequireQualifiedAccess>]
module Cutoff =
    /// argument is old -> new -> bool
    ///
    /// If the input function returns `true` when the input value changes from `old` to `new`,
    /// we don't propagate that change through the graph.
    val create<'a> : ('a -> 'a -> bool) -> 'a Cutoff

    /// Cut off changes for which the given comparison function (e.g. a `.Compare`) returns `0`.
    val ofCompare<'a> : ('a -> 'a -> int) -> 'a Cutoff

    /// A synonym of `create`. Cut off changes for which the input function returns `true` when
    /// presented with `old` and `new`.
    val ofEqual<'a> : ('a -> 'a -> bool) -> 'a Cutoff

    /// Always cut off changes: don't propagate any events through the graph.
    val always<'a> : 'a Cutoff
    /// Never cut off changes: all events should propagate through the graph, even if they didn't
    /// change any values.
    val never<'a> : 'a Cutoff
    /// Cut off changes which maintain reference equality of the inputs (or default value equality in
    /// the case of value types).
    val physEqual<'a> : 'a Cutoff
    /// Cut off changes which maintain `.Equals` equality of the inputs.
    val polyEqual<'a when 'a : equality> : 'a Cutoff

    val shouldCutoff<'a> : 'a Cutoff -> old : 'a -> newValue : 'a -> bool

    val internal invariant<'a> : ('a -> unit) -> 'a Cutoff -> unit
