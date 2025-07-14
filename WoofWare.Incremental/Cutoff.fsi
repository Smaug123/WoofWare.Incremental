namespace WoofWare.PlayFetch

// A Cutoff<'a> is a function that returns [true] if propagation of changes should be
// cut off at a node based on the old value and the (possible) new value of the node.

[<NoEquality ; NoComparison>]
type internal Cutoff<'a>

[<RequireQualifiedAccess>]
module internal Cutoff =
    /// argument is old -> new -> bool
    val create<'a> : ('a -> 'a -> bool) -> 'a Cutoff

    val ofCompare<'a> : ('a -> 'a -> int) -> 'a Cutoff
    val ofEqual<'a> : ('a -> 'a -> bool) -> 'a Cutoff

    val always<'a> : 'a Cutoff
    val never<'a> : 'a Cutoff
    val physEqual<'a> : 'a Cutoff
    val polyEqual<'a when 'a : equality> : 'a Cutoff
    val equal<'a> : 'a Cutoff -> 'a Cutoff -> bool
    val shouldCutoff<'a> : 'a Cutoff -> old : 'a -> newValue : 'a -> bool
