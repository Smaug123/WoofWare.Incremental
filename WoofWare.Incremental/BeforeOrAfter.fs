namespace WoofWare.Incremental

/// Indicates which side of a time threshold a node is firing at.
[<Struct>]
type BeforeOrAfter =
    /// The current time of the clock is strictly before the time this node has a step-change at.
    | Before
    /// The current time of the clock is at or after the time this node has a step-change at.
    | After
