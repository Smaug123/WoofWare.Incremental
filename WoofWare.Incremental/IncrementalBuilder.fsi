namespace WoofWare.Incremental

/// A computation expression builder for constructing Incremental nodes.
/// Use `let!` for monadic bind (creates a Bind node that can restructure the graph).
/// Use `and!` for applicative combination (uses Map2, which is more efficient than Bind when you don't need graph restructuring).
/// Use `return` to lift a value into a constant node.
/// Use `return!` to return an existing node unchanged.
[<Sealed>]
type IncrementalBuilder =
    new : Incremental -> IncrementalBuilder
    member Return<'a> : 'a -> Node<'a>
    member ReturnFrom<'a> : Node<'a> -> Node<'a>
    member Bind<'a, 'b> : Node<'a> * ('a -> Node<'b>) -> Node<'b>
    member MergeSources<'a, 'b> : Node<'a> * Node<'b> -> Node<'a * 'b>
    member BindReturn<'a, 'b> : Node<'a> * ('a -> 'b) -> Node<'b>

/// Module containing helper functions for creating IncrementalBuilder instances.
[<RequireQualifiedAccess>]
module IncrementalBuilder =
    /// Create a computation expression builder for the given Incremental instance.
    val create : Incremental -> IncrementalBuilder
