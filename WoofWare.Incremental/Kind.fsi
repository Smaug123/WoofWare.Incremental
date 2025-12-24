namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal Kind =

    val name<'a> : Kind<'a> -> string
    val initialNumChildren<'a> : Kind<'a> -> int

    /// <exception cref="IndexOutOfRangeException">
    /// <c>index</c> was negative, or was greater-or-equal <c>maxNumChildren t</c>,
    /// or if the <c>index</c>'th child is currently undefined.
    /// </exception>
    val slowGetChild<'a> : Kind<'a> -> index : int -> NodeCrate

    val bindRhsChildIndex : int
    val freezeChildIndex : int
    val ifBranchChildIndex : int
    val joinRhsChildIndex : int
    val iteriChildren<'a> : Kind<'a> -> f : (int -> NodeCrate -> unit) -> unit

    /// Allocation-free version of iteriChildren using a struct visitor.
    val inline iteriChildrenWithVisitor<'a, 'TVisitor, 'TState
        when 'TVisitor : struct and 'TVisitor :> IChildVisitor<'TState>> :
        Kind<'a> -> visitor : 'TVisitor -> state : 'TState -> unit

    val invariant<'a> : ('a -> unit) -> Kind<'a> -> unit
