namespace WoofWare.Incremental

(** A module internal to Incremental. Users should see {!Incremental_intf}.

    An [('a, 'b) Bind.t] is stored inside a bind node where the left-hand side is of type
    ['a], and the right-hand side is of type ['b Node.t].

    Each bind [t] has [t.lhs_change : unit Node.t] that is used to detect when [t.lhs]
    changes. Computing [t.lhs_change] restructures the graph by calling [t.f] and
    replacing [t.rhs] with the result.

    Each bind tracks the set of nodes created on its right-hand side, as a singly-linked
    list [t.all_nodes_created_on_rhs]. This is used to invalidate all those nodes when the
    [t.lhs] changes.

    The key invariant of a bind node [t] is:

    [t.lhs_change.height < node.height] for all necessary [node]s in
    [t.all_nodes_created_on_rhs].

    This ensures that a node created on the right-hand side is not computed unless the
    left-hand side is stable.

    The graph manipulation done when [t.lhs_change] fires can't be done when [t.lhs]
    fires, because [t.main] could be itself created inside a bind, and this bind's lhs is
    not guaranteed to be stable when [t.lhs] fires (but it is guaranteed to be stable when
    [t.lhs_change] fires). *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Bind =
    /// [isValid t] iff the scope in which [t] was created is valid.
    val isValid : Bind<'a, 'b> -> bool

    val iterNodesCreatedOnRhs : Bind<'a, 'b> -> f : (NodeCrate -> unit) -> unit

    val internal invariant<'a, 'b> : ('a -> unit) -> ('b -> unit) -> Bind<'a, 'b> -> unit

    val internal physSame<'a, 'b, 'c, 'd> : Bind<'a, 'b> -> Bind<'c, 'd> -> bool
