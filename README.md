# WoofWare.Incremental

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.Incremental.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.Incremental)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.Incremental/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.Incremental/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.Incremental)](./LICENCE.md)

This is [`incremental`](https://github.com/janestreet/incremental), but in F#.

# Status

The library is ported, and most of the tests are ported and running.
There are known bugs and infelicities; this library is not yet usable unless you are willing to get hurt and report the bugs.

# Licence

This is a derivative work of [incremental](https://github.com/janestreet/incremental/tree/4c3946aafe786e4846f8ec3f4825e7bc689a70fa), used under the MIT licence.
A copy of that licence is at [LICENCE_janestreet.md](LICENCE_janestreet.md).
All glory to Jane Street.

WoofWare.Incremental is licenced to you under the MIT licence.
A copy of that licence is at [LICENCE.md](LICENCE.md).

# Purpose

*This section is entirely quoted from the original OCaml documentation, with minor adjustments to accommodate the port.*

Incremental is a library that gives you a way of building complex computations that can update efficiently in response to their inputs changing, inspired by the work of [[http://www.umut-acar.org/self-adjusting-computation][Umut Acar et. al.]] on self-adjusting computations.
Incremental can be useful in a number of applications, including:

- Building large calculations (of the kind you might build into a spreadsheet) that can react efficiently to changing data.
- Constructing views in GUI applications that can incorporate new data efficiently.
- Computing derived data while guaranteeing that the derived data stays in sync with the source data, for instance filtering or inversing a mapping.

You can find an informal introduction to the library in this [[https://blog.janestreet.com/introducing-incremental][blog post]] and [this video](https://www.youtube.com/watch?v=G6a5G5i4gQU).

## Incremental in a nutshell

Incremental is used to define a collection of interdependent values, some of which are
"variables" set by user code and others that are defined via functions (in the
mathematical and programming senses) of other incremental values. Incremental
automatically tracks all the dependencies between incremental values and can, on
demand, propagate changed variables and recompute the incremental values that depend
on them.

To use `incremental`, one first creates a new instance via:

```fsharp
  let I : Incremental = Incremental.make ()
```

This creates data structures that will be shared throughout the lifetime of all incremental values used with this instance.

As an example of a simple computation, suppose we have integer variables `x` and `y`
and want to keep an incremental value `z` defined by `z = x + y`. We could do this
with:

```fsharp
let x = I.Var.Create 13
let y = I.Var.Create 17
let z = I.Map2 (fun x y -> x + y) (I.Var.watch x) (I.Var.watch y)
```

With this, as `x` and `y` change, incremental can recompute `z = x + y` on demand.
Incremental only recomputes values that are being "observed", which one indicates by
calling the `Observe` function to get an "observer", e.g.:

```fsharp
let obsZ = I.Observe z
```

Incremental doesn't compute `z` every time `x` and `y` change. Rather, one must
explicitly tell incremental when one wants `z` (and all other observed values) to be
brought up to date, by calling `Stabilize`:

```fsharp
  I.Stabilize ()
```

At this point, the value of `z` is `30`, which we can verify by:

```fsharp
assert (Observer.valueThrowing obsZ = 30)
```

If we change the value of `x` and then tell incremental to recompute observed values,
then the value of `z` will change appropriately:

```fsharp
I.Var.Set x 19;
I.Stabilize ();
assert (Observer.valueThrowing obsZ = 36)
```

Another way to observe values is to use `Observer.onUpdateThrowing`, which attaches an
"on-update handler" to an observer -- the handler will be run after each stabilization
in which the observer's value changed (or was initialized) during stabilization.

User functions given to incremental should never raise any exceptions: doing so will
cause all future calls to `stabilize` to raise.

## The incremental DAG

One can think of incrementals as forming a directed acyclic graph (DAG), where nodes
correspond to incremental values and there is an edge from node `n1` to node `n2` if
the value of `n2` depends on the value of `n1`. For example, the DAG for the above
example has an edge from `x` to `z` and from `y` to `z`. The graph must be acyclic in
order for the computation to be well defined. The graph is a DAG rather than a tree
because incremental values can be shared. Extending the above example, we might have:

```fsharp
let w = I.Map2 (fun y z -> y - z) (I.Var.Watch y) z
```

Both the node for `y` and the node for `z` are shared.

We will use "node" to mean "incremental value" when we want to emphasize some aspect
of the DAG.

Say that a node is "observed" if there is an observer for it (created via `Observe`).
Say that a node is "necessary" if there is a path from that node to an observed node.
`Stabilize` ensures that all necessary nodes have correct values; it will not compute
unnecessary nodes. An unobserved node becomes necessary by a call to `Observe` or by
being used to compute an observed node; this will cause the appropriate DAG edges to
be added. A necessary node will become unnecessary if its observer (if any) becomes
unused and if the node is no longer used to compute any observed nodes. This will
cause the appropriate DAG edges to be removed.

Incremental does not know whether user-supplied functions (e.g. functions supplied to
`Bind` or `Map`) are side effecting, and will not evaluate them for side effect. If
the resulting incrementals are not necessary then the function will not be called.

## Stabilization

`Stabilize` traverses the DAG in topological order starting at variables that changed
since the last stabilization and recomputing their dependents. This is done by using a
"recompute heap" to visit the nodes in non-decreasing order of "height", which is a
over-approximation of the longest path from a variable to that node. To ensure that
each node is computed at most once and that its children are stabilized before it is
computed, nodes satisfy the property that if there is an edge from n1 to n2, then the
height of n1 is less than the height of n2.

`Stabilize` repeats the following steps until the heap becomes empty:

1. remove from the recompute heap a node with the smallest height
2. recompute that node
3. if the node's value changes, then add its parents to the heap.

The definition of "changes" in step (3) is configurable by user code. By default, a
node is considered to change if its new value is not `Object.ReferenceEquals`
(for reference types) or `EqualityComparer.Default.Equals` (for value types) to the previous
value. One can use `SetCutoff` on a node to change its cutoff function, e.g. for
`float`s one could cutoff propagation if the old value and new value are closer than
some threshold.

If `Stabilize` ever raises due to an error, then the incremental system becomes
unusable, and all future calls to `Stabilize` will immediately raise.

Stabilization uses a heap implemented with an array whose length is the max height, so
for good performance, the height of nodes must be small. There is an upper bound on
the height of nodes, `I.MaxHeightAllowed`, which defaults to 128. An attempt to create
a node with larger height will raise. One can dynamically increase
`MaxHeightAllowed`; however, one should be wary of doing so, for performance
reasons.

## Bind

Much of the power of incremental comes from `Bind`. As a reminder, `Bind` has this type:

```fsharp
val Bind<'a, 'b> : f:('a -> 'b Node) -> 'a Node -> 'b Node
```

`Bind f ta` returns an incremental `tb` that behaves like `f a`, where `a` is the
most recent value of `ta`. The implementation only calls `f` when the value of `ta`
changes. Thinking in terms of the DAG, `Bind f ta` returns a node `tb` such that
whenever the value of `ta` changes, the implementation calls `f` to obtain a node
(possibly with an arbitrary DAG below it) that defines the value of `tb`.

`Bind` can be used to transition existing parts of the graph between necessary and
unnecessary. E.g.:

```fsharp
val if_ : bool Node -> a Node -> a Node -> a Node

let if_ a b c = a |> I.Bind (fun a -> if a then b else c)
```

With `let t = if_ a b c`, when `a` is `true`, if `t` is necessary, then `b` will be
necessary, but `c` will not. And vice-versa when `a` is `false`.

Even more, `Bind` allows one to dynamically create an arbitrary graph based on the
value of some other incremental, and to "hide" that dynamism behind an ordinary
incremental value. One common way to use this is for dynamic reconfiguration, e.g.:

```fsharp
let configVar = I.Var.Create config
I.Var.Watch configVar |> I.Bind (fun config -> ...)
```

Then, whenever one wants to reconfigure the system, one does `Var.Set configVar` and
then `Stabilize`, which will construct a new DAG according to the new config.

Bind nodes introduce special height constraints, so that stabilization is guaranteed
to recompute the left-hand side of a bind before recomputing any node created by the
right-hand side `f`. This avoids recomputing nodes created on the right-hand side that
would then become unnecessary when the left-hand side changes. More precisely, in
`t |> I.Bind f`, any node created by `f` is made to have a height larger than `t`. This rule
applies also to bind nodes created by `f`, so that ultimately the height of every node
is greater than the height of all the left-hand sides of the binds that were involved
in its creation. The height requirement does not apply to nodes returned by `f` but
not created by `f` -- such nodes depend on the bind in effect when they were created,
but have no dependence on `t`.

When the left-hand side of a bind node changes, stabilization "invalidates" all the
nodes that depend on it (because they may use an old value of the left-hand side).

For example, consider:

```fsharp
let t1 = I.Map ...
t2
|> I.Bind (fun _ ->
    let t3 = I.Map ...
    (t1, t3)
    ||> I.Map2 (fun t1 t3 -> ...)
)
```

In this example, `t1` is created outside of `Bind t2`, whereas `t3` is created by the
right-hand side of `Bind t2`. So, `t3` depends on `t2` (and has a greater height),
whereas `t1` does not. And, in a stabilization in which `t2` changes, we are
guaranteed to not recompute the old `t3`, but we have no such guarantee about `t1`.
Furthermore, when `t2` changes, the old `t3` will be invalidated, whereas `t1` will
not.

Since `Bind` essentially allows one to add arbitrary edges to the DAG, one can use it
to construct a cycle. `Stabilize` will detect such cycles and raise.

## Garbage collection

Incremental maintains three kinds of pointers:

- from nodes to their children (nodes they depend on).
- from necessary nodes to their necessary parents (nodes that depend on them).
- from observers to the nodes they observe.

So, all necessary nodes are kept alive, from the perspective of the garbage collector.

If an observer has no on-update handlers and user code no longer holds on to it,
incremental (via a finalizer on the observer) detects this and disallows future use of
the observer, making the node it observed unnecessary if it is not necessary for
another reason. One can eagerly remove an observer by calling `Observer.disallowFutureUse`.
Because finalizers may be called much later than when an observer actually becomes
unreachable, it is preferable to disable observers using `Observer.disallowFutureUse` to
avoid useless propagation during stabilizations.

If an observer has on-update handlers, calling `Observer.disallowFutureUse` is the only way
to have it removed.

## The implementation

The key type in the implementation is `Node<'a>`, which represents one node in the
incremental DAG. A node is a record with many fields (> 20). In
particular a node holds:

- kind -- the kind of node it is (const, var, map, bind, snapshot, etc.).
- value -- the node's current value.
- recompute id -- the stabilization number at which it was last recomputed.
- change id -- the stabilization number at which its value last changed.
- height -- the height of the node in the DAG.
- parents -- the necessary nodes that depend on it.
- children -- the nodes that it depends on.
- created_in -- the scope in which it was created.

Say that a node is "stale" if it has never been computed or if its recompute id is
less than the change id of one of its children. A node should be recomputed if it is
both necessary and stale.

The `State` type holds all the mutable data used to implement stabilization. In
particular, the incremental state contains:

- the current stabilization number
- the set of observers
- a recompute heap -- nodes that need to be recomputed, ordered by height.
- an adjust-heights heap -- nodes whose height needs increasing, ordered by height.

The goals of stabilization are to:

- compute all necessary nodes that need to be recomputed.
- only compute necessary nodes.
- compute each node at most once.
- only compute a node after ensuring its children are up to date.

To do this, incremental maintains the following invariants:

- `p` is in `c`'s parents iff (`c` is in `p`'s children && `p` is necessary)
- `p` is in the recompute heap iff `p` is necessary and stale.
- if `p` is a parent of `c`, then `p`'s height is greater than `c`'s height.

The first invariant ensures that when a node's value changes, we can reach from it all
necessary nodes (and only the necessary nodes) that depend on it. The second invariant
ensures that that stabilization only computes necessary nodes. The third invariant,
combined with the fact that stabilization always recomputes a node from the
recompute-heap that has minimum height, ensures that we only compute a node after all
its children are stable, and that we compute each node at most once.

Finally, at the end of stabilization, the recompute heap is empty, so the invariant
implies that there are no necessary nodes that are stale, i.e. stabilization has
computed all necessary nodes that need to be recomputed.

### Maintaining the parent invariant

Maintaining the invariant that a node has edges only to necessary parents requires
traversing a node's descendants when it transitions between necessary and unnecessary,
in order to add or remove parents as appropriate. For example, when an observer is
first added to an unnecessary node, the implementation visits all its descendants to
add parents. This is essentially a form of ref-counting, in which the counter is the
number of parents that a node has. There is no problem with cycles because the DAG
requirement on the graph is enforced.

### Maintaining the height invariant and checking for cycles

Maintaining the invariant that a necessary node's height is larger than all of its
children requires adjusting heights when an edge is added to the DAG (e.g. when a bind
left-hand side changes). This is done using the "adjust-heights" heap. When an edge is
added, if the child's height is greater than or equal to the parent's height, then the
adjust-heights heap increases the height of the parent and all of the parent's
ancestors as necessary in order to restore the height invariant. This is done by
visiting ancestors in topological order, in increasing order of pre-adjusted height.
If during that traversal, the child of the original edge is visited, then there is a
cycle in the graph, and stabilization raises.

In pathological situations, the implementation will raise due to a cyclic graph even
though subsequent graph operations would eliminate the cycle. This is because the
cyclicity check happens after each edge is added, rather than waiting until a batch of
graph changes.

### Bind, scopes, and invalidation

Much of the complexity of the implementation comes from `bind`. In `t |> I.Bind f`, when `f`
is applied to the value of `t`, all of the nodes that are created depend on that
value. If the value of `t` changes, then those nodes no longer make sense because they
depend on a stale value. It would be both wasteful and wrong to recompute any of those
"invalid" nodes. So, the implementation maintains the invariant that the height of a
necessary node is greater than the height of the left-hand side of the nearest
enclosing bind. That guarantees that stabilization will stabilize the left-hand side
before recomputing any nodes created on the right-hand side. Furthermore, if the
left-hand side's value changes, stabilization marks all the nodes on the right-hand
side as invalid. Such invalid nodes will typically be unnecessary, but there are
pathological cases where they remain necessary.

The bind height invariant is accomplished using a special "bind-lhs-change" node,
which is a parent of the bind-lhs and a child of the bind result. The incremental
state maintains the "current scope", which is the bind whose right-hand side is
currently being evaluated, or a special "top" scope if there is no bind in effect.
Each node has a `CreatedIn` field set to the scope in effect when the node is
created. The implementation keeps for each scope, a singly-linked list of all nodes
created in that scope. Invalidation traverses this list, and recurs on bind nodes in
it to traverse their scopes as well.

`I.If` and `I.Join` are special cases of `Bind` that manipulate the graph; however they
do not create new scopes. They use a similar lhs-change node to detect changes and
perform graph manipulation.

## Debugging

For performance reasons, `Incremental` is built with debugging asserts disabled.
You can set `Debug.globalFlag <- true` to enable debugging asserts.
This mode is significantly slower than `Incremental`, but may detect a bug in the
library that would otherwise remain undetected.
