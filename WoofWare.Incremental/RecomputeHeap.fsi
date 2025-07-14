namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal RecomputeHeap =
    val create : max_height_allowed:int -> RecomputeHeap
    val length : RecomputeHeap -> int

    /// <summary>The maximum <c>Node.Height</c> allowed for <c>node</c> in this heap.</summary>
    val maxHeightAllowed : RecomputeHeap -> int

    /// <remarks>
    /// It is an error to call <c>setMaxHeightAllowed</c> if there is a <c>Node</c> in this heap with
    /// <c>Height &gt; m</c>.
    /// </remarks>
    val setMaxHeightAllowed : RecomputeHeap -> int -> unit

    (** [min_height t] returns the smallest height of any element in [t], or
        [max_height_allowed + 1] if [length t = 0]. *)
    val minHeight : RecomputeHeap -> int

    (** [add t node] should only be called iff:

        {[
          (not (Node.is_in_recompute_heap node))
          && Node.needs_to_be_computed node
          && node.height <= max_height_allowed t
        ]} *)
    val add : RecomputeHeap -> 'a Node -> unit

    (** [remove t node] should only be called iff:

        {[
          Node.is_in_recompute_heap node && not (Node.needs_to_be_computed node)
        ]} *)
    val remove : RecomputeHeap -> 'a Node  -> unit

    (** [remove_min t] removes and returns a node in [t] with minimum height. [remove_min]
        should only be called if [length t > 0]. *)
    val removeMin : RecomputeHeap -> NodeCrate

    (** [increase_height t node] should only be called when:

        - [node.height > node.height_in_recompute_heap]
        - [node.height <= max_height_allowed t]
        - [Node.is_in_recompute_heap node]

        It changes [node.height_in_recompute_heap] to equal [node.height] and adjusts [node]'s
        position in [t]. *)
    val increaseHeight : RecomputeHeap -> 'a Node -> unit
