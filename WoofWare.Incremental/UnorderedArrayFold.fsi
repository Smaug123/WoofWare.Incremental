namespace WoofWare.PlayFetch

// An UnorderedArrayFold<'a, 'acc> is a kind of DAG node, where 'a is the type of value being folded
// and 'acc is the type of the accumulator.

type internal Update<'a, 'b> =
    | FInverse of ('b -> 'a -> 'b)
    // 'a params are old, then new
    | Update of ('b -> 'a -> 'a -> 'b)

[<RequireQualifiedAccess>]
module internal UnorderedArrayFold =
    val create :
        init : 'acc ->
        f : ('acc -> 'a -> 'acc) ->
        update : Update<'a, 'acc> ->
        fullComputeEveryNChanges : int ->
        children : 'a Node[] ->
        main : 'acc Node ->
            UnorderedArrayFold<'a, 'acc>

    val compute : UnorderedArrayFold<'a, 'acc> -> 'acc

    val childChanged<'a, 'b, 'acc> :
        t : UnorderedArrayFold<'a, 'acc> ->
        child : 'b Node ->
        childIndex : int ->
        oldValueOpt : 'b voption ->
        newValue : 'b ->
            unit

    val forceFullCompute : UnorderedArrayFold<'a, 'b> -> unit
