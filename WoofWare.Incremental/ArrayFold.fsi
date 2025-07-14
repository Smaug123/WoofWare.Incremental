namespace WoofWare.PlayFetch

// An [Array_fold.t] is a kind of DAG node. It is an immutable value that holds the
// children of type ['a] and can [compute] the fold to produce a value of type ['b].

[<RequireQualifiedAccess>]
module internal ArrayFold =
    val compute : ArrayFold<'a, 'b> -> 'b
