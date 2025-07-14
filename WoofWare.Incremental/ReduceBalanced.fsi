namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal ReduceBalanced =
    val create :  State -> 'a Node array -> f:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b Node option

