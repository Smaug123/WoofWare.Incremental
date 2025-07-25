namespace WoofWare.Incremental

type 'a Expert1Dependency

[<RequireQualifiedAccess>]
module Expert1Dependency =

    val create : onChange : ('a -> unit) option -> 'a Node -> 'a Expert1Dependency
    val value : 'a Expert1Dependency -> 'a

type Expert1Node<'a>

[<RequireQualifiedAccess>]
module Expert1Node =

    /// parameter to onObservabilityChange is "isNowObservable"
    val create : State -> onObservabilityChange : (bool -> unit) option -> (unit -> 'a) -> 'a Expert1Node

    val watch : 'a Expert1Node -> 'a Node
    val makeStale : _ Expert1Node -> unit
    val invalidate : _ Expert1Node -> unit
    val addDependency : _ Expert1Node -> _ Expert1Dependency -> unit
    val removeDependency : _ Expert1Node -> _ Expert1Dependency -> unit
