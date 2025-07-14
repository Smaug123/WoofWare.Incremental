namespace WoofWare.PlayFetch

module internal Observer =
    val observing : 'a Observer -> 'a Node
    val useIsAllowed : _ Observer -> bool
    val valueThrowing : 'a Observer -> 'a
    val onUpdateThrowing : 'a Observer -> 'a OnUpdateHandler -> unit
    val incrState : _ Observer -> State
