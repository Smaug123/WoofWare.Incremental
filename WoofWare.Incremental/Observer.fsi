namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal Observer =
    val observing<'a> : 'a Observer -> 'a Node
    val useIsAllowed<'a> : 'a Observer -> bool
    val valueThrowing<'a> : 'a Observer -> 'a
    val onUpdateThrowing<'a> : 'a Observer -> 'a OnUpdateHandler -> unit
    val incrState<'a> : 'a Observer -> State
