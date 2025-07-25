namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserver =
    val same : 'a InternalObserver -> 'b InternalObserver -> bool
    val observing : 'a InternalObserver -> 'a Node
    val useIsAllowed : 'a InternalObserver -> bool
    val valueThrowing : 'a InternalObserver -> 'a
    val onUpdateThrowing : 'a InternalObserver -> 'a OnUpdateHandler -> unit
    val unlink : 'a InternalObserver -> unit
    val incrState : 'a InternalObserver -> State

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserverCrate =
    val nextInAll : InternalObserverCrate -> InternalObserverCrate voption
    val prevInAll : InternalObserverCrate -> InternalObserverCrate voption
    val setPrevInAll : InternalObserverCrate -> InternalObserverCrate voption -> unit
    val state : InternalObserverCrate -> InternalObserverState
