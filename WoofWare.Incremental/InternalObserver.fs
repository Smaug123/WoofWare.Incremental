namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserver =
    let same (a : InternalObserver<'a>) (b : InternalObserver<'b>) = Type.referenceEqual' a b

    let setPrevInAll' (i : InternalObserverCrate) (v : InternalObserverCrate voption) =
        { new InternalObserverEval<_> with
            member _.Eval i = (i.PrevInAll <- v) |> FakeUnit.ofUnit
        }
        |> i.Apply
        |> FakeUnit.toUnit

    let setNextInAll' (i : InternalObserverCrate) (v : InternalObserverCrate voption) =
        { new InternalObserverEval<_> with
            member _.Eval i = (i.NextInAll <- v) |> FakeUnit.ofUnit
        }
        |> i.Apply
        |> FakeUnit.toUnit

    let valueThrowing (t : 'a InternalObserver) : 'a =
        match t.State with
        | InternalObserverState.Created -> failwith "Observer.valueThrowing called without stabilizing"
        | InternalObserverState.Disallowed
        | InternalObserverState.Unlinked -> failwith "Observer.valueThrowing called after disallowFutureUse"
        | InternalObserverState.InUse ->
            match t.Observing.ValueOpt with
            | ValueNone -> failwith "attempt to get value of an invalid node"
            | ValueSome v -> v

    let onUpdateThrowing (t : 'a InternalObserver) (onUpdateHandler : 'a OnUpdateHandler) : unit =
        match t.State with
        | InternalObserverState.Disallowed
        | InternalObserverState.Unlinked -> failwith "onUpdate disallowed"
        | InternalObserverState.Created ->
            t.OnUpdateHandlers <- onUpdateHandler :: t.OnUpdateHandlers
            // We'll bump observing.NumOnUpdateHandlers when t is actually added to Observers at the start of
            // the next stabilization.
            ()
        | InternalObserverState.InUse ->
            t.OnUpdateHandlers <- onUpdateHandler :: t.OnUpdateHandlers
            t.Observing.NumOnUpdateHandlers <- t.Observing.NumOnUpdateHandlers + 1

    let unlinkFromObserving (t : 'a InternalObserver) : unit =
        let prev = t.PrevInObserving
        let next = t.NextInObserving
        t.PrevInObserving <- ValueNone
        t.NextInObserving <- ValueNone

        match next with
        | ValueNone -> ()
        | ValueSome next -> next.PrevInObserving <- prev

        match prev with
        | ValueNone -> ()
        | ValueSome prev -> prev.NextInObserving <- next

        let observing = t.Observing

        if Type.referenceEqual t observing.Observers.Value then
            observing.Observers <- next

        observing.NumOnUpdateHandlers <- observing.NumOnUpdateHandlers - List.length t.OnUpdateHandlers
        t.OnUpdateHandlers <- []

    let unlinkFromAll (t : 'a InternalObserver) : unit =
        let prev = t.PrevInAll
        let next = t.NextInAll
        t.PrevInAll <- ValueNone
        t.NextInAll <- ValueNone

        match next with
        | ValueNone -> ()
        | ValueSome next -> setPrevInAll' next prev

        match prev with
        | ValueNone -> ()
        | ValueSome prev -> setNextInAll' prev next

    let unlink t =
        unlinkFromObserving t
        unlinkFromAll t

    let incrState (t : InternalObserver<'a>) : State = t.Observing.State

    let useIsAllowed (t : InternalObserver<'a>) =
        match t.State with
        | InternalObserverState.Created
        | InternalObserverState.InUse -> true
        | _ -> false

    let observing (t : InternalObserver<'a>) : Node<'a> = t.Observing

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserverCrate =
    let nextInAllEval =
        { new InternalObserverEval<_> with
            member _.Eval i = i.NextInAll
        }

    let prevInAllEval =
        { new InternalObserverEval<_> with
            member _.Eval i = i.PrevInAll
        }

    let nextInAll (i : InternalObserverCrate) = i.Apply nextInAllEval
    let prevInAll (i : InternalObserverCrate) = i.Apply prevInAllEval
    let setPrevInAll i j = InternalObserver.setPrevInAll' i j

    let internalObserverStateEval =
        { new InternalObserverEval<_> with
            member _.Eval s = s.State
        }

    let state (c : InternalObserverCrate) = c.Apply internalObserverStateEval
