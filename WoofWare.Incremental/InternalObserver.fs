namespace WoofWare.Incremental

open System

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserver =
    let same (a : InternalObserver<'a>) (b : InternalObserver<'b>) = Object.ReferenceEquals (a, b)
    let sameAsCrate (a : InternalObserver<'a>) (b : InternalObserverCrate) : bool =
        { new InternalObserverEval<_> with
            member _.Eval b =
                same a b
         }
        |> b.Apply

    let nextInAll' (i : InternalObserverCrate) =
        { new InternalObserverEval<_> with
            member _.Eval i = i.NextInAll
         }
        |> i.Apply

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

    let nextInAll' (i : InternalObserverCrate) =
        { new InternalObserverEval<_> with
            member _.Eval i = i.NextInAll
        }
        |> i.Apply

    let prevInAll' (i : InternalObserverCrate) =
        { new InternalObserverEval<_> with
            member _.Eval i = i.PrevInAll
        }
        |> i.Apply

    let invariant (invA : 'a -> unit) (t : 'a InternalObserver) =
        Node.invariant invA t.Observing
        do
            match t.State with
            | InternalObserverState.Unlinked ->
                if not (List.isEmpty t.OnUpdateHandlers) then
                    failwith "invariant failure"
            | _ -> ()

        do
            match t.State with
            | InternalObserverState.Created | InternalObserverState.Unlinked ->
                if t.PrevInAll.IsSome then
                    failwith "invariant failure"
            | _ -> ()

            match t.PrevInAll with
            | ValueSome prevInAll ->
                if not (sameAsCrate t (nextInAll' prevInAll).Value) then
                    failwith "invariant failed"
            | ValueNone -> ()

        do
            match t.State with
            | InternalObserverState.Created | InternalObserverState.Unlinked ->
                if t.NextInAll.IsSome then
                    failwith "invariant failure"
            | _ -> ()
            match t.NextInAll with
            | ValueSome nextInAll ->
                if not (sameAsCrate t (prevInAll' nextInAll).Value) then
                    failwith "invariant failed"
            | ValueNone -> ()

        do
            match t.State with
            | InternalObserverState.Created | InternalObserverState.Unlinked ->
                if t.PrevInObserving.IsSome then failwith "invariant failed"
            | _ -> ()
            match t.PrevInObserving with
            | ValueSome prevInObserving ->
                if not (Object.ReferenceEquals (t, prevInObserving.NextInObserving)) then
                    failwith "invariant failed"
            | ValueNone -> ()

        do
            match t.State with
            | InternalObserverState.Created | InternalObserverState.Unlinked ->
                if t.PrevInObserving.IsSome then failwith "invariant failed"
            | _ -> ()
            match t.NextInObserving with
            | ValueSome nextInObserving ->
                if not (Object.ReferenceEquals (t, nextInObserving.PrevInObserving)) then
                    failwith "invariant failed"
            | ValueNone -> ()

    let valueThrowing (t : 'a InternalObserver) : 'a =
        match t.State with
        | InternalObserverState.Created -> failwith "Observer.valueThrowing called without stabilizing"
        | InternalObserverState.Disallowed | InternalObserverState.Unlinked ->
            failwith "Observer.valueThrowing called after disallowFutureUse"
        | InternalObserverState.InUse ->
            match t.Observing.ValueOpt with
            | ValueNone ->  failwith "attempt to get value of an invalid node"
            | ValueSome v -> v

    let onUpdateThrowing (t : 'a InternalObserver) (onUpdateHandler : 'a OnUpdateHandler) : unit =
        match t.State with
        | InternalObserverState.Disallowed | InternalObserverState.Unlinked -> failwith "onUpdate disallowed"
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
        if Object.ReferenceEquals (t, observing.Observers.Value) then
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

    let incrState (t: InternalObserver<'a>) : State = t.Observing.State

    let useIsAllowed (t : InternalObserver<'a>) =
        match t.State with
        | InternalObserverState.Created | InternalObserverState.InUse -> true
        | _ -> false

    let observing (t: InternalObserver<'a>) : Node<'a> = t.Observing

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalObserverCrate =
    let invariant (i : InternalObserverCrate) =
        { new InternalObserverEval<_> with
            member _.Eval i = InternalObserver.invariant ignore i |> FakeUnit.ofUnit
        }
        |> i.Apply
        |> FakeUnit.toUnit

    let nextInAll = InternalObserver.nextInAll'
    let setPrevInAll = InternalObserver.setPrevInAll'

    let internalObserverStateEval =
        { new InternalObserverEval<_> with
            member _.Eval s = s.State
        }

    let state (c : InternalObserverCrate) = c.Apply internalObserverStateEval
