namespace WoofWare.Incremental

[<AutoOpen>]
module internal InternalObserverInvariant =
    let private sameAsCrate (a : InternalObserver<'a>) (b : InternalObserverCrate) : bool =
        { new InternalObserverEval<_> with
            member _.Eval b = InternalObserver.same a b
        }
        |> b.Apply

    type InternalObserver<'a> with

        static member invariant (invA : 'a -> unit) (t : 'a InternalObserver) =
            Node.invariant invA t.Observing

            do
                match t.State with
                | InternalObserverState.Unlinked ->
                    if not (List.isEmpty t.OnUpdateHandlers) then
                        failwith "invariant failure"
                | _ -> ()

            do
                match t.State with
                | InternalObserverState.Created
                | InternalObserverState.Unlinked ->
                    if t.PrevInAll.IsSome then
                        failwith "invariant failure"
                | _ -> ()

                match t.PrevInAll with
                | ValueSome prevInAll ->
                    if not (sameAsCrate t (InternalObserverCrate.nextInAll prevInAll).Value) then
                        failwith "invariant failed"
                | ValueNone -> ()

            do
                match t.State with
                | InternalObserverState.Created
                | InternalObserverState.Unlinked ->
                    if t.NextInAll.IsSome then
                        failwith "invariant failure"
                | _ -> ()

                match t.NextInAll with
                | ValueSome nextInAll ->
                    if not (sameAsCrate t (InternalObserverCrate.prevInAll nextInAll).Value) then
                        failwith "invariant failed"
                | ValueNone -> ()

            do
                match t.State with
                | InternalObserverState.Created
                | InternalObserverState.Unlinked ->
                    if t.PrevInObserving.IsSome then
                        failwith "invariant failed"
                | _ -> ()

                match t.PrevInObserving with
                | ValueSome prevInObserving ->
                    if not (Type.referenceEqual t prevInObserving.NextInObserving.Value) then
                        failwith "invariant failed"
                | ValueNone -> ()

            do
                match t.State with
                | InternalObserverState.Created
                | InternalObserverState.Unlinked ->
                    if t.PrevInObserving.IsSome then
                        failwith "invariant failed"
                | _ -> ()

                match t.NextInObserving with
                | ValueSome nextInObserving ->
                    if not (Type.referenceEqual t nextInObserving.PrevInObserving.Value) then
                        failwith "invariant failed"
                | ValueNone -> ()

    type InternalObserverCrate with
        static member invariant (i : InternalObserverCrate) : unit =
            { new InternalObserverEval<_> with
                member _.Eval i =
                    InternalObserver.invariant ignore i |> FakeUnit.ofUnit
            }
            |> i.Apply
            |> FakeUnit.toUnit
