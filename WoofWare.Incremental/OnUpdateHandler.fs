namespace WoofWare.PlayFetch

type PreviousUpdateKind =
    | NeverBeenUpdated
    | Necessary
    | Changed
    | Invalidated
    | Unnecessary

type NodeUpdate<'a> =
    | Necessary of 'a
    | Changed of 'a * 'a
    | Invalidated
    | Unnecessary

type OnUpdateHandler<'a> =
    {
        F : 'a NodeUpdate -> unit
        mutable PreviousUpdateKind : PreviousUpdateKind
        CreatedAt : StabilizationNum
    }

[<RequireQualifiedAccess>]
module OnUpdateHandler =
    let create f at =
        {
            F = f
            PreviousUpdateKind = PreviousUpdateKind.NeverBeenUpdated
            CreatedAt = at
        }

    let reallyRun (t : OnUpdateHandler<'a>) (nodeUpdate : 'a NodeUpdate) =
        t.PreviousUpdateKind <-
            match nodeUpdate with
            | Necessary _ -> PreviousUpdateKind.Necessary
            | Changed _ -> PreviousUpdateKind.Changed
            | Invalidated -> PreviousUpdateKind.Invalidated
            | Unnecessary -> PreviousUpdateKind.Unnecessary

        t.F nodeUpdate

    let run (t : OnUpdateHandler<'a>) (nodeUpdate : 'a NodeUpdate) now =
        (* We only run the handler if was created in an earlier stabilization cycle.  If the
         handler was created by another on-update handler during the running of on-update
         handlers in the current stabilization, we treat the added handler as if it were added
         after this stabilization finished.  We will run it at the next stabilization, because
         the node with the handler was pushed on [state.handle_after_stabilization]. *)
        if t.CreatedAt < now then
            match t.PreviousUpdateKind, nodeUpdate with
            (* Once a node is invalidated, there will never be further information to provide,
           since incremental does not allow an invalid node to become valid. *)
            | PreviousUpdateKind.Invalidated, _ -> ()
            (* These cases can happen if a node is handled after stabilization due to another
           handler.  But for the current handler, there is nothing to do because there is no
           new information to provide. *)
            | PreviousUpdateKind.Changed, NodeUpdate.Necessary _
            | PreviousUpdateKind.Necessary, NodeUpdate.Necessary _
            | PreviousUpdateKind.Unnecessary, NodeUpdate.Unnecessary -> ()
            (* If this handler hasn't seen a node that is changing, we treat the update as an
           initialization. *)
            | (PreviousUpdateKind.NeverBeenUpdated | PreviousUpdateKind.Unnecessary), NodeUpdate.Changed (_, a) ->
                reallyRun t (Necessary a)
            (* All other updates are run as is. *)
            | PreviousUpdateKind.NeverBeenUpdated,
              (NodeUpdate.Necessary _ | NodeUpdate.Unnecessary | NodeUpdate.Invalidated)
            | PreviousUpdateKind.Unnecessary, (Necessary _ | NodeUpdate.Invalidated)
            | PreviousUpdateKind.Necessary, (NodeUpdate.Changed _ | NodeUpdate.Unnecessary | NodeUpdate.Invalidated)
            | PreviousUpdateKind.Changed, (NodeUpdate.Changed _ | NodeUpdate.Unnecessary | NodeUpdate.Invalidated) ->
                reallyRun t nodeUpdate
