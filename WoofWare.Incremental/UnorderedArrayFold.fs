namespace WoofWare.PlayFetch

open System
open TypeEquality

type internal Update<'a, 'b> =
    | FInverse of ('b -> 'a -> 'b)
    | Update of ('b -> 'a -> 'a -> 'b)


[<RequireQualifiedAccess>]
module Update =
    let update t f =
        match t with
        | Update.Update update -> update
        | Update.FInverse fInverse -> fun fold_value old newValue -> f (fInverse fold_value old) newValue

[<RequireQualifiedAccess>]
module internal UnorderedArrayFold =
    let same (t1 : UnorderedArrayFold<'a, 'b>) (t2 : UnorderedArrayFold<'c, 'd>) = Object.ReferenceEquals (t1, t2)

    let create init f update fullComputeEveryNChanges children main =
        {
            Init = init
            F = f
            Update = Update.update update f
            FullComputeEveryNChanges = fullComputeEveryNChanges
            Children = children
            Main = main
            FoldValue = ValueNone
            (* We make [num_changes_since_last_full_compute = full_compute_every_n_changes]
     so that there will be a full computation the next time the node is computed. *)
            NumChangesSinceLastFullCompute = fullComputeEveryNChanges
        }

    let fullCompute (f : UnorderedArrayFold<'a, 'acc>) : 'acc =
        let mutable result = f.Init

        for i = 0 to Array.length f.Children - 1 do
            let child = f.Children.[i]
            result <- f.F result child.ValueOpt.Value

        result

    let compute t =
        if t.NumChangesSinceLastFullCompute = t.FullComputeEveryNChanges then
            t.NumChangesSinceLastFullCompute <- 0
            t.FoldValue <- ValueSome (fullCompute t)

        t.FoldValue.Value

    let forceFullCompute t =
        t.FoldValue <- ValueNone
        t.NumChangesSinceLastFullCompute <- t.FullComputeEveryNChanges

    let childChanged<'a, 'b, 'acc>
        (t : UnorderedArrayFold<'a, 'acc>)
        (child : 'b Node)
        (childIndex : int)
        (oldValueOpt : 'b voption)
        (newValue : 'b)
        : unit
        =
        let childAtIndex = t.Children.[childIndex]

        match Node.typeEqualIfPhysSame child childAtIndex with
        | None -> failwith "UnorderedArrayFold.childChanged mismatch"
        | Some teq ->
            if t.NumChangesSinceLastFullCompute < t.FullComputeEveryNChanges - 1 then
                t.NumChangesSinceLastFullCompute <- t.NumChangesSinceLastFullCompute + 1
                (* We only reach this case if we have already done a full compute, in which case
           [t.fold_value.IsSome] and [old_value_opt.IsSome]. *)
                t.FoldValue <-
                    t.Update t.FoldValue.Value (oldValueOpt.Value |> Teq.cast teq) (newValue |> Teq.cast teq)
                    |> ValueSome
            elif t.NumChangesSinceLastFullCompute < t.FullComputeEveryNChanges then
                forceFullCompute t
