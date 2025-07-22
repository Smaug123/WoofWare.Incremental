namespace WoofWare.Incremental

open System
open TypeEquality

type internal FoldUpdate<'a, 'b> =
    | FInverse of ('b -> 'a -> 'b)
    | Update of ('b -> 'a -> 'a -> 'b)


[<RequireQualifiedAccess>]
module FoldUpdate =
    let update t f =
        match t with
        | FoldUpdate.Update update -> update
        | FoldUpdate.FInverse fInverse -> fun fold_value old newValue -> f (fInverse fold_value old) newValue

[<RequireQualifiedAccess>]
module internal UnorderedArrayFold =
    let same (t1 : UnorderedArrayFold<'a, 'b>) (t2 : UnorderedArrayFold<'c, 'd>) = Object.ReferenceEquals (t1, t2)

    let create init f update fullComputeEveryNChanges children main =
        {
            Init = init
            F = f
            Update = FoldUpdate.update update f
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

        match NodeHelpers.typeEqualIfPhysSame child childAtIndex with
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

    let invariant<'a, 'b> (invA : 'a -> unit) (invB : 'b -> unit) (f : UnorderedArrayFold<'a, 'b>) : unit =
        match f.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.UnorderedArrayFold t' ->
            { new UnorderedArrayFoldEval<_, _> with
                member _.Eval t' =
                    if not (same f t') then
                        failwith "invariant failed"

                    FakeUnit.ofUnit ()
            }
            |> t'.Apply
            |> FakeUnit.toUnit
        | k -> failwith $"invariant failed: {k}"

        invB f.Init

        for child in f.Children do
            child.ValueOpt |> ValueOption.iter invA

            if f.NumChangesSinceLastFullCompute < f.FullComputeEveryNChanges then
                if child.ValueOpt.IsNone then
                    failwith "invariant failed"

        f.FoldValue |> ValueOption.iter invB

        if
            f.FoldValue.IsSome
            <> (f.NumChangesSinceLastFullCompute < f.FullComputeEveryNChanges)
        then
            failwith "invariant failed"

        do
            if f.NumChangesSinceLastFullCompute < 0 then
                failwith "invariant failed"

            if f.NumChangesSinceLastFullCompute > f.FullComputeEveryNChanges then
                failwith "invariant failed"

        if f.FullComputeEveryNChanges <= 0 then
            failwith "invariant failed"
