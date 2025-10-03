namespace WoofWare.Incremental

open System
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    let physSame (t1 : StepFunctionNode<'a>) (t2 : StepFunctionNode<'b>) = Type.referenceEqual' t1 t2

    let rec advanceInternal (t : StepFunctionNode<'a>) (to_ : TimeNs) (a1 : 'a) (steps : Sequence<TimeNs * 'a>) : unit =
        match Sequence.next steps with
        | Some ((stepAt, a2), steps2) when to_ >= stepAt -> advanceInternal t to_ a2 steps2
        | _ ->
            t.Value <- ValueSome a1
            t.UpcomingSteps <- steps

    let advance (t : StepFunctionNode<'a>) to_ =
        advanceInternal t to_ t.Value.Value t.UpcomingSteps

    let invariant<'a> (invA : 'a -> unit) (t : StepFunctionNode<'a>) : unit =
        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.Const _ ->
            // happens when upcomingSteps becomes empty
            ()
        | Kind.StepFunction t' ->
            if not (physSame t t') then
                failwith "invariant failed"
        | k -> failwith $"invariant failed: %O{k}"

        t.Value |> ValueOption.iter invA

        match t.AlarmValue.Action with
        | AlarmValueAction.StepFunction t2 ->
            { new StepFunctionNodeEval<_> with
                member _.Eval t2 =
                    if not (physSame t t2) then
                        failwith "invariant failed"

                    FakeUnit.ofUnit ()
            }
            |> t2.Apply
            |> FakeUnit.toUnit
        | k -> failwith $"invariant failed: %O{k}"
