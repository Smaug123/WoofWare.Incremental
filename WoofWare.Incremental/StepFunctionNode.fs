namespace WoofWare.Incremental

open System
open System.Collections.Generic
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    let physSame (t1 : StepFunctionNode<'a>) (t2 : StepFunctionNode<'b>) = Object.ReferenceEquals (t1, t2)

    let rec advanceInternal (t : StepFunctionNode<'a>) (to_: TimeNs) (a1: 'a) (steps : IEnumerator<TimeNs * 'a>) : unit =
      if steps.MoveNext () then
        let step_at, a2 = steps.Current
        if to_ >= step_at then
            advanceInternal t to_ a2 steps
        else
            t.Value <- ValueSome a1
            t.UpcomingSteps <- stepsOld
      else
        t.Value <- ValueSome a1
        t.UpcomingSteps <- stepsOld

    let advance (t : StepFunctionNode<'a>) to_ =
        use s = t.UpcomingSteps.GetEnumerator ()
        advanceInternal t to_ (ValueOption.get t.Value) s

    let invariant<'a> (invA : 'a -> unit) (t : StepFunctionNode<'a>) : unit =
        match t.Main.Kind with
        | Kind.Invalid -> ()
        | Kind.Const _ ->
            // happens when upcomingSteps becomes empty
            ()
        | Kind.StepFunction t' -> if not (Object.ReferenceEquals (t, t')) then failwith "invariant failed"
        | k -> failwith $"invariant failed: {k}"

        t.Value |> ValueOption.iter invA
        match t.AlarmValue.Action with
        | AlarmValueAction.StepFunction t2 ->
            { new StepFunctionNodeEval<_> with
                member _.Eval t2 =
                    if not (Object.ReferenceEquals (t, t2)) then
                        failwith "invariant failed"
                    FakeUnit.ofUnit ()
             }
            |> t2.Apply
            |> FakeUnit.toUnit
        | k -> failwith $"invariant failed: {k}"
