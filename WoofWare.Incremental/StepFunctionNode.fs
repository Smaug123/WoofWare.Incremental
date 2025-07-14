namespace WoofWare.PlayFetch

open System
open System.Collections.Generic
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    let physSame (t1 : StepFunctionNode<'a>) (t2 : StepFunctionNode<'b>) = Object.ReferenceEquals (t1, t2)

    let rec advance_internal (t : StepFunctionNode<'a>) to_ a1 (steps : IEnumerator<TimeNs * 'a>) =
      if steps.MoveNext () then
        let step_at, a2 = steps.Current
        if to_ >= step_at then
            advance_internal t to_ a2 steps
        else
            t.Value <- ValueSome a1
            t.UpcomingSteps <- stepsOld
      else
        t.Value <- ValueSome a1
        t.UpcomingSteps <- stepsOld

    let advance (t : StepFunctionNode<'a>) to_ =
        use s = t.UpcomingSteps.GetEnumerator ()
        advance_internal t to_ (ValueOption.get t.Value) s
