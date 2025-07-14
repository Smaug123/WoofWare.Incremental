namespace WoofWare.PlayFetch

open System.Collections.Generic
open WoofWare.TimingWheel

[<NoEquality ; NoComparison>]
type StepFunction<'a> =
    {
        Init : 'a
        Steps : (TimeNs * 'a) IEnumerator
    }

    interface System.IDisposable with
        member this.Dispose () = this.Steps.Dispose ()

[<RequireQualifiedAccess>]
module StepFunction =

    let init (x : 'a StepFunction) = x.Init
    let steps (x : 'a StepFunction) = x.Steps

    let rec valueInternal (init : 'a) (steps : (TimeNs * 'a) IEnumerator) (at : TimeNs) : 'a =
        if not (steps.MoveNext ()) then
            init
        else
            let t, a = steps.Current
            if at < t then init else valueInternal a steps at

    let value (sf : 'a StepFunction) (at : TimeNs) : 'a = valueInternal sf.Init sf.Steps at

    let constant a =
        {
            Init = a
            Steps = Seq.empty.GetEnumerator ()
        }

    let createExn (init : 'a) (steps : (TimeNs * 'a) list) =
        if not (List.isSortedBy fst steps) then
            failwith "createExn got unsorted times"

        {
            Init = init
            Steps = (Seq.ofList steps).GetEnumerator ()
        }

    let createFromSequence init (steps : _ seq) =
        {
            Init = init
            Steps = steps.GetEnumerator ()
        }
