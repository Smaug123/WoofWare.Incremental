namespace WoofWare.Incremental

open WoofWare.TimingWheel

[<NoEquality ; NoComparison>]
type StepFunction<'a> =
    {
        Init : 'a
        Steps : (TimeNs * 'a) Sequence
    }

[<RequireQualifiedAccess>]
module StepFunction =

    let init (x : 'a StepFunction) = x.Init
    let steps (x : 'a StepFunction) = x.Steps

    let rec valueInternal (init : 'a) (steps : (TimeNs * 'a) Sequence) (at : TimeNs) : 'a =
        match Sequence.next steps with
        | None -> init
        | Some ((t, a), steps) -> if at < t then init else valueInternal a steps at

    let value (sf : 'a StepFunction) (at : TimeNs) : 'a = valueInternal sf.Init sf.Steps at

    let constant a =
        {
            Init = a
            Steps = Sequence.empty ()
        }

    let createExn (init : 'a) (steps : (TimeNs * 'a) list) =
        if not (List.isSortedBy fst steps) then
            failwith "createExn got unsorted times"

        {
            Init = init
            Steps = Sequence.ofList steps
        }

    let createFromSequence<'a> (init : 'a) (s : Sequence<TimeNs * 'a>) : StepFunction<'a> =
        {
            Init = init
            Steps = s
        }
