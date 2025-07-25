namespace WoofWare.Incremental

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    val advance : StepFunctionNode<'a> -> TimeNs -> unit

    val invariant<'a> : ('a -> unit) -> 'a StepFunctionNode -> unit
