namespace WoofWare.PlayFetch

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal StepFunctionNode =
    val advance : StepFunctionNode<'a> -> TimeNs -> unit
