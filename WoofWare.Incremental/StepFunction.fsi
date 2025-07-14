namespace WoofWare.PlayFetch

open System
open System.Collections.Generic
open WoofWare.TimingWheel

[<NoEquality ; NoComparison ; Sealed>]
type StepFunction<'a> =
    interface IDisposable

[<RequireQualifiedAccess>]
module StepFunction =

    val init : 'a StepFunction -> 'a
    val steps : 'a StepFunction -> (TimeNs * 'a) IEnumerator
    val value : 'a StepFunction -> at : TimeNs -> 'a

    /// [constant a] is the step function [t] with [value t ~at = a] for all [at].
    val constant : 'a -> 'a StepFunction

    /// [create_exn ~init ~steps:[(t_1, v_1); ...; (t_n, vn)]] is the step function [t] with
    ///    [value t ~at = init] for [at < t_1], [value t ~at = vi] for [t_i <= at < t_i+1].
    ///    [create_exn] raises if the times aren't in nondecreasing order, i.e. if for some
    ///    [i < j], [ti > tj].
    val createExn : init : 'a -> steps : (TimeNs * 'a) list -> 'a StepFunction

    val createFromSequence : init : 'a -> steps : (TimeNs * 'a) seq -> 'a StepFunction
