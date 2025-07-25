namespace WoofWare.Incremental

open WoofWare.TimingWheel

/// Models a function that takes different values over different intervals of time.
[<NoEquality ; NoComparison ; Sealed>]
type StepFunction<'a>

/// Models a function that takes different values over different intervals of time.
[<RequireQualifiedAccess>]
module StepFunction =

    /// Get the initial value of this step function before execution has begun.
    val init : 'a StepFunction -> 'a
    /// Get the list of timestamps at which this step function changes value, and the values it will assume
    /// at those times.
    val steps : 'a StepFunction -> (TimeNs * 'a) Sequence
    /// Get the value assumed by this step function at the given time.
    val value : 'a StepFunction -> at : TimeNs -> 'a

    /// The step function whose value is the given value at all times.
    val constant : 'a -> 'a StepFunction

    /// <summary>
    /// Given <c>[(t(1), v1), ..., (t(n), vn)]</c>, returns the step function whose value is <c>init</c> for
    /// all times strictly less than <c>t(1)</c>, then has value <c>vi</c> for time <c>t(i) &lt;= t &lt; t(i+1)</c>.
    /// </summary>
    /// <exception cref="Exception">The input step times must be nondecreasing; throws if that's not the case.</exception>
    val create : init : 'a -> steps : (TimeNs * 'a) list -> 'a StepFunction

    /// <summary>
    /// Given <c>[(t(1), v1), ..., (t(n), vn)]</c>, returns the step function whose value is <c>init</c> for
    /// all times strictly less than <c>t(1)</c>, then has value <c>vi</c> for time <c>t(i) &lt;= t &lt; t(i+1)</c>.
    /// </summary>
    val createFromSequence<'a> : init : 'a -> Sequence<TimeNs * 'a> -> StepFunction<'a>
