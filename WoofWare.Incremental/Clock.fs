namespace WoofWare.Incremental

open WoofWare.TimingWheel


[<RequireQualifiedAccess>]
module internal Clock =
    let invariant (t : Clock) =
      if t.Now.Value <> TimingWheel.now t.TimingWheel then
          failwith "invariant failure"
      if t.FiredAlarmValues.IsSome then
          failwith "invariant failure"
      TimingWheel.invariant AlarmValue.invariant t.TimingWheel

    let incrState (t : Clock) : State = Var.incrState t.Now

    let now (clock : Clock) : TimeNs = clock.Now.Value
    let timingWheelLength (clock : Clock) : int = TimingWheel.length clock.TimingWheel
