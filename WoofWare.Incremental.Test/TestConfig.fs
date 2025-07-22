namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Expect
open WoofWare.TimingWheel

[<TestFixture>]
module TestConfig =

    [<Test>]
    let ``default timing-wheel precision and level durations`` () =
        let Incr = Incremental.make ()
        let config = Incr.Clock.DefaultTimingWheelConfig
        let durations = TimingWheelConfig.durations config

        List.last durations >= TimeNs.Span.day |> shouldEqual true

        expect {
            snapshot @"1048576"
            return TimingWheelConfig.alarmPrecision config |> TimeNs.Span.toInt64Ns
        }

        expect {
            snapshot
                @"17s.179869100
01d15h05m37s.488355300
52d02h59m59s.627370400
104d05h59m59s.254740899
208d11h59m58s.509481899
416d23h59m57s.018963903
833d23h59m54s.037927896
1667d23h59m48s.075855792
3335d23h59m36s.151711702
6671d23h59m12s.303423524
13343d23h58m24s.606847048
26687d23h56m49s.213694096
53375d23h53m38s.427388191
106751d23h47m16s.854776382"

            return durations |> List.map TimeNs.Span.display |> String.concat "\n"
        }

    [<Test>]
    let ``default timing wheel can handle the full range of times`` () =
        let Incr = Incremental.make ()
        let clock = Incr.Clock.Create TimeNs.epoch
        let o = Incr.Observe (Incr.Clock.At clock TimeNs.maxValueRepresentable)
        Incr.Stabilize ()

        expect {
            snapshot "Before"
            return o
        }

        Incr.Clock.AdvanceClock clock TimeNs.maxValueRepresentable
        Incr.Stabilize ()

        expect {
            snapshot "After"
            return o
        }
