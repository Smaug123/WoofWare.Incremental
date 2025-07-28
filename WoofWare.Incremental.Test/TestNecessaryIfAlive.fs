namespace WoofWare.Incremental.Test

open System
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestNecessaryIfAlive =
    [<Test ; Explicit "not yet passing">]
    let ``dead is unnecessary`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let push, check = onUpdateQueue ()
        I.OnUpdate (I.Var.Watch x) push
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]
        let t = I.NecessaryIfAlive (I.Var.Watch x)
        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        I.Var.Set x 14
        fix.Stabilize ()
        check [ NodeUpdate.Changed (13, 14) ]
        GC.KeepAlive t

        Gc.collect ()

        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]

    [<Test>]
    let ``cutoff is preserved`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        I.Var.Watch x |> I.SetCutoff Cutoff.never

        let t = I.NecessaryIfAlive (I.Var.Watch x)
        let o = I.Observe t
        let push, check = onUpdateQueue ()
        I.OnUpdate t push

        fix.Stabilize ()
        check [ NodeUpdate.Necessary 13 ]
        I.Var.Set x 14
        fix.Stabilize ()
        check [ NodeUpdate.Changed (13, 14) ]
        I.Var.Set x 14
        fix.Stabilize ()
        check [ NodeUpdate.Changed (14, 14) ]

        Observer.disallowFutureUse o
        Gc.collect ()
        fix.Stabilize ()
        check [ NodeUpdate.Unnecessary ]
