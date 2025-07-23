namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Expect

[<TestFixture>]
module TestExceptionalBehaviour =
    [<Test>]
    let ``handling and rethrowing`` () =
        let I = Incremental.make ()
        let x = I.Var.Create ()
        let y: Node<unit> =
            I.Var.Watch x
            |> I.Map (fun () ->
                failwith "oh no!"
            )
        let o = I.Observe y
        expect' {
            snapshotThrows @"System.Exception: oh no!"
            return! fun () -> I.Stabilize ()
        }
        expect' {
            snapshotThrows "cannot stabilize -- stabilize previously raised"
            return! fun () -> I.Stabilize ()
        }
        expect' {
            snapshotThrows "Observer.value_exn called after stabilize previously raised"
            return! fun () -> Observer.valueThrowing o
        }
        expect' {
            snapshotThrows "cannot set var -- stabilization previously raised"
            return! fun () -> I.Var.Set x ()
        }
