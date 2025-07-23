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
        let y : Node<unit> = I.Var.Watch x |> I.Map (fun () -> failwith "oh no!")
        let o = I.Observe y

        expect {
            snapshotThrows @"System.Exception: oh no!"
            return! fun () -> I.Stabilize ()
        }

        expect {
            snapshotThrows @"System.AggregateException: cannot stabilize -- stabilize previously raised (oh no!)"
            return! fun () -> I.Stabilize ()
        }

        expect {
            snapshotThrows
                @"System.AggregateException: Observer.valueThrowing called after stabilize previously raised (oh no!)"

            return! fun () -> Observer.valueThrowing o
        }

        expect {
            snapshotThrows @"System.AggregateException: cannot set var -- stabilization previously raised (oh no!)"
            return! fun () -> I.Var.Set x ()
        }
