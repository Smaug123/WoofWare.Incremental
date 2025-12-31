namespace WoofWare.Incremental.Test

open WoofWare.Incremental
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestSyntax =

    [<Test>]
    let ``simple examples of let! and and! (map and bind)`` () =
        let I = Incremental.make ()
        let incr = IncrementalBuilder.create I

        let xi = I.Var.Create 13

        // Using and! for applicative combination (more efficient, uses Map2)
        let i1 =
            incr {
                let! a = I.Var.Watch xi
                and! b = I.Var.Watch xi
                return a + b
            }

        let xb = I.Var.Create true

        // Using let! for monadic bind
        let i2 =
            incr {
                let! b = I.Var.Watch xb

                return! if b then I.Return 17 else I.Return 19
            }

        let o1 = I.Observe i1
        let o2 = I.Observe i2
        I.Stabilize ()

        Observer.value o1 |> shouldEqual 26
        Observer.value o2 |> shouldEqual 17

    [<Test>]
    let ``simple example of using map3 via and!`` () =
        let I = Incremental.make ()
        let incr = IncrementalBuilder.create I

        let x = I.Var.Create 13
        let y = I.Var.Create 42
        let z = I.Var.Create 12

        // With 3 and! clauses, MergeSources is chained creating nested tuples: ((a, b), c)
        let xyz =
            incr {
                let! x = I.Var.Watch x
                and! y = I.Var.Watch y
                and! z = I.Var.Watch z
                return (x, y, z)
            }

        let o = I.Observe xyz
        I.Stabilize ()

        Observer.value o |> shouldEqual (13, 42, 12)

        I.Var.Set x 100
        I.Stabilize ()

        Observer.value o |> shouldEqual (100, 42, 12)

    [<Test>]
    let ``simple example of using bind3 via and! followed by return!`` () =
        let I = Incremental.make ()
        let incr = IncrementalBuilder.create I

        let x = I.Var.Create 13
        let y = I.Var.Create 42
        let z = I.Var.Create 12

        let xyz =
            incr {
                let! x = I.Var.Watch x
                and! y = I.Var.Watch y
                and! z = I.Var.Watch z
                return! I.Return (x, y, z)
            }

        let o = I.Observe xyz
        I.Stabilize ()

        Observer.value o |> shouldEqual (13, 42, 12)

        I.Var.Set x 100
        I.Stabilize ()

        Observer.value o |> shouldEqual (100, 42, 12)

    [<Test>]
    let ``BindReturn optimization with single let!`` () =
        let I = Incremental.make ()
        let incr = IncrementalBuilder.create I

        let x = I.Var.Create 10

        // This should use BindReturn (i.e., Map) rather than Bind + Return
        let doubled =
            incr {
                let! v = I.Var.Watch x
                return v * 2
            }

        let o = I.Observe doubled
        I.Stabilize ()

        Observer.value o |> shouldEqual 20

        I.Var.Set x 25
        I.Stabilize ()

        Observer.value o |> shouldEqual 50

    [<Test>]
    let ``MergeSources plus BindReturn with two let! and!`` () =
        let I = Incremental.make ()
        let incr = IncrementalBuilder.create I

        let x = I.Var.Create 3
        let y = I.Var.Create 7

        // This uses MergeSources (Both) then BindReturn (Map)
        let product =
            incr {
                let! a = I.Var.Watch x
                and! b = I.Var.Watch y
                return a * b
            }

        let o = I.Observe product
        I.Stabilize ()

        Observer.value o |> shouldEqual 21

        I.Var.Set x 5
        I.Stabilize ()

        Observer.value o |> shouldEqual 35

        I.Var.Set y 10
        I.Stabilize ()

        Observer.value o |> shouldEqual 50
