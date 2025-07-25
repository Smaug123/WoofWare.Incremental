namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open FsUnitTyped

[<TestFixture>]
module TestListFunctions =

    let testAll
        (fix : IncrementalFixture)
        (q : Node<bool> array -> Node<'a>)
        (listF : ('a -> 'a) -> bool list -> 'a)
        : unit
        =
        let I = fix.I

        for numVars = 0 to 3 do
            let vars = List.init numVars (fun _ -> I.Var.Create true)
            let q = I.Observe (q (Array.ofList (List.map I.Var.Watch vars)))
            let all = I.Observe (I.All (List.map I.Var.Watch vars))

            let rec loop vars =
                match vars with
                | [] ->
                    fix.Stabilize ()
                    let expected = listF id (Observer.valueThrowing all)
                    Observer.valueThrowing q |> shouldEqual expected
                | var :: vars ->
                    for b in [ false ; true ] do
                        I.Var.Set var b
                        loop vars

            loop vars

    [<Test>]
    let ``test exists`` () =
        let fix = IncrementalFixture.Make ()
        testAll fix fix.I.Exists List.exists

    [<Test>]
    let ``test forall`` () =
        let fix = IncrementalFixture.Make ()
        testAll fix fix.I.ForAll List.forall
