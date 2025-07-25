namespace WoofWare.Incremental.Test

open System.Text
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Expect

[<TestFixture>]
module TestDot =
    let printNode (nodes : NodeCrate list) : string =
        let sb = StringBuilder ()

        nodes |> NodeToDot.renderDot true true (sb.Append >> ignore<StringBuilder>)

        sb.ToString ()

    [<Test>]
    let ``plain node graphviz`` () =
        let I = Incremental.make ()
        let n = I.Return "hello"

        expect {
            snapshot
                @"digraph G {
  rankdir = BT
  n### [shape=Mrecord label=""{{n###|Const|height=-1}}"" ]
}
"

            return [ NodeCrate.make n ] |> printNode
        }

    [<Test>]
    let ``annotated with info`` () =
        let I = Incremental.make ()
        let n = I.Return "hello"
        Node.setUserInfo n (Some "hello world")

        expect {
            snapshot
                @"digraph G {
  rankdir = BT
  n### [shape=Mrecord label=""{{hello\ world}|{n###|Const|height=-1}}"" ]
}
"

            return [ NodeCrate.make n ] |> printNode
        }

    [<Test>]
    let ``annotated with label and attributes`` () =
        let I = Incremental.make ()
        let n = I.Return "hello"
        Node.appendUserInfoGraphviz n [ "hello" ; "world" ] (Map.ofList [ "fillcolor", "green" ])

        expect {
            snapshot
                @"digraph G {
  rankdir = BT
  n### [shape=Mrecord label=""{{hello|world}|{n###|Const|height=-1}}""  ""fillcolor""=""green""]
}
"

            return [ NodeCrate.make n ] |> printNode
        }
