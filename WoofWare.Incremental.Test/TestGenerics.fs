namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel

[<TestFixture>]
module TestGenerics =

    (* This code tests that generic functions can operate on [Incremental.Make] values. *)
    let I = Incremental.make ()

    [<Test>]
    let ``test 1`` () =
        let i = I.Return ()
        let _ = Incremental.map i id
        let o = I.Observe i
        let _ = Observer.value o
        let v = I.Var.Create ()
        let _ = Var.value v
        let _ = Scope.within I.State I.Scope.top id
        let _ = Clock.now (I.Clock.Create TimeNs.epoch)

        let _ =
            ExpertNode.addDependency (I.Expert.Node.create id) (I.Expert.Dependency.create i)

        ()

    [<Test>]
    let ``test State create`` () =
        let i = State.create ()
        let state = I
        let _ = Incremental.konst state 13
        ()
(*
let%expect_test "[State.create]" =
  let module I = (val Incremental.State.create ()) in
  let state = I.t in
  let (_ : (int, I.state_witness) Incremental.t) = Incremental.const state 13 in
  ()
;;
        *)
