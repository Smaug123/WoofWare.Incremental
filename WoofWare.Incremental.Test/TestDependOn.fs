namespace WoofWare.Incremental.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental

[<TestFixture>]
module TestDependOn =

   [<Test>]
   let ``test 1`` () =
      let fix = IncrementalFixture.Make ()
      let I = fix.I

      let x = I.Var.Create 13
      let y = I.Var.Create 14
      let d = I.DependOn (I.Var.Watch y) (I.Var.Watch x)
      let o = I.Observe d
      let nx = ref 0
      let incrO (r : int ref) u =
          match u with
          | Observer.Update.Invalidated -> failwith "should not call"
          | _ -> r.Value <- r.Value + 1
      let incrR (r : int ref) u =
          match u with
          | NodeUpdate.Invalidated -> failwith "should not call"
          | NodeUpdate.Unnecessary -> ()
          | _ -> r.Value <- r.Value + 1

      Observer.onUpdateThrowing o (incrO nx)
      let ny = ref 0
      I.OnUpdate (I.Var.Watch y) (incrR ny)

      let check (eo : int) (enx : int) (eny : int) =
          fix.Stabilize ()
          Observer.valueThrowing o |> shouldEqual eo
          nx.Value |> shouldEqual enx
          ny.Value |> shouldEqual eny

      check 13 1 1
      I.Var.Set x 15
      check 15 2 1
      I.Var.Set y 16
      check 15 2 2;
      I.Var.Set x 17
      I.Var.Set y 18
      check 17 3 3
      I.Var.Set x 17
      check 17 3 3
      I.Var.Set y 18
      check 17 3 3
      Observer.disallowFutureUse o

      let check enx eny =
          fix.Stabilize ()
          nx.Value |> shouldEqual enx
          ny.Value |> shouldEqual eny

      I.Var.Set x 19
      I.Var.Set y 20
      check 3 3
      let o = I.Observe d

      Observer.onUpdateThrowing o (incrO nx)
      check 4 4
      Observer.valueThrowing o |> shouldEqual 19

   [<Test>]
   let ``propagating the dependOn while the result of dependOn is not observable`` () =
      let fix = IncrementalFixture.Make ()
      let I = fix.I

      let var = I.Var.Create 1
      let depend = I.DependOn (I.Const ()) (I.Var.Watch var)

      let o = I.Observe depend
      fix.Stabilize ()
      Observer.valueThrowing o |> shouldEqual 1
      Observer.disallowFutureUse o
      let o = I.Observe (I.Var.Watch var)
      I.Var.Set var 2
      fix.Stabilize ()
      Observer.valueThrowing o |> shouldEqual 2
      Observer.disallowFutureUse o
      let o = I.Observe depend
      fix.Stabilize ()
      Observer.valueThrowing o |> shouldEqual 2

   [<Test>]
   let ``dependOn doesn't cut off using physEqual`` () =
      let fix = IncrementalFixture.Make ()
      let I = fix.I

      let v1 = I.Var.Create ()
      let v2 = I.Var.Create 1
      I.SetCutoff (I.Var.Watch v1) Cutoff.never

      let o =
        I.Var.Watch v1
        |> I.DependOn (I.Var.Watch v2)
        |> I.Observe

      let mutable updates = 0
      Observer.onUpdateThrowing o (fun _ -> Interlocked.Increment &updates |> ignore<int>)

      updates |> shouldEqual 0
      fix.Stabilize ()
      updates |> shouldEqual 1

      I.Var.Set v2 2
      fix.Stabilize ()
      updates |> shouldEqual 1

      I.Var.Set v1 ()
      fix.Stabilize ()
      updates |> shouldEqual 2

      Observer.disallowFutureUse o
