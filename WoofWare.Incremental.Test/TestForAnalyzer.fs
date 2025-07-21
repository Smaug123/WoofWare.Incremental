namespace WoofWare.Incremental.Test

open System
open System.Collections.Generic
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Expect

type NodePrint =
  { Id : NodeId
  ; Kind : ForAnalyzer.Kind
  ; Cutoff : ForAnalyzer.Cutoff
  ; Children : NodeId IReadOnlyList
  ; BindChildren : NodeId IReadOnlyList
  ; RecomputedAt : StabilizationNum
  ; ChangedAt : StabilizationNum
  ; Height : int
  }

  override this.ToString () =
      let children = this.Children |> Seq.map NodeId.toString |> String.concat " "
      let bindChildren = this.BindChildren |> Seq.map NodeId.toString |> String.concat " "
      $"(node (
  (id %i{NodeId.toInt this.Id})
  (kind %s{ForAnalyzer.Kind.toString this.Kind})
  (cutoff %s{ForAnalyzer.Cutoff.toString this.Cutoff})
  (children      (%s{children}))
  (bind_children (%s{bindChildren}))
  (recomputed_at %i{StabilizationNum.toInt this.RecomputedAt})
  (changed_at    %i{StabilizationNum.toInt this.ChangedAt})
  (height        %i{this.Height})))"

[<TestFixture>]
module TestForAnalyzer =
    let printNodes (nodeList: 'a list) (pack: 'a -> NodeCrate) (outputNode : NodePrint -> string) : string =
        let nodes = ResizeArray<NodePrint> ()
        ForAnalyzer.traverse (List.map pack nodeList) (fun id kind cutoff children bindChildren _userInfo recomputedAt changedAt height ->
            nodes.Add { Id = id ; Kind = kind ; Cutoff = cutoff ; Children = children ; BindChildren = bindChildren ; RecomputedAt = recomputedAt ; ChangedAt = changedAt ; Height = height }
        )

        let minIdOpt =
            nodes
            |> Seq.map (fun node -> NodeId.toString node.Id |> System.Int32.Parse)
            |> Seq.tryMin

        let result = ResizeArray ()
        match minIdOpt with
        | None -> ()
        | Some minId ->
            let newId nodeId =
                let idNum = (NodeId.toString nodeId |> System.Int32.Parse) - minId + 1
                NodeId.ofInt idNum

            for node in nodes do
                let node =
                    let newChildren = node.Children |> Seq.map newId |> ResizeArray
                    let newBindChildren = node.BindChildren |> Seq.map newId |> ResizeArray
                    { node with
                         Id = newId node.Id
                         Children = newChildren
                         BindChildren = newBindChildren
                    }
                outputNode node
                |> result.Add
        result.Reverse ()
        result
        |> String.concat "\n"

    let printComputationInfo node =
        let id = node.Id
        let recomputedAt = node.RecomputedAt
        let changedAt = node.ChangedAt
        $"(id %s{NodeId.toString id}) (recomputedAt %i{StabilizationNum.toInt recomputedAt}) (changedAt %i{StabilizationNum.toInt changedAt})"

    [<Test>]
    let ``traverses basic`` () =
        let Incr = Incremental.make ()

        let n = Incr.Return "hello"
        expect {
            snapshot @"(node (
  (id 1)
  (kind Const)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))"
            return printNodes [ n ] Incr.Pack (fun n -> n.ToString ())
        }

    [<Test>]
    let ``traverses map`` () =
        let Incr = Incremental.make ()

        let n = Incr.Return "hello"
        let r = Incr.Map (fun s -> s + "!") n

        expect {
            snapshot @"(node (
  (id 1)
  (kind Const)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))
(node (
  (id 2)
  (kind Map)
  (cutoff PhysEqual)
  (children      (1))
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))"
            return printNodes [r] Incr.Pack (fun n -> n.ToString ())
        }

    [<Test>]
    let ``traverses bind`` () =
        let Incr = Incremental.make ()

        let x = Incr.VarCreate 1
        let a = Incr.Return 3
        let b = Incr.Return 4

        let cond = Incr.Map (fun i -> i % 2 = 0) (Incr.VarWatch x)

        let c = Incr.Bind (fun bool -> if bool then a else Incr.Map (fun i -> i * 4) b) cond

        expect {
            snapshot @"(node (
  (id 1)
  (kind Var)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))
(node (
  (id 4)
  (kind Map)
  (cutoff PhysEqual)
  (children      (1))
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))
(node (
  (id 5)
  (kind BindLhsChange)
  (cutoff Never)
  (children      (4))
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))
(node (
  (id 6)
  (kind BindMain)
  (cutoff PhysEqual)
  (children      (5))
  (bind_children ())
  (recomputed_at -1)
  (changed_at    -1)
  (height        -1)))"
            return printNodes [c] Incr.Pack (fun n -> n.ToString ())
        }

        let observerSoThatStabilizationPerformsWork = Incr.Observe c
        Incr.Stabilize ()
        GC.KeepAlive observerSoThatStabilizationPerformsWork

        expect {
            snapshot @"(node (
  (id 3)
  (kind Const)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        0)))
(node (
  (id 7)
  (kind Map)
  (cutoff PhysEqual)
  (children      (3))
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        3)))
(node (
  (id 1)
  (kind Var)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        0)))
(node (
  (id 4)
  (kind Map)
  (cutoff PhysEqual)
  (children      (1))
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        1)))
(node (
  (id 5)
  (kind BindLhsChange)
  (cutoff Never)
  (children      (4))
  (bind_children (7))
  (recomputed_at 0)
  (changed_at    0)
  (height        2)))
(node (
  (id 6)
  (kind BindMain)
  (cutoff PhysEqual)
  (children      (5 7))
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        4)))"
            return printNodes [c] Incr.Pack (fun n -> n.ToString ())
        }

    [<Test>]
    let ``different recomputedAt and changedAt`` () =
        let Incr = Incremental.make ()
        let a = Incr.VarCreate 3
        let aVal = Incr.VarWatch a
        let mult = aVal |> Incr.Map (fun a -> a % 2)
        let multObserver = Incr.Observe mult

        Incr.Stabilize ()

        expect {
            snapshot @"(id 1) (recomputedAt 0) (changedAt 0)
(id 2) (recomputedAt 0) (changedAt 0)"
            return printNodes [mult] Incr.Pack printComputationInfo
        }

        Incr.VarSet a 1
        Incr.Stabilize ()

        (* NOTE: The reason that [recomputed_at] and [changed_at] are different here, is that -
           according to the doc comments of incremental:

           - [recomputed_at] is the last stabilization when [t]'s value was recomputed, even if
             it was cut off.
           - [changed_at] is the last stabilization when this node was computed and not cut off.
             It is used to detect when [t]'s parents are stale and (because all parents are
             necessary) need to be recomputed.

           The modulo node was "recomputed" (3 % 2) and (1 % 2), but it did not "change" as the resulting
           value was the "same" according to its phys_equal cutoff. *)

        GC.KeepAlive multObserver
        expect {
            snapshot @"(id 1) (recomputedAt 1) (changedAt 1)
(id 2) (recomputedAt 1) (changedAt 0)"
            return printNodes [mult] Incr.Pack printComputationInfo
        }

    [<Test>]
    let ``directly observes all observers`` () =
        let Incr = Incremental.make ()
        let a = Incr.Return "hello"
        let b = a |> Incr.Map (fun s -> s + "!")
        let a' = Incr.Return "world"
        let b' = a' |> Incr.Map (fun s -> s + ".")
        let bObs = Incr.Observe b
        let b'Obs = Incr.Observe b'
        expect {
            snapshot ""
            return printNodes (ForAnalyzer.directlyObserved Incr.State) id (fun n -> n.ToString ())
        }

        Incr.Stabilize ()
        GC.KeepAlive bObs
        GC.KeepAlive b'Obs

        expect' {
            snapshot @"(node (
  (id 1)
  (kind Const)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        0)))
(node (
  (id 2)
  (kind Map)
  (cutoff PhysEqual)
  (children      (1))
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        1)))
(node (
  (id 3)
  (kind Const)
  (cutoff PhysEqual)
  (children      ())
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        0)))
(node (
  (id 4)
  (kind Map)
  (cutoff PhysEqual)
  (children      (3))
  (bind_children ())
  (recomputed_at 0)
  (changed_at    0)
  (height        1)))"
            return printNodes (ForAnalyzer.directlyObserved Incr.State) id (fun n -> n.ToString ())
        }
