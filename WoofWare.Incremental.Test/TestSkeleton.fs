namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.Incremental.Skeleton
open WoofWare.Expect

[<TestFixture>]
module TestSkeleton =
    [<Test>]
    let ``render dot file`` () : unit =
        let I = Incremental.make ()
        let n1 = I.Const 1
        let n2 = I.Const 2

        let res =
            n1
            |> I.Bind (fun n1 -> if n1 % 2 = 0 then n2 |> I.Map (fun n2 -> n1 * n2) else n2)

        let obs = I.Observe res
        I.Stabilize ()

        let result = Observer.valueThrowing obs
        result |> shouldEqual 2

        let skeleton = Skeleton.snapshot (Some true) I.State

        expect {
            snapshot
                @"nodes:
{
  id: 4
  kind: BindMain
  children: (3 2)
  recomputedAt: (0)
  changedAt: (0)
  height: (2)
}
{
  id: 3
  kind: BindLhsChange
  children: (1)
  recomputedAt: (0)
  cutoff: (Never)
  changedAt: (0)
  height: (1)
}
{
  id: 1
  kind: Const
  recomputedAt: (0)
  changedAt: (0)
  height: (0)
}
{
  id: 2
  kind: Const
  recomputedAt: (0)
  changedAt: (0)
  height: (0)
}
seen: (1 2 3 4)
numStabilizes:1"

            return skeleton
        }

        let dot = Skeleton.toDot None (Some RenderTarget.Dot) None None skeleton
        let graphEasy = Skeleton.toDot None (Some RenderTarget.GraphEasy) None None skeleton

        expect {
            snapshot
                @"  digraph G {
    rankdir = TB
    bgcolor = transparent
-     n4 [shape=Mrecord label=""{{n4|BindMain|height=2}}""  ""fontname""=""Sans Serif""]
-     n3 [shape=Mrecord label=""{{n3|BindLhsChange|height=1}}""  ""fontname""=""Sans Serif""]
-     n1 [shape=Mrecord label=""{{n1|Const|height=0}}""  ""fontname""=""Sans Serif""]
-     n2 [shape=Mrecord label=""{{n2|Const|height=0}}""  ""fontname""=""Sans Serif""]
+     n4 [shape=box label=""{{n4|BindMain|height=2}}"" ]
+     n3 [shape=box label=""{{n3|BindLhsChange|height=1}}"" ]
+     n1 [shape=box label=""{{n1|Const|height=0}}"" ]
+     n2 [shape=box label=""{{n2|Const|height=0}}"" ]
    n3 -> n4
    n2 -> n4
    n1 -> n3
  }"

            return Diff.patience dot graphEasy |> Diff.format
        }

    [<Test>]
    let ``no binds`` () =
        let I = Incremental.make ()
        let node = I.Return "hello" |> I.Map (fun x -> x + "!")
        let result = I.Observe node
        I.Stabilize ()

        let result = Observer.valueThrowing result
        result |> shouldEqual "hello!"

        let skeleton = Skeleton.snapshot (Some true) I.State

        expect {
            snapshot
                @"nodes:
{
  id: 2
  kind: Map
  children: (1)
  recomputedAt: (0)
  changedAt: (0)
  height: (1)
}
{
  id: 1
  kind: Const
  recomputedAt: (0)
  changedAt: (0)
  height: (0)
}
seen: (1 2)
numStabilizes:1"

            return skeleton
        }

        let dot = Skeleton.toDot None (Some RenderTarget.GraphEasy) None None skeleton

        expect {
            snapshot
                @"digraph G {
  rankdir = TB
  bgcolor = transparent
    n2 [shape=box label=""{{n2|Map|height=1}}"" ]
    n1 [shape=box label=""{{n1|Const|height=0}}"" ]
  n1 -> n2
}"

            return dot
        }

    [<Test ; Explicit "requires graph-easy">]
    let ``The render from No Binds`` () =
        let s =
            """digraph G {
  rankdir = TB
  bgcolor = transparent
    n2 [shape=box label="{{n2|Map|height=1}}" ]
    n1 [shape=box label="{{n1|Const|height=0}}" ]
  n1 -> n2
}"""

        expect {
            snapshot
                @"
┌───────────────────────┐
│ {{n1|Const|height=0}} │
└───────────────────────┘
  │
  │
  ▼
┌───────────────────────┐
│  {{n2|Map|height=1}}  │
└───────────────────────┘
"

            return Dot.render s
        }

    [<Test>]
    let ``with binds`` () =
        let I = Incremental.make ()

        let mkIncr i =
            let v = I.Var.Create i
            I.Var.Watch v, v

        let a, ai = mkIncr 0
        let b, _bi = mkIncr 3
        let c, _ci = mkIncr 4

        let node =
            let isEven = a |> I.Map (fun a -> a % 2 = 0)

            isEven |> I.Bind (fun isEven -> if isEven then I.Return 0 else I.Map2 (*) b c)

        let observer = I.Observe node
        I.Stabilize ()

        let result = Observer.valueThrowing observer
        result |> shouldEqual 0

        let skeleton = Skeleton.snapshot (Some true) I.State

        expect {
            snapshot
                @"nodes:
{
  id: 6
  kind: BindMain
  children: (5 7)
  recomputedAt: (0)
  changedAt: (0)
  height: (4)
}
{
  id: 5
  kind: BindLhsChange
  children: (4)
  bindChildren: (7)
  recomputedAt: (0)
  cutoff: (Never)
  changedAt: (0)
  height: (2)
}
{
  id: 4
  kind: Map
  children: (1)
  recomputedAt: (0)
  changedAt: (0)
  height: (1)
}
{
  id: 1
  kind: Var
  recomputedAt: (0)
  changedAt: (0)
  height: (0)
}
{
  id: 7
  kind: Const
  recomputedAt: (0)
  changedAt: (0)
  height: (3)
}
seen: (1 4 5 6 7)
numStabilizes:1"

            return skeleton
        }

        I.Var.Set ai 3
        I.Stabilize ()
        Observer.valueThrowing observer |> shouldEqual 12

        let newSkeleton = Skeleton.snapshot (Some true) I.State

        expect' {
            snapshot
                @"  nodes:
  {
    id: 6
    kind: BindMain
-   children: (5 7)
-   recomputedAt: (0)
-   changedAt: (0)
+   children: (5 8)
+   recomputedAt: (1)
+   changedAt: (1)
    height: (4)
  }
  {
    id: 5
    kind: BindLhsChange
    children: (4)
-   bindChildren: (7)
-   recomputedAt: (0)
+   bindChildren: (8)
+   recomputedAt: (1)
    cutoff: (Never)
-   changedAt: (0)
+   changedAt: (1)
    height: (2)
  }
  {
    id: 4
    kind: Map
    children: (1)
-   recomputedAt: (0)
-   changedAt: (0)
+   recomputedAt: (1)
+   changedAt: (1)
    height: (1)
  }
  {
    id: 1
    kind: Var
-   recomputedAt: (0)
-   changedAt: (0)
+   recomputedAt: (1)
+   changedAt: (1)
    height: (0)
  }
  {
-   id: 7
-   kind: Const
-   recomputedAt: (0)
-   changedAt: (0)
+   id: 8
+   kind: Map2
+   children: (2 3)
+   recomputedAt: (1)
+   changedAt: (1)
    height: (3)
  }
- seen: (1 4 5 6 7)
- numStabilizes:1
+ {
+   id: 2
+   kind: Var
+   recomputedAt: (1)
+   changedAt: (1)
+   height: (0)
+ }
+ {
+   id: 3
+   kind: Var
+   recomputedAt: (1)
+   changedAt: (1)
+   height: (0)
+ }
+ seen: (1 2 3 4 5 6 8)
+ numStabilizes:2"

            withFormat Diff.format
            return Diff.patience (skeleton.ToString ()) (newSkeleton.ToString ())
        }

    (*
         (height                      (height
    -     4                      +     5
         ))                           ))
        ((id 5)                      ((id 5)
         (kind Bind_lhs_change)       (kind Bind_lhs_change)
         (children (4))               (children (4))
         (bind_children               (bind_children
    -     (7)                    +     (9 8)
         )                            )
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (cutoff Never)               (cutoff Never)
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 2))                  (height 2))
        ((id 4)                      ((id 4)
         (kind Map)                   (kind Map)
         (children (1))               (children (1))
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 1))                  (height 1))
        ((id 1)                      ((id 1)
         (kind Var)                   (kind Var)
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 0))                  (height 0))
                                 +   ((id 9) (kind Map) (children (8)) (recomputed_at 1) (changed_at 1)
                                 +    (height 4))
        ((id                         ((id
    -     7                      +     8
         )                            )
         (kind                        (kind
    -     Const                  +     Map2
         )                            )
                                 +    (children (2 3))
         (recomputed_at               (recomputed_at
    -     0                      +     1
         )                            )
         (changed_at                  (changed_at
    -     0                      +     1
         )                            )
         (height 3))                  (height 3))
                                 +   ((id 2) (kind Var) (recomputed_at 1) (changed_at 1) (height 0))
                                 +   ((id 3) (kind Var) (recomputed_at 1) (changed_at 1) (height 0))
       ))                           ))
      (seen                        (seen
       (1                           (1
                                 +   2
                                 +   3
        4                            4
        5                            5
        6                            6
                                 +   8
    -   7                        +   9
       ))                           ))
      (num_stabilizes              (num_stabilizes
    -  1                         +  2
      ))                           ))
    |}];
  let dot = Incremental_skeleton.to_dot ~render_target:Graph_easy skeleton in
  print_endline dot;
  [%expect
    {|
    digraph G {
      rankdir = TB
      bgcolor = transparent
        n6 [shape=box label="{{n6|Bind_main|height=4}}" ]
        n5 [shape=box label="{{n5|Bind_lhs_change|height=2}}" ]
        n4 [shape=box label="{{n4|Map|height=1}}" ]
        n1 [shape=box label="{{n1|Var|height=0}}" ]
        n7 [shape=box label="{{n7|Const|height=3}}" ]
      n5 -> n6
      n7 -> n6
      n4 -> n5
      n1 -> n4
      n5 -> n7 [style=dashed]
    }
    |}];
  Expect_test_graphviz.print_dot_blocking dot;
  [%expect
    {|
    ┌─────────────────────────────────┐
    │       {{n1|Var|height=0}}       │
    └─────────────────────────────────┘
      │
      │
      ▼
    ┌─────────────────────────────────┐
    │       {{n4|Map|height=1}}       │
    └─────────────────────────────────┘
      │
      │
      ▼
    ┌─────────────────────────────────┐
    │ {{n5|Bind_lhs_change|height=2}} │ ─┐
    └─────────────────────────────────┘  │
      ╵                                  │
      ╵                                  │
      ▼                                  │
    ┌─────────────────────────────────┐  │
    │      {{n7|Const|height=3}}      │  │
    └─────────────────────────────────┘  │
      │                                  │
      │                                  │
      ▼                                  │
    ┌─────────────────────────────────┐  │
    │    {{n6|Bind_main|height=4}}    │ ◀┘
    └─────────────────────────────────┘
    |}]
;;
*)

    [<Test>]
    let ``unobserved incr graph`` () =
        let I = Incremental.make ()
        let node = I.Return "hello" |> I.Map (fun x -> x + "!")
        let result = I.Observe node
        I.Stabilize ()

        expect {
            snapshot
                @"nodes:
{
  id: 2
  kind: Map
  children: (1)
  recomputedAt: (0)
  changedAt: (0)
  height: (1)
}
{
  id: 1
  kind: Const
  recomputedAt: (0)
  changedAt: (0)
  height: (0)
}
seen: (1 2)
numStabilizes:1"

            return Skeleton.snapshot (Some true) I.State
        }

        Observer.disallowFutureUse result
        I.Stabilize ()

        expect {
            snapshot
                @"nodes:

seen: ()
numStabilizes:2"

            return Skeleton.snapshot (Some true) I.State
        }
