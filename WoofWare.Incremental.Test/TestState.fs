namespace WoofWare.Incremental.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental
open WoofWare.Expect

[<TestFixture>]
module TestState =
    [<Test>]
    let ``default max height`` () =
      let I = Incremental.make ()
      State.maxHeightAllowed I.State |> shouldEqual 128

    [<Test>]
    let ``max height seen`` () =
      let fix = IncrementalFixture.Make ()
      // because of the Invalid we made
      State.maxHeightSeen fix.I.State |> shouldEqual 3

    [<TestCase 1>]
    [<TestCase 2>]
    let ``can't set smaller height`` (height : int) =
      let fix = IncrementalFixture.Make ()
      expect' {
          snapshotThrows ""
          return! fun () -> State.setMaxHeightAllowed fix.I.State height
      }

    [<Test>]
    let ``max height setting`` () =
      let fix = IncrementalFixture.Make ()
      State.setMaxHeightAllowed fix.I.State 10
      State.maxHeightAllowed fix.I.State |> shouldEqual 10

    [<Test>]
    let ``observe max height`` () =
      let fix = IncrementalFixture.Make ()
      let I = fix.I

      State.setMaxHeightAllowed I.State 256
      let rec loop n =
        if n = 0 then
            I.Return 0
        else
            loop (n - 1)
        |> I.Map (fun i -> i + 1)

      let o = I.Observe (loop (State.maxHeightAllowed I.State))

      fix.Stabilize ()
      Observer.valueThrowing o |> shouldEqual (State.maxHeightAllowed I.State)

      State.maxHeightAllowed I.State |> shouldEqual (State.maxHeightSeen I.State)
      State.invariant I.State

      do
          GC.Collect ()
          GC.WaitForPendingFinalizers ()
          GC.Collect ()

      fix.Stabilize ()

      do
          let n = State.numActiveObservers I.State
          let o = I.Observe (I.Const 0)
          Observer.disallowFutureUse o
          State.numActiveObservers I.State |> shouldEqual n


        let%expect_test _ =
          Gc.full_major ();
          stabilize_ [%here];
          let n = num_active_observers t in
          let o = observe (const 0) in
          disallow_future_use o;
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let%expect_test _ =
          Gc.full_major ();
          stabilize_ [%here];
          let n = num_active_observers t in
          let o = observe (const 0) in
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:(n + 1);
          disallow_future_use o;
          [%test_result: int] (num_active_observers t) ~expect:n;
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let%expect_test _ =
          (* [observe ~should_finalize:true] *)
          Gc.full_major ();
          stabilize_ [%here];
          let _o = observe (const 13) ~should_finalize:true in
          stabilize_ [%here];
          let n = num_active_observers t in
          Gc.full_major ();
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:(n - 1)
        ;;

        let%expect_test _ =
          (* [observe ~should_finalize:false] *)
          Gc.full_major ();
          stabilize_ [%here];
          let _o = observe (const 13) ~should_finalize:false in
          stabilize_ [%here];
          let n = num_active_observers t in
          Gc.full_major ();
          stabilize_ [%here];
          [%test_result: int] (num_active_observers t) ~expect:n
        ;;

        let keep_node_creation_backtrace = keep_node_creation_backtrace
        let set_keep_node_creation_backtrace = set_keep_node_creation_backtrace
        let num_nodes_became_necessary = num_nodes_became_necessary
        let num_nodes_became_unnecessary = num_nodes_became_unnecessary
        let num_nodes_changed = num_nodes_changed
        let num_nodes_created = num_nodes_created
        let num_nodes_invalidated = num_nodes_invalidated
        let num_nodes_recomputed = num_nodes_recomputed
        let num_stabilizes = num_stabilizes
        let num_var_sets = num_var_sets

        let num_nodes_recomputed_directly_because_min_height =
          num_nodes_recomputed_directly_because_min_height
        ;;

        let%expect_test _ =
          let var = Var.create 1 in
          let o =
            observe
              (map2
                 (map2 (Var.watch var) (const 1) ~f:( + ))
                 (map2 (const 2) (const 3) ~f:( + ))
                 ~f:( + ))
          in
          stabilize_ [%here];
          let stat1 = num_nodes_recomputed_directly_because_min_height t in
          Var.set var 2;
          stabilize_ [%here];
          let stat2 = num_nodes_recomputed_directly_because_min_height t in
          [%test_eq: int] (stat2 - stat1) 2;
          disallow_future_use o
        ;;

        let num_nodes_recomputed_directly_because_one_child =
          num_nodes_recomputed_directly_because_one_child
        ;;

        let%expect_test _ =
          (* We can't use the same variable twice otherwise the optimization is not
               applied. *)
          let var1 = Var.create 1 in
          let var2 = Var.create 1 in
          let o var = observe (map (map (Var.watch var) ~f:Fn.id) ~f:Fn.id) in
          let o1 = o var1 in
          let o2 = o var2 in
          stabilize_ [%here];
          let stat1 = num_nodes_recomputed_directly_because_one_child t in
          Var.set var1 2;
          Var.set var2 2;
          stabilize_ [%here];
          let stat2 = num_nodes_recomputed_directly_because_one_child t in
          [%test_result: int] (stat2 - stat1) ~expect:4;
          disallow_future_use o1;
          disallow_future_use o2
        ;;
      end

