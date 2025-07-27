namespace WoofWare.Incremental.Test

open System.Threading
open NUnit.Framework
open WoofWare.Incremental
open WoofWare.TimingWheel
open FsUnitTyped

[<TestFixture>]
module TestIncremental =
    let sec = TimeNs.Span.ofSec

    [<Test>]
    let ``test 1`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        State.invariant I.State
        let i = 13
        let t = I.Const i
        let o = I.Observe t
        fix.Stabilize ()

        Observer.value o |> shouldEqual i
        Observer.disallowFutureUse o

    [<Test>]
    let ``const is valid`` () =
        let I = Incremental.make ()
        I.Const 3 |> NodeHelpers.isValid |> shouldEqual true

    [<TestCase true>]
    [<TestCase false>]
    let ``const and return`` (useConst : bool) =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let i = if useConst then I.Const 13 else I.Return 13
        Node.isConst i |> shouldEqual true
        NodeHelpers.isNecessary i |> shouldEqual false

        let o = I.Observe i
        NodeHelpers.isNecessary i |> shouldEqual false

        fix.Stabilize ()

        NodeHelpers.isNecessary i |> shouldEqual true
        Observer.value o |> shouldEqual 13
        Node.isConst i |> shouldEqual true

    [<Test>]
    let ``not stabilizing by default`` () =
        let I = Incremental.make ()
        I.AmStabilizing |> shouldEqual false

    [<Test>]
    let ``am stabilizing within observation`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        let x = I.Var.Create 13

        let o =
            I.Var.Watch x
            |> I.Map (fun _ -> I.AmStabilizing |> shouldEqual true)
            |> I.Observe

        fix.Stabilize ()
        Observer.disallowFutureUse o

    [<Test>]
    let ``walking chains of maps is not allowed to cross scopes`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x0 = I.Var.Create 0
        let x1 = I.Var.Create 1
        let mutable r = 0

        let i2 =
            I.Var.Watch x0
            |> I.Bind (fun i ->
                I.Var.Watch x1
                |> I.Map (fun _ ->
                    Interlocked.Increment &r |> ignore<int>
                    i
                )
            )
            |> I.Observe

        r |> shouldEqual 0
        fix.Stabilize ()
        r |> shouldEqual 1
        Observer.value i2 |> shouldEqual 0

        I.Var.Set x0 10
        I.Var.Set x1 11
        fix.Stabilize ()
        r |> shouldEqual 2
        Observer.value i2 |> shouldEqual 10

    [<Test>]
    let ``nested var sets`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I
        // we model a simple ETF that initially consists of 100 shares of IBM and
        // 200 shares of Microsoft with an implicit divisor of 1

        // the last trade prices of two stocks
        let ibm = I.Var.Create 50.0
        let msft = I.Var.Create 20.0

        // .5 shares of IBM, .5 shares of MSFT, divisor implicitly 1
        let cfg = I.Var.Create (0.5, 0.5)

        let nav =
            I.Var.Watch cfg
            |> I.Bind (fun (ibmMult, msftMult) ->
                let x = I.Var.Watch ibm |> I.Map (fun ibm -> ibm * ibmMult)
                let y = I.Var.Watch msft |> I.Map (fun msft -> msft * msftMult)
                I.Sum None 0.0 (+) (-) [| x ; y |]
            )
            |> I.Observe

        fix.Stabilize ()
        Observer.value nav |> shouldEqual ((0.5 * 50.0) + (0.5 * 20.0))

        I.Var.Set cfg (0.6, 0.4)
        fix.Stabilize ()
        Observer.value nav |> shouldEqual ((0.6 * 50.0) + (0.4 * 20.0))

    [<Test>]
    let ``adjust heights`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 0

        let rec chain i =
            if i = 0 then
                I.Var.Watch x
            else
                chain (i - 1) |> I.Map (fun i -> i + 1)

        let b = I.Var.Watch x |> I.Bind chain

        let rec dag i =
            if i = 0 then
                b
            else
                let t = dag (i - 1)
                I.Map2 (+) t t

        let o = I.Observe (dag 20)

        for i = 1 to 10 do
            I.Var.Set x i
            fix.Stabilize ()

        Observer.disallowFutureUse o

    [<Test>]
    let ``test laziness`` () =
        let I = Incremental.make ()

        let mutable r = 0
        let l = I.LazyFromFun (fun () -> Interlocked.Increment &r |> ignore<int>)
        r |> shouldEqual 0
        l.Force ()
        r |> shouldEqual 1
        l.Force ()
        r |> shouldEqual 1

    [<Test>]
    let ``nodes created when forcing are in the right scope`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let l = I.LazyFromFun (fun () -> I.Const 13)
        let x = I.Var.Create 13
        let o = I.Var.Watch x |> I.Bind (fun _ -> l.Force ()) |> I.Observe

        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

        I.Var.Set x 14
        fix.Stabilize ()
        Observer.value o |> shouldEqual 13

    (*
      let default_hash_table_initial_size = default_hash_table_initial_size
      let memoize_fun = memoize_fun
      let memoize_fun_by_key = memoize_fun_by_key
      let weak_memoize_fun = weak_memoize_fun
      let weak_memoize_fun_by_key = weak_memoize_fun_by_key

      let test_memoize_fun (memoize_fun : int Base.Hashtbl.Key.t -> _ -> _) =
        let x = Var.create_ [%here] 13 in
        let y = Var.create_ [%here] 14 in
        let z = Var.create_ [%here] 15 in
        let num_calls = ref 0 in
        let o =
          observe
            (bind
               (bind (watch x) ~f:(fun i1 ->
                  let f i2 =
                    incr num_calls;
                    map (watch y) ~f:(fun i3 -> i1 + i2 + i3)
                  in
                  return (unstage (memoize_fun (module Int) f))))
               ~f:(fun f -> bind (watch z) ~f))
        in
        stabilize_ [%here];
        [%test_eq: int] (value o) 42;
        assert (!num_calls = 1);
        Var.set z 16;
        stabilize_ [%here];
        [%test_eq: int] (value o) 43;
        assert (!num_calls = 2);
        Var.set z 17;
        stabilize_ [%here];
        assert (!num_calls = 3);
        [%test_eq: int] (value o) 44;
        Var.set z 16;
        stabilize_ [%here];
        assert (!num_calls = 3);
        [%test_eq: int] (value o) 43;
        Var.set y 20;
        stabilize_ [%here];
        assert (!num_calls = 3);
        [%test_eq: int] (value o) 49;
        Var.set x 30;
        stabilize_ [%here];
        assert (!num_calls = 4);
        [%test_eq: int] (value o) 66
      ;;

      let%expect_test _ = test_memoize_fun memoize_fun

      let%expect_test _ =
        test_memoize_fun (fun hashable f -> memoize_fun_by_key hashable Fn.id f)
      ;;

      let%expect_test _ =
        test_memoize_fun (fun hashable f ->
          let memo_f =
            unstage (weak_memoize_fun hashable (fun a -> Heap_block.create_exn (f a)))
          in
          stage (fun a -> Heap_block.value (memo_f a)))
      ;;

      let%expect_test _ =
        test_memoize_fun (fun hashable f ->
          let memo_f =
            unstage
              (weak_memoize_fun_by_key hashable Fn.id (fun a ->
                 Heap_block.create_exn (f a)))
          in
          stage (fun a -> Heap_block.value (memo_f a)))
      ;;

      let node_value = node_value

      let%expect_test _ =
        let show t = print_s [%sexp (node_value t : int Node_value.t)] in
        if M.bind_lhs_change_should_invalidate_rhs
        then (
          show invalid;
          [%expect {| Invalid |}]);
        let x = Var.create 13 in
        let t = watch x in
        show t;
        [%expect {| (Unnecessary_maybe_stale ()) |}];
        let o = observe t in
        show t;
        [%expect {| (Unnecessary_maybe_stale ()) |}];
        stabilize_ [%here];
        show t;
        [%expect {| (Necessary_maybe_stale (13)) |}];
        Observer.disallow_future_use o;
        stabilize_ [%here];
        show t;
        [%expect {| (Unnecessary_maybe_stale (13)) |}]
      ;;

      let%expect_test _ =
        (* removal of unused data *)
        let num_calls = ref 0 in
        let f =
          unstage
            (weak_memoize_fun
               (module Int)
               (function
                 | i ->
                   incr num_calls;
                   Heap_block.create_exn (ref i)))
        in
        let f i = Heap_block.value (f i) in
        let x0 = f 13 in
        let x0' = f 13 in
        let x1 = f 15 in
        assert (!num_calls = 2);
        assert (!x0 + !x0' + !x1 = 41);
        Gc.full_major ();
        assert (!num_calls = 2);
        let _x0 = f 13 in
        assert (!num_calls = 3);
        assert (phys_equal (f 15) x1);
        assert (!num_calls = 3);
        Gc.keep_alive x1
      ;;

      let%expect_test _ =
        (* removing a parent is constant time *)
        (* We can't run this test with debugging, because it's too slow. *)
        if not debug
        then
          for e = 0 to 5 do
            let num_observers = Float.to_int (10. ** Float.of_int e) in
            let t = const 13 in
            let observers =
              List.init num_observers ~f:(fun _ -> observe (map t ~f:Fn.id))
            in
            let cpu_used () =
              let module R = Unix.Resource_usage in
              let { R.utime; stime; _ } = R.get `Self in
              Time_ns.Span.of_sec (utime +. stime)
            in
            let before = cpu_used () in
            (* Don't use [stabilize_], which runs the invariant, which is too slow
                 here. *)
            stabilize ();
            List.iter observers ~f:disallow_future_use;
            stabilize ();
            let consumed = Time_ns.Span.( - ) (cpu_used ()) before in
            assert (Time_ns.Span.( < ) consumed (sec 1.))
          done
      ;;

*)

    [<Test>]
    let ``replacing a parent with a second parent that has different child_index`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let c1 = I.Const 12
        let c2 = I.Const 12
        let o1 = I.Observe (I.Map2 (+) c1 c2)
        // c2 is child 1, o1 is parent 0
        fix.Stabilize ()

        let o2 = I.Observe (I.Map id c2)
        // c2 is child 0, o2 is parent 1
        fix.Stabilize ()

        Observer.disallowFutureUse o1
        // o2 is parent 0, so c2 is child 1 for that index
        fix.Stabilize ()
        Observer.disallowFutureUse o2
        fix.Stabilize ()
