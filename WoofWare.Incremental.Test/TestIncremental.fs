namespace WoofWare.Incremental.Test

open System
open System.Diagnostics
open System.Threading
open System.IO
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

        Observer.valueThrowing o |> shouldEqual i
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
        Observer.valueThrowing o |> shouldEqual 13
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
        Observer.valueThrowing i2 |> shouldEqual 0

        I.Var.Set x0 10
        I.Var.Set x1 11
        fix.Stabilize ()
        r |> shouldEqual 2
        Observer.valueThrowing i2 |> shouldEqual 10

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
                I.Sum None [| x ; y |] 0.0 (+) (-)
            )
            |> I.Observe

        fix.Stabilize ()
        Observer.valueThrowing nav |> shouldEqual ((0.5 * 50.0) + (0.5 * 20.0))

        I.Var.Set cfg (0.6, 0.4)
        fix.Stabilize ()
        Observer.valueThrowing nav |> shouldEqual ((0.6 * 50.0) + (0.4 * 20.0))

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

(*
      let test q list_f =
        for num_vars = 0 to 3 do
          let vars = List.init num_vars ~f:(fun _ -> Var.create_ [%here] true) in
          let q = observe (q (Array.of_list_map vars ~f:watch)) in
          let all = observe (all (List.map vars ~f:watch)) in
          let rec loop vars =
            match vars with
            | [] ->
              stabilize_ [%here];
              [%test_eq: Bool.t] (value q) (list_f (value all) ~f:Fn.id)
            | var :: vars ->
              List.iter [ false; true ] ~f:(fun b ->
                Var.set var b;
                loop vars)
          in
          loop vars
        done
      ;;

      let%expect_test _ = test exists List.exists
      let%expect_test _ = test for_all List.for_all
      let array_fold = array_fold

      let%expect_test _ =
        (* empty array *)
        let o = observe (array_fold [||] ~init:13 ~f:(fun _ -> assert false)) in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        let x = Var.create_ [%here] 13 in
        let y = Var.create_ [%here] 14 in
        let o =
          observe (array_fold [| watch y; watch x |] ~init:[] ~f:(fun ac x -> x :: ac))
        in
        let check where expect =
          stabilize_ where;
          [%test_result: int list] (value o) ~expect
        in
        check [%here] [ 13; 14 ];
        Var.set x 15;
        check [%here] [ 15; 14 ];
        Var.set y 16;
        check [%here] [ 15; 16 ];
        Var.set x 17;
        Var.set y 18;
        check [%here] [ 17; 18 ]
      ;;

      let reduce_balanced = reduce_balanced

      let reduce_balanced_exn a ~f ~reduce =
        Option.value_exn (reduce_balanced a ~f ~reduce)
      ;;

      let%expect_test _ =
        (* empty array *)
        let f =
          reduce_balanced
            [||]
            ~f:(fun _ -> assert false)
            ~reduce:(fun _ _ -> assert false)
        in
        assert (Option.is_none f)
      ;;

      let%expect_test _ =
        (* singular value *)
        let f =
          reduce_balanced_exn [| watch (Var.create_ [%here] 1) |] ~f:Fn.id ~reduce:( + )
        in
        let o = observe f in
        stabilize_ [%here];
        assert (value o = 1)
      ;;

      let%expect_test _ =
        (* non-commutative function test *)
        let list = [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ] in
        let list = List.map list ~f:(Var.create_ [%here]) in
        let array = Array.of_list_map ~f:Var.watch list in
        let reduce_calls = ref 0 in
        let f =
          reduce_balanced_exn array ~f:Fn.id ~reduce:(fun x y ->
            incr reduce_calls;
            x ^ y)
        in
        let o = observe f in
        stabilize_ [%here];
        [%test_eq: string] (value o) "abcdefg";
        [%test_eq: int] !reduce_calls 6;
        Var.set (List.hd_exn list) "z";
        stabilize_ [%here];
        [%test_eq: string] (value o) "zbcdefg";
        [%test_eq: int] !reduce_calls 9
      ;;

      let%expect_test _ =
        (* observability changes *)
        let observe_stabilize_disallow node =
          let o = observe node in
          stabilize_ [%here];
          let v = value o in
          disallow_future_use o;
          v
        in
        let v = Var.create_ [%here] 0 in
        let res = observe_stabilize_disallow (Var.watch v) in
        assert (res = 0);
        (* stabilize a reduce_balanced_exn node with already stabilized children *)
        let f = reduce_balanced_exn [| watch v |] ~f:Fn.id ~reduce:( + ) in
        let res = observe_stabilize_disallow f in
        assert (res = 0);
        (* re-stabilize a reduce_balanced_exn with a stale cache of its stabilized
             children. *)
        Var.set v 1;
        let res = observe_stabilize_disallow (Var.watch v) in
        assert (res = 1);
        let res = observe_stabilize_disallow f in
        assert (res = 1)
      ;;

      let%expect_test _ =
        (* multiple occurences of a node in the fold. *)
        let x = Var.create_ [%here] 1 in
        let f = reduce_balanced_exn [| watch x; watch x |] ~f:Fn.id ~reduce:( + ) in
        let o = observe f in
        let f2 =
          reduce_balanced_exn [| watch x; watch x; watch x |] ~f:Fn.id ~reduce:( + )
        in
        let o2 = observe f2 in
        stabilize_ [%here];
        assert (value o = 2);
        assert (value o2 = 3);
        Var.set x 3;
        stabilize_ [%here];
        assert (value o = 6);
        assert (value o2 = 9);
        disallow_future_use o;
        disallow_future_use o2;
        stabilize_ [%here];
        Var.set x 4;
        stabilize_ [%here];
        let o = observe f in
        let o2 = observe f2 in
        stabilize_ [%here];
        assert (value o = 8);
        assert (value o2 = 12)
      ;;

      let%expect_test _ =
        (* general creation and updating *)
        let module Test_value = struct
          type t =
            { var : int Var.t
            ; update1 : int option
            ; update2 : int option
            ; update3 : int option
            }

          let quickcheck_generator =
            let open Quickcheck.Generator.Let_syntax in
            let update_gen =
              let%bind weight = Float.gen_uniform_excl 0.0 3.0 in
              Quickcheck.Generator.weighted_union
                [ 1.0, Quickcheck.Generator.singleton None
                ; weight, Int.quickcheck_generator >>| Option.some
                ]
            in
            let%map var = Int.quickcheck_generator >>| Var.create_ [%here]
            and update1 = update_gen
            and update2 = update_gen
            and update3 = update_gen in
            { var; update1; update2; update3 }
          ;;
        end
        in
        Quickcheck.test
          (let open Quickcheck.Let_syntax in
           let%map test_value = List.gen_non_empty Test_value.quickcheck_generator in
           test_value)
          (* Trials limited because incremental tests can take time on the order of
               milliseconds each, due to the invariant checking. *)
          ~trials:100
          ~f:(fun test_values ->
            let array =
              Array.of_list_map test_values ~f:(fun test_value -> watch test_value.var)
            in
            let len = Array.length array in
            let reduce_count = ref 0 in
            let fold_count = ref 0 in
            let update_count = ref 0 in
            let assert_expected_reductions_and_reset () =
              if !update_count = 0
              then (
                assert (!fold_count = 0);
                assert (!reduce_count = 0))
              else (
                assert (!fold_count = len);
                assert (
                  !reduce_count <= Int.min (len - 1) (Int.ceil_log2 len * !update_count)));
              fold_count := 0;
              reduce_count := 0;
              update_count := 0
            in
            let reduce_f =
              reduce_balanced_exn array ~f:Fn.id ~reduce:(fun a b ->
                incr reduce_count;
                a * b)
            in
            let fold_f =
              array_fold array ~init:1 ~f:(fun a b ->
                incr fold_count;
                a * b)
            in
            update_count := len;
            let reduce_o = observe reduce_f in
            let fold_o = observe fold_f in
            stabilize_ [%here];
            assert (value fold_o = value reduce_o);
            assert_expected_reductions_and_reset ();
            List.iter test_values ~f:(fun test_value ->
              Option.iter test_value.update1 ~f:(fun a ->
                Var.set test_value.var a;
                incr update_count));
            stabilize_ [%here];
            assert (value fold_o = value reduce_o);
            assert_expected_reductions_and_reset ();
            List.iter test_values ~f:(fun test_value ->
              let updated = ref false in
              Option.iter test_value.update2 ~f:(fun a ->
                Var.set test_value.var a;
                updated := true);
              Option.iter test_value.update3 ~f:(fun a ->
                Var.set test_value.var a;
                updated := true);
              if !updated then incr update_count);
            stabilize_ [%here];
            assert (value fold_o = value reduce_o);
            assert_expected_reductions_and_reset ())
      ;;

      let get_cutoff = get_cutoff
      let set_cutoff = set_cutoff

      let%expect_test _ =
        let i = Var.watch (Var.create_ [%here] 0) in
        assert (Cutoff.equal (get_cutoff i) Cutoff.phys_equal);
        set_cutoff i Cutoff.never;
        assert (Cutoff.equal (get_cutoff i) Cutoff.never)
      ;;

      let%expect_test _ =
        let a = Var.create_ [%here] 0 in
        let n = map ~f:Fn.id (watch a) in
        set_cutoff
          n
          (Cutoff.create (fun ~old_value ~new_value -> abs (old_value - new_value) <= 1));
        let a' = observe n in
        stabilize_ [%here];
        assert (value a' = 0);
        List.iter
          [ 1, 0; 2, 2; 2, 2 ]
          ~f:(fun (v, expect) ->
            Var.set a v;
            stabilize_ [%here];
            assert (value a' = expect))
      ;;

      let lazy_from_fun = lazy_from_fun

      let%expect_test _ =
        (* laziness *)
        let r = ref 0 in
        let l = lazy_from_fun (fun () -> incr r) in
        assert (!r = 0);
        force l;
        assert (!r = 1);
        force l;
        assert (!r = 1)
      ;;

      let%expect_test _ =
        (* nodes created when forcing are in the right scope *)
        let l = lazy_from_fun (fun () -> const 13) in
        let x = Var.create_ [%here] 13 in
        let o = observe (bind (watch x) ~f:(fun _i -> force l)) in
        stabilize_ [%here];
        assert (value o = 13);
        Var.set x 14;
        stabilize_ [%here];
        assert (value o = 13)
      ;;

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

      let%expect_test _ =
        (* Deleting a parent from a child in such a way that it is replaced by a
             second parent, and the two parents have different child_indexes for the
             child. *)
        let c1 = const 12 in
        let c2 = const 12 in
        let o1 = observe (map2 c1 c2 ~f:( + )) in
        (* c2 is child 1, o1 is parent 0 *)
        stabilize_ [%here];
        let o2 = observe (map c2 ~f:Fn.id) in
        (* c2 is child 0, o2 is parent 1 *)
        stabilize_ [%here];
        Observer.disallow_future_use o1;
        (* o2 is parent 0, so c2 is child 1 for that index *)
        stabilize_ [%here];
        Observer.disallow_future_use o2;
        stabilize_ [%here]
      ;;

      let%expect_test _ =
        (* [bind_lhs_change_should_invalidate_rhs = false] *)
        if not M.bind_lhs_change_should_invalidate_rhs
        then (
          let va = Var.create 0 in
          let vb = Var.create 0 in
          let r = ref None in
          let o1 =
            observe
              (bind (watch va) ~f:(fun a ->
                 let t = map (watch vb) ~f:(fun b -> a + b) in
                 if a = 0 then r := Some t;
                 t))
          in
          stabilize_ [%here];
          let o2 = observe (Option.value_exn !r) in
          Var.set va 1;
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o2) ~expect:0;
          Var.set vb 1;
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o2) ~expect:1;
          Var.set vb 2;
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o2) ~expect:2;
          disallow_future_use o1;
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o2) ~expect:2;
          Var.set vb 3;
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o2) ~expect:3;
          disallow_future_use o2)
      ;;

    end :

*)
