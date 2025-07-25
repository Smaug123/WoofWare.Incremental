namespace WoofWare.Incremental.Test

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

      Var.toString t
      |> shouldEqual (string<int> i)
      Observer.disallowFutureUse o

    let isInvalid (fix : IncrementalFixture) (t : Node<'a>) =
        let o = fix.I.Observe t
        fix.Stabilize ()
        let result = Observer.value o |> Result.isError
        Observer.disallowFutureUse o

        result

    let isInvalidatedOnBindRhs (fix : IncrementalFixture) (f : int -> Node<'a>) : bool =
        let x = fix.I.Var.Create 13
        let r = ref None
        let o1 =
            fix.I.Var.Watch x
            |> fix.I.Bind (fun i ->
                r.Value <- Some (f i)
                fix.I.Return ()
            )
            |> fix.I.Observe
        fix.Stabilize ()
        let t = r.Value.Value
        let o2 = fix.I.Observe t
        fix.I.Var.Set x 14
        fix.Stabilize ()
        let result = isInvalid fix t
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2
        result

    [<Test>]
    let ``invalid is invalid`` () =
      let fix = IncrementalFixture.Make ()
      isInvalid fix fix.Invalid
      |> shouldEqual true

      fix.Invalid
      |> NodeHelpers.isValid
      |> shouldEqual false

    [<Test>]
    let ``const is valid`` () =
      let I = Incremental.make ()
      I.Const 3
      |> NodeHelpers.isValid
      |> shouldEqual true

    [<TestCase true>]
    [<TestCase false>]
    let ``const and return`` (useConst : bool) =
      let fix = IncrementalFixture.Make ()
      let I = fix.I

      let i =
          if useConst then I.Const 13 else I.Return 13
      Node.isConst i |> shouldEqual true
      NodeHelpers.isNecessary i |> shouldEqual false

      let o = I.Observe i
      NodeHelpers.isNecessary i |> shouldEqual false

      fix.Stabilize ()

      NodeHelpers.isNecessary i |> shouldEqual true
      Observer.valueThrowing o |> shouldEqual 13
      Node.isConst i |> shouldEqual true

    [<Test>]
    let ``Test isInvalidatedOnBindRhs`` () =
        let fix = IncrementalFixture.Make ()
        isInvalidatedOnBindRhs fix (fun _ -> fix.I.Const 13)
        |> shouldEqual true


    [<Test>]
    let ``not stabilizing by default`` () =
      let I = Incremental.make ()
      I.AmStabilizing |> shouldEqual false

      let%expect_test _ =
        let x = Var.create_ [%here] 13 in
        let o = observe (map (watch x) ~f:(fun _ -> assert (am_stabilizing ()))) in
        stabilize_ [%here];
        disallow_future_use o
      ;;

      let test_map n (mapN : int t -> int t) =
        let o = observe (mapN (const 1)) in
        stabilize_ [%here];
        assert (value o = n);
        let x = Var.create_ [%here] 1 in
        let o = observe (mapN (watch x)) in
        stabilize_ [%here];
        assert (value o = n);
        Var.set x 0;
        stabilize_ [%here];
        assert (value o = 0);
        Var.set x 2;
        stabilize_ [%here];
        assert (value o = 2 * n);
        assert (is_invalid (mapN invalid));
        assert (is_invalidated_on_bind_rhs (fun i -> mapN (const i)))
      ;;

      let%expect_test _ = test_map 1 (fun i -> i >>| fun a1 -> a1)
      let%expect_test _ = test_map 1 (fun i -> map i ~f:(fun a1 -> a1))
      let%expect_test _ = test_map 2 (fun i -> map2 i i ~f:(fun a1 a2 -> a1 + a2))

      let%expect_test _ =
        let x0 = Var.create_ [%here] 13 in
        let o0 = observe (watch x0) in
        let t1 = map (watch x0) ~f:(fun x -> x + 1) in
        let t1_o = observe t1 in
        stabilize_ [%here];
        assert (value t1_o = value o0 + 1);
        Var.set x0 14;
        stabilize_ [%here];
        assert (value t1_o = value o0 + 1);
        let x1 = Var.create_ [%here] 15 in
        let o1 = observe (watch x1) in
        let t2 = map2 (watch x0) (watch x1) ~f:(fun x y -> x + y) in
        let t2_o = observe t2 in
        let t3 = map2 t1 t2 ~f:(fun x y -> x - y) in
        let t3_o = observe t3 in
        let check () =
          stabilize_ [%here];
          assert (value t1_o = value o0 + 1);
          assert (value t2_o = value o0 + value o1);
          assert (value t3_o = value t1_o - value t2_o)
        in
        check ();
        Var.set x0 16;
        check ();
        Var.set x1 17;
        check ();
        Var.set x0 18;
        Var.set x1 19;
        check ()
      ;;

      let%expect_test _ =
        (* deep *)
        let rec loop i t =
          if i = 0 then t else loop (i - 1) (map t ~f:(fun x -> x + 1))
        in
        let x0 = Var.create_ [%here] 0 in
        let n = 100 in
        let o = observe (loop n (watch x0)) in
        stabilize_ [%here];
        assert (value o = n);
        Var.set x0 1;
        stabilize_ [%here];
        assert (value o = n + 1);
        disallow_future_use o;
        stabilize_ [%here]
      ;;

      let bind = bind
      let ( >>= ) = ( >>= )

      let%expect_test _ =
        (* [bind] of a constant *)
        stabilize_ [%here];
        let o = observe (const 13 >>= const) in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%test _ =
        is_invalidated_on_bind_rhs (fun i -> bind (const i) ~f:(fun _ -> const i))
      ;;

      let%expect_test _ =
        (* bind created with an invalid rhs *)
        let o = observe (const () >>= fun () -> invalid) in
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* bind created with an rhs that becomes invalid *)
        let b = Var.create true in
        let o = observe (Var.watch b >>= fun b -> if b then const 13 else invalid) in
        stabilize_ [%here];
        Var.set b false;
        assert (is_valid (Observer.observing o));
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* an invalid node created on the rhs of a valid bind, later invalidated *)
        let x = Var.create_ [%here] 13 in
        let r = ref None in
        let o1 =
          observe
            (bind (Var.watch x) ~f:(fun _ ->
               r := Some (map invalid ~f:Fn.id);
               return ()))
        in
        stabilize_ [%here];
        let o2 = observe (Option.value_exn !r) in
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o2)));
        Var.set x 14;
        stabilize_ [%here];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* invariants blow up here if we don't make sure that we first make the
             lhs-change node of binds necessary and only then the rhs necessary. *)
        let node1 = const () >>= return in
        let o = observe node1 in
        stabilize_ [%here];
        disallow_future_use o;
        stabilize_ [%here];
        let o = observe node1 in
        stabilize_ [%here];
        disallow_future_use o
      ;;

      let%expect_test _ =
        let v1 = Var.create_ [%here] 0 in
        let i1 = Var.watch v1 in
        let i2 = i1 >>| fun x -> x + 1 in
        let i3 = i1 >>| fun x -> x + 2 in
        let i4 = i2 >>= fun x1 -> i3 >>= fun x2 -> const (x1 + x2) in
        let o4 = observe i4 in
        List.iter (List.init 20 ~f:Fn.id) ~f:(fun x ->
          Gc.full_major ();
          Var.set v1 x;
          stabilize_ [%here];
          assert (Observer.value_exn o4 = (2 * x) + 3))
      ;;

      let%expect_test _ =
        (* graph changes only *)
        let x = Var.create_ [%here] true in
        let a = const 3 in
        let b = const 4 in
        let o = observe (bind (watch x) ~f:(fun bool -> if bool then a else b)) in
        let check where expect =
          stabilize_ where;
          [%test_eq: int] (value o) expect
        in
        check [%here] 3;
        Var.set x false;
        check [%here] 4;
        Var.set x true;
        check [%here] 3
      ;;

      let%expect_test _ =
        let x0 = Var.create_ [%here] 13 in
        let o0 = observe (watch x0) in
        let x1 = Var.create_ [%here] 15 in
        let o1 = observe (watch x1) in
        let x2 = Var.create_ [%here] true in
        let o2 = observe (watch x2) in
        let t = bind (watch x2) ~f:(fun b -> if b then watch x0 else watch x1) in
        let t_o = observe t in
        let check () =
          stabilize_ [%here];
          assert (value t_o = value (if value o2 then o0 else o1))
        in
        check ();
        Var.set x0 17;
        check ();
        Var.set x1 19;
        check ();
        Var.set x2 false;
        check ();
        Var.set x0 21;
        Var.set x2 true;
        check ()
      ;;

      let%expect_test _ =
        (* walking chains of maps is not allowed to cross scopes *)
        let x0 = Var.create_ [%here] 0 in
        let x1 = Var.create_ [%here] 1 in
        let r = ref 0 in
        let i2 =
          observe
            (Var.watch x0
             >>= fun i ->
             Var.watch x1
             >>| fun _ ->
             incr r;
             i)
        in
        assert (!r = 0);
        stabilize_ [%here];
        assert (!r = 1);
        assert (value i2 = 0);
        Var.set x0 10;
        Var.set x1 11;
        stabilize_ [%here];
        assert (!r = 2);
        assert (value i2 = 10)
      ;;

      let%expect_test _ =
        let v1 = Var.create_ [%here] 0 in
        let i1 = Var.watch v1 in
        let o1 = observe i1 in
        Var.set v1 1;
        let i2 = i1 >>= fun _ -> i1 in
        let o2 = observe i2 in
        stabilize_ [%here];
        Var.set v1 2;
        stabilize_ [%here];
        Gc.keep_alive (i1, i2, o1, o2)
      ;;

      let%expect_test _ =
        (* topological overload many *)
        let rec copy_true c1 = bind c1 ~f:(fun x -> if x then c1 else copy_false c1)
        and copy_false c1 = bind c1 ~f:(fun x -> if x then copy_true c1 else c1) in
        let x1 = Var.create_ [%here] false in
        let rec loop cur i =
          if i > 1000 then cur else loop (copy_true (copy_false cur)) (i + 1)
        in
        let hold = loop (Var.watch x1) 0 in
        let rec set_loop at i =
          if i < 5
          then (
            Var.set x1 at;
            stabilize_ [%here];
            set_loop (not at) (i + 1))
        in
        set_loop true 0;
        Gc.keep_alive hold
      ;;

      let%expect_test _ =
        (* nested var sets *)
        (* We model a simple ETF that initially consists of 100 shares of IBM and
             200 shares of microsoft with an implicit divisor of 1. *)
        (* the last trade prices of two stocks *)
        let ibm = Var.create_ [%here] 50. in
        let msft = Var.create_ [%here] 20. in
        (* .5 shares of IBM, .5 shares of MSFT.  Divisor implicitly 1. *)
        let cfg = Var.create_ [%here] (0.5, 0.5) in
        let nav =
          observe
            (bind (Var.watch cfg) ~f:(fun (ibm_mult, msft_mult) ->
               let x = map (Var.watch ibm) ~f:(fun ibm -> ibm *. ibm_mult) in
               let y = map (Var.watch msft) ~f:(fun msft -> msft *. msft_mult) in
               sum [| x; y |] ~zero:0. ~add:( +. ) ~sub:( -. )))
        in
        stabilize_ [%here];
        assert (value nav =. (0.5 *. 50.) +. (0.5 *. 20.));
        Var.set cfg (0.6, 0.4);
        stabilize_ [%here];
        assert (value nav =. (0.6 *. 50.) +. (0.4 *. 20.))
      ;;

      let%expect_test _ =
        (* adjust heights *)
        let x = Var.create_ [%here] 0 in
        let rec chain i = if i = 0 then watch x else chain (i - 1) >>| fun i -> i + 1 in
        let b = bind (watch x) ~f:chain in
        let rec dag i =
          if i = 0
          then b
          else (
            let t = dag (i - 1) in
            map2 t t ~f:( + ))
        in
        let o = observe (dag 20) in
        for i = 1 to 10 do
          Var.set x i;
          stabilize_ [%here]
        done;
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* an invalid unused rhs doesn't invalidate the [bind] *)
        let r = ref None in
        let lhs = Var.create_ [%here] 1 in
        let o1 =
          observe
            (bind (watch lhs) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let else_ = Option.value_exn !r in
        let test = Var.create_ [%here] false in
        let o2 =
          observe
            (bind
               (make_high (watch test))
               ~f:(fun test -> if test then const 13 else else_))
        in
        stabilize_ [%here];
        Var.set lhs 2;
        (* invalidates [else_]. *)
        Var.set test true;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid else_));
        assert (value o2 = 13);
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* plugging an invalid node in a bind can invalidate the bind (though
             not always) *)
        let x = Var.create 4 in
        let r = ref (const (-1)) in
        let o =
          observe
            (Var.watch x
             >>= fun i ->
             r := const i;
             const ())
        in
        stabilize_ [%here];
        let escaped = !r in
        let escaped_o = observe escaped in
        stabilize_ [%here];
        assert (Observer.value_exn escaped_o = 4);
        Var.set x 5;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid escaped));
        disallow_future_use o;
        let o = observe (Var.watch x >>= fun _ -> escaped) in
        stabilize_ [%here];
        disallow_future_use o;
        disallow_future_use escaped_o
      ;;

      let%expect_test _ =
        (* changing the rhs from a node to its ancestor, which causes problems if
             we leave the node with a broken invariant while adding the ancestor. *)
        let lhs_var = Var.create false in
        let num_calls = ref 0 in
        let rhs_var = Var.create 13 in
        let rhs_false =
          map (watch rhs_var) ~f:(fun i ->
            incr num_calls;
            i + 1)
        in
        let rhs_true = map rhs_false ~f:(fun i -> i + 1) in
        let o =
          observe (bind (watch lhs_var) ~f:(fun b -> if b then rhs_true else rhs_false))
        in
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1;
        Var.set lhs_var true;
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1;
        disallow_future_use o;
        stabilize_ [%here];
        Var.set rhs_var 14;
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1
      ;;

      let bind2 = bind2
      let bind3 = bind3
      let bind4 = bind4

      let%expect_test _ =
        let v1 = Var.create_ [%here] 1 in
        let v2 = Var.create_ [%here] 2 in
        let v3 = Var.create_ [%here] 3 in
        let v4 = Var.create_ [%here] 4 in
        let o =
          observe
            (bind4 (watch v1) (watch v2) (watch v3) (watch v4) ~f:(fun x1 x2 x3 x4 ->
               bind3 (watch v2) (watch v3) (watch v4) ~f:(fun y2 y3 y4 ->
                 bind2 (watch v3) (watch v4) ~f:(fun z3 z4 ->
                   bind (watch v4) ~f:(fun w4 ->
                     return (x1 + x2 + x3 + x4 + y2 + y3 + y4 + z3 + z4 + w4))))))
        in
        let check where =
          stabilize_ where;
          [%test_result: int]
            (value o)
            ~expect:
              (Var.value v1 + (2 * Var.value v2) + (3 * Var.value v3) + (4 * Var.value v4))
        in
        check [%here];
        Var.set v4 5;
        check [%here];
        Var.set v3 6;
        check [%here];
        Var.set v2 7;
        check [%here];
        Var.set v1 8;
        check [%here];
        Var.set v1 9;
        Var.set v2 10;
        Var.set v3 11;
        Var.set v4 12;
        check [%here]
      ;;

      let if_ = if_

      let%expect_test _ =
        (* [if_ true] *)
        let o = observe (if_ (const true) ~then_:(const 13) ~else_:(const 14)) in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        (* [if_ false] *)
        let o = observe (if_ (const false) ~then_:(const 13) ~else_:(const 14)) in
        stabilize_ [%here];
        assert (value o = 14)
      ;;

      let%expect_test _ =
        (* graph changes only *)
        let x = Var.create_ [%here] true in
        let o = observe (if_ (watch x) ~then_:(const 3) ~else_:(const 4)) in
        let check where expect =
          stabilize_ where;
          [%test_eq: int] (value o) expect
        in
        check [%here] 3;
        Var.set x false;
        check [%here] 4;
        Var.set x true;
        check [%here] 3;
        Var.set x false;
        check [%here] 4
      ;;

      let%expect_test _ =
        let test = Var.create_ [%here] true in
        let then_ = Var.create_ [%here] 1 in
        let else_ = Var.create_ [%here] 2 in
        let num_then_run = ref 0 in
        let num_else_run = ref 0 in
        let ite =
          observe
            (if_
               (Var.watch test)
               ~then_:
                 (Var.watch then_
                  >>| fun i ->
                  incr num_then_run;
                  i)
               ~else_:
                 (Var.watch else_
                  >>| fun i ->
                  incr num_else_run;
                  i))
        in
        stabilize_ [%here];
        assert (Observer.value_exn ite = 1);
        assert (!num_then_run = 1);
        assert (!num_else_run = 0);
        Var.set test false;
        stabilize_ [%here];
        assert (Observer.value_exn ite = 2);
        Var.set test true;
        stabilize_ [%here];
        assert (Observer.value_exn ite = 1);
        Var.set then_ 3;
        Var.set else_ 4;
        let ntr = !num_then_run in
        let ner = !num_else_run in
        stabilize_ [%here];
        assert (Observer.value_exn ite = 3);
        assert (!num_then_run = ntr + 1);
        assert (!num_else_run = ner);
        Var.set test false;
        Var.set then_ 5;
        Var.set else_ 6;
        stabilize_ [%here];
        assert (Observer.value_exn ite = 6)
      ;;

      let%expect_test _ =
        (* an invalid unused branch doesn't invalidate the [if_] *)
        let r = ref None in
        let lhs = Var.create_ [%here] 1 in
        let o1 =
          observe
            (bind (watch lhs) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let else_ = Option.value_exn !r in
        let test = Var.create_ [%here] false in
        let o2 = observe (if_ (make_high (watch test)) ~then_:(const 13) ~else_) in
        stabilize_ [%here];
        Var.set lhs 2;
        (* invalidates [else_]. *)
        Var.set test true;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid else_));
        assert (value o2 = 13);
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* if-then-else created with an invalid test *)
        let o =
          observe (if_ (invalid >>| fun _ -> true) ~then_:(const ()) ~else_:(const ()))
        in
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* if-then-else created with an invalid branch *)
        let o = observe (if_ (const true) ~then_:invalid ~else_:(const 13)) in
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* if-then-else switching to an invalid branch *)
        let b = Var.create false in
        let o = observe (if_ (Var.watch b) ~then_:invalid ~else_:(const 13)) in
        stabilize_ [%here];
        assert (is_valid (Observer.observing o));
        Var.set b true;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* if-then-else switching to an invalid branch via a map *)
        let b = Var.create false in
        let o =
          observe (if_ (Var.watch b) ~then_:(invalid >>| fun _ -> 13) ~else_:(const 13))
        in
        stabilize_ [%here];
        assert (is_valid (Observer.observing o));
        Var.set b true;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* if-then-else switching to an invalid test *)
        let b = Var.create false in
        let o =
          observe
            (if_
               (if_ (Var.watch b) ~then_:(invalid >>| fun _ -> true) ~else_:(const true))
               ~then_:(const 13)
               ~else_:(const 15))
        in
        stabilize_ [%here];
        assert (is_valid (Observer.observing o));
        Var.set b true;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid (Observer.observing o)))
      ;;

      let%expect_test _ =
        (* changing branches from a node to its ancestor, which causes problems if
             we leave the node with a broken invariant while adding the ancestor. *)
        let test_var = Var.create false in
        let num_calls = ref 0 in
        let branch_var = Var.create 13 in
        let else_ =
          map (watch branch_var) ~f:(fun i ->
            incr num_calls;
            i + 1)
        in
        let then_ = map else_ ~f:(fun i -> i + 1) in
        let o = observe (if_ (watch test_var) ~then_ ~else_) in
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1;
        Var.set test_var true;
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1;
        disallow_future_use o;
        stabilize_ [%here];
        Var.set branch_var 14;
        stabilize_ [%here];
        [%test_result: int] !num_calls ~expect:1
      ;;

      let freeze = freeze

      let%expect_test _ =
        let x = Var.create_ [%here] 13 in
        let f = freeze (Var.watch x) in
        let y = observe f in
        assert (not (is_const f));
        stabilize_ [%here];
        assert (value y = 13);
        assert (is_const f);
        let u = Var.create_ [%here] 1 in
        let z = observe (bind (Var.watch u) ~f:(fun _ -> freeze (Var.watch x))) in
        stabilize_ [%here];
        assert (value z = 13);
        Var.set u 2;
        Var.set x 14;
        stabilize_ [%here];
        assert (value z = 14);
        Var.set x 15;
        stabilize_ [%here];
        assert (value z = 14);
        Var.set u 3;
        stabilize_ [%here];
        assert (value z = 15)
      ;;

      let%expect_test _ =
        let x = Var.create_ [%here] 13 in
        let o1 = observe (freeze (Var.watch x >>| Fn.id)) in
        let o2 = observe (Var.watch x >>| Fn.id) in
        stabilize_ [%here];
        assert (value o1 = 13);
        assert (value o2 = 13);
        stabilize_ [%here];
        assert (value o1 = 13);
        assert (value o2 = 13);
        Var.set x 14;
        stabilize_ [%here];
        assert (value o1 = 13);
        assert (value o2 = 14)
      ;;

      let%expect_test _ =
        (* [freeze] nodes increment [num_nodes_became_necessary] *)
        let i1 = State.(num_nodes_became_necessary t) in
        ignore (freeze (const ()) : unit t);
        let i2 = State.(num_nodes_became_necessary t) in
        [%test_result: int] i2 ~expect:(i1 + 2)
      ;;

      (* one for the [const], one for the [freeze] *)

      (* TEST_UNIT = (\* freeze nodes leak memory (and forces spurious computations) until
         *                they freeze *\)
         *   let c = const () in
         *   for i = 0 to 100_000_000 do
         *     ignore (freeze c ~when_:(fun () -> false) : unit t);
         *     if i mod 1000 = 0 then begin
         *       Printf.printf "num parents %d\n%!" ((Obj.magic c : int array).(7));
         *       stabilize_ [%here];
         *     end
         *   done;
         * ;; *)

      let%expect_test _ =
        (* [freeze]ing something that is otherwise unnecessary. *)
        let x = Var.create_ [%here] 0 in
        let i = freeze (Var.watch x >>| fun i -> i + 1) in
        stabilize_ [%here];
        Var.set x 13;
        let o = observe i in
        stabilize_ [%here];
        assert (value o = 1 (* not 14 *))
      ;;

      let%expect_test _ =
        (* a frozen node remains valid, even if its original scope isn't *)
        let x = Var.create_ [%here] 13 in
        let r = ref None in
        let o1 =
          observe
            (watch x
             >>= fun i ->
             if Option.is_none !r then r := Some (freeze (const i));
             const ())
        in
        stabilize_ [%here];
        let f = Option.value_exn !r in
        Var.set x 15;
        stabilize_ [%here];
        let o2 = observe f in
        stabilize_ [%here];
        assert (is_const f);
        assert (value o2 = 13);
        disallow_future_use o1;
        stabilize_ [%here]
      ;;

      let%expect_test _ =
        (* a frozen node remains valid, even if the node it froze isn't *)
        let x = Var.create_ [%here] 13 in
        let r = ref (const 14) in
        let o1 =
          observe
            (watch x
             >>= fun i ->
             r := const i;
             const ())
        in
        stabilize_ [%here];
        let o2 = observe (freeze !r) in
        stabilize_ [%here];
        Var.set x 15;
        stabilize_ [%here];
        assert (value o2 = 13);
        disallow_future_use o1
      ;;

      let%expect_test _ =
        (* [freeze ~when] *)
        let x = Var.create_ [%here] 13 in
        let o = observe (freeze (watch x) ~when_:(fun i -> i >= 15)) in
        let check where expect =
          stabilize_ where;
          [%test_result: int] (value o) ~expect
        in
        check [%here] 13;
        Var.set x 14;
        check [%here] 14;
        Var.set x 15;
        check [%here] 15;
        Var.set x 16;
        check [%here] 15;
        Var.set x 14;
        check [%here] 15
      ;;

      let%expect_test _ =
        (* a freeze that is invalidated before it is frozen. *)
        let r = ref None in
        let x = Var.create_ [%here] 13 in
        let o =
          observe
            (bind (watch x) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let f = freeze (Option.value_exn !r) ~when_:(fun _ -> false) in
        Var.set x 14;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid f));
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* a freeze that is stabilized and invalidated before it is frozen. *)
        let r = ref None in
        let x = Var.create_ [%here] 13 in
        let o =
          observe
            (bind (watch x) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let f = freeze (Option.value_exn !r) ~when_:(fun _ -> false) in
        stabilize_ [%here];
        Var.set x 14;
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid f));
        disallow_future_use o
      ;;

      let depend_on = depend_on

      let%expect_test _ =
        let x = Var.create_ [%here] 13 in
        let y = Var.create_ [%here] 14 in
        let d = depend_on (watch x) ~depend_on:(watch y) in
        let o = observe d in
        let nx = ref 0 in
        let incr_o r = function
          | Observer.Update.Invalidated -> assert false
          | Initialized _ | Changed _ -> incr r
        in
        let incr r = function
          | Update.Invalidated -> assert false
          | Unnecessary -> ()
          | Necessary _ | Changed _ -> incr r
        in
        Observer.on_update_exn o ~f:(incr_o nx);
        let ny = ref 0 in
        on_update (Var.watch y) ~f:(incr ny);
        let check where eo enx eny =
          stabilize_ where;
          [%test_result: int] (value o) ~expect:eo;
          [%test_result: int] !nx ~expect:enx;
          [%test_result: int] !ny ~expect:eny
        in
        check [%here] 13 1 1;
        Var.set x 15;
        check [%here] 15 2 1;
        Var.set y 16;
        check [%here] 15 2 2;
        Var.set x 17;
        Var.set y 18;
        check [%here] 17 3 3;
        Var.set x 17;
        check [%here] 17 3 3;
        Var.set y 18;
        check [%here] 17 3 3;
        disallow_future_use o;
        let check where enx eny =
          stabilize_ where;
          [%test_result: int] !nx ~expect:enx;
          [%test_result: int] !ny ~expect:eny
        in
        Var.set x 19;
        Var.set y 20;
        check [%here] 3 3;
        let o = observe d in
        Observer.on_update_exn o ~f:(incr_o nx);
        check [%here] 4 4;
        [%test_result: int] (value o) ~expect:19
      ;;

      let%expect_test _ =
        (* propagating the first argument of [depend_on] while the result of
             [depend_on] is not observable *)
        let var = Var.create 1 in
        let depend = depend_on (Var.watch var) ~depend_on:(const ()) in
        let o = observe depend in
        stabilize_ [%here];
        assert (Observer.value_exn o = 1);
        disallow_future_use o;
        let o = observe (Var.watch var) in
        Var.set var 2;
        stabilize_ [%here];
        assert (Observer.value_exn o = 2);
        disallow_future_use o;
        let o = observe depend in
        stabilize_ [%here];
        [%test_eq: int] (Observer.value_exn o) 2
      ;;

      let%expect_test _ =
        (* depend_on doesn't cutoff using phys_equal *)
        let v1 = Var.create () in
        let v2 = Var.create 1 in
        set_cutoff (Var.watch v1) Cutoff.never;
        let o = observe (depend_on (Var.watch v1) ~depend_on:(Var.watch v2)) in
        let updates = ref 0 in
        Observer.on_update_exn o ~f:(fun _ -> incr updates);
        [%test_eq: int] !updates 0;
        stabilize_ [%here];
        [%test_eq: int] !updates 1;
        Var.set v2 2;
        stabilize_ [%here];
        [%test_eq: int] !updates 1;
        Var.set v1 ();
        stabilize_ [%here];
        [%test_eq: int] !updates 2;
        disallow_future_use o
      ;;

      let necessary_if_alive = necessary_if_alive

      let%expect_test _ =
        (* dead => unnecessary *)
        let x = Var.create 13 in
        let push, check = on_update_queue () in
        on_update (watch x) ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        let t = necessary_if_alive (watch x) in
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set x 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        Gc.keep_alive t;
        Gc.full_major ();
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* cutoff is preserved *)
        let x = Var.create 13 in
        set_cutoff (watch x) Cutoff.never;
        let t = necessary_if_alive (watch x) in
        let o = observe t in
        let push, check = on_update_queue () in
        on_update t ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set x 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        Var.set x 14;
        stabilize_ [%here];
        check [ Changed (14, 14) ];
        disallow_future_use o;
        Gc.full_major ();
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let all = all
      let both = both
      let exists = exists
      let for_all = for_all

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

      module Unordered_array_fold_update = Unordered_array_fold_update

      let unordered_array_fold = unordered_array_fold

      let%expect_test _ =
        (* empty array *)
        let o =
          observe
            (unordered_array_fold
               ~full_compute_every_n_changes:0
               [||]
               ~init:13
               ~f:(fun _ -> assert false)
               ~update:(F_inverse (fun _ -> assert false)))
        in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        (* an unnecessary [unordered_array_fold] isn't computed. *)
        let x = Var.create_ [%here] 1 in
        let num_f_inverse = ref 0 in
        let ox = observe (Var.watch x) in
        let fold =
          unordered_array_fold
            [| Var.watch x |]
            ~init:0
            ~f:( + )
            ~update:
              (F_inverse
                 (fun b a ->
                   incr num_f_inverse;
                   b - a))
        in
        let r = observe fold in
        stabilize_ [%here];
        assert (value r = 1);
        assert (!num_f_inverse = 0);
        Var.set x 2;
        stabilize_ [%here];
        assert (value r = 2);
        assert (!num_f_inverse = 1);
        disallow_future_use r;
        Var.set x 3;
        stabilize_ [%here];
        assert (!num_f_inverse = 1);
        assert (value ox = 3);
        let r = observe fold in
        stabilize_ [%here];
        [%test_result: int] (value r) ~expect:3;
        assert (!num_f_inverse = 1)
      ;;

      let%expect_test _ =
        (* multiple occurences of a node in the fold. *)
        let x = Var.create_ [%here] 1 in
        let f =
          unordered_array_fold
            [| watch x; watch x |]
            ~init:0
            ~f:( + )
            ~update:(F_inverse ( - ))
        in
        let o = observe f in
        stabilize_ [%here];
        assert (value o = 2);
        Var.set x 3;
        stabilize_ [%here];
        assert (value o = 6);
        disallow_future_use o;
        stabilize_ [%here];
        Var.set x 4;
        stabilize_ [%here];
        let o = observe f in
        stabilize_ [%here];
        assert (value o = 8)
      ;;

      let%expect_test "[~update:(Update _)]" =
        let x = Var.create_ [%here] 1 in
        let fold =
          unordered_array_fold
            [| Var.watch x |]
            ~init:0
            ~f:( + )
            ~update:
              (Update (fun acc ~old_value ~new_value -> acc - old_value + new_value))
        in
        let r = observe fold in
        let print () =
          stabilize_ [%here];
          let r = value r in
          print_s [%sexp (r : int)]
        in
        print ();
        [%expect {| 1 |}];
        Var.set x 3;
        print ();
        [%expect {| 3 |}]
      ;;

      let opt_unordered_array_fold = opt_unordered_array_fold

      let%expect_test _ =
        let o =
          observe
            (opt_unordered_array_fold
               [||]
               ~init:()
               ~f:(fun _ -> assert false)
               ~f_inverse:(fun _ -> assert false))
        in
        stabilize_ [%here];
        assert (is_some (value o))
      ;;

      let%expect_test _ =
        let x = Var.create_ [%here] None in
        let y = Var.create_ [%here] None in
        let t =
          observe
            (opt_unordered_array_fold
               [| watch x; watch y |]
               ~init:0
               ~f:( + )
               ~f_inverse:( - ))
        in
        let check where expect =
          stabilize_ where;
          [%test_eq: int option] (value t) expect
        in
        check [%here] None;
        Var.set x (Some 13);
        check [%here] None;
        Var.set y (Some 14);
        check [%here] (Some 27);
        Var.set y None;
        check [%here] None
      ;;

      let sum = sum

      let%expect_test _ =
        (* empty *)
        let o =
          observe
            (sum [||] ~zero:13 ~add:(fun _ -> assert false) ~sub:(fun _ -> assert false))
        in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        (* full recompute *)
        let x = Var.create_ [%here] 13. in
        let y = Var.create_ [%here] 15. in
        let num_adds = ref 0 in
        let add a b =
          incr num_adds;
          a +. b
        in
        let num_subs = ref 0 in
        let sub a b =
          incr num_subs;
          a -. b
        in
        let z =
          observe
            (sum
               [| watch x; watch y |]
               ~zero:0.
               ~add
               ~sub
               ~full_compute_every_n_changes:2)
        in
        stabilize_ [%here];
        assert (!num_adds = 2);
        assert (!num_subs = 0);
        assert (Float.equal (value z) 28.);
        Var.set x 17.;
        stabilize_ [%here];
        assert (!num_adds = 3);
        assert (!num_subs = 1);
        assert (Float.equal (value z) 32.);
        Var.set y 19.;
        stabilize_ [%here];
        (* [num_adds] increases 2 for the full recompute.  [num_subs] doesn't change
             because of the full recompute. *)
        [%test_result: int] !num_adds ~expect:5;
        [%test_result: int] !num_subs ~expect:1;
        assert (Float.equal (value z) 36.)
      ;;

      let opt_sum = opt_sum

      let%expect_test _ =
        let t =
          observe
            (opt_sum
               [||]
               ~zero:()
               ~add:(fun _ -> assert false)
               ~sub:(fun _ -> assert false))
        in
        stabilize_ [%here];
        assert (is_some (value t))
      ;;

      let%expect_test _ =
        let x = Var.create_ [%here] None in
        let y = Var.create_ [%here] None in
        let t = observe (opt_sum [| watch x; watch y |] ~zero:0 ~add:( + ) ~sub:( - )) in
        let check where expect =
          stabilize_ where;
          [%test_eq: int option] (value t) expect
        in
        check [%here] None;
        Var.set x (Some 13);
        check [%here] None;
        Var.set y (Some 14);
        check [%here] (Some 27);
        Var.set y None;
        check [%here] None
      ;;

      let sum_int = sum_int
      let sum_float = sum_float

      let test_sum (type a) sum (of_int : int -> a) equal =
        let x = Var.create_ [%here] (of_int 13) in
        let y = Var.create_ [%here] (of_int 15) in
        let z = observe (sum [| watch x; watch y |]) in
        stabilize_ [%here];
        assert (equal (value z) (of_int 28));
        stabilize_ [%here];
        Var.set x (of_int 17);
        stabilize_ [%here];
        assert (equal (value z) (of_int 32));
        Var.set x (of_int 19);
        Var.set y (of_int 21);
        stabilize_ [%here];
        assert (equal (value z) (of_int 40))
      ;;

      let%expect_test _ = test_sum sum_int Fn.id Int.equal
      let%expect_test _ = test_sum sum_float Float.of_int Float.equal

      let%expect_test _ =
        let o = observe (sum_float [||]) in
        stabilize_ [%here];
        [%test_result: Float.t] (value o) ~expect:0.
      ;;

      module Clock = Clock

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let w = observe (Clock.watch_now clock) in
        stabilize_ [%here];
        let before_advance = Clock.now clock in
        assert (Time_ns.equal before_advance (value w));
        let to_ = Time_ns.add before_advance (sec 1.) in
        Clock.advance_clock clock ~to_;
        assert (Time_ns.equal (Clock.now clock) to_);
        assert (Time_ns.equal (value w) before_advance);
        (* we didn't yet stabilize *)
        stabilize_ [%here];
        assert (Time_ns.equal (value w) to_)
      ;;

      let%expect_test "[advance_clock] backwards" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o = observe (Clock.watch_now clock) in
        let show_now () =
          stabilize_ [%here];
          print_s [%sexp (Clock.now clock : Time_ns.t), (value o : Time_ns.t)]
        in
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:00.000000000-05:00)
           (1969-12-31 19:00:00.000000000-05:00))
          |}];
        Clock.advance_clock clock ~to_:(Time_ns.add Time_ns.epoch (sec 1.));
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:01.000000000-05:00)
           (1969-12-31 19:00:01.000000000-05:00))
          |}];
        Clock.advance_clock clock ~to_:Time_ns.epoch;
        show_now ();
        [%expect
          {|
          ((1969-12-31 19:00:01.000000000-05:00)
           (1969-12-31 19:00:01.000000000-05:00))
          |}]
      ;;

      let is observer v = Poly.equal (value observer) v

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o = observe (Clock.after clock (sec 1.)) in
        let show () =
          stabilize_ [%here];
          print_s [%sexp (value o : Before_or_after.t)]
        in
        show ();
        [%expect {| Before |}];
        Clock.advance_clock_by clock (sec 1.);
        show ();
        [%expect {| After |}];
        Clock.advance_clock_by clock (Clock.alarm_precision clock);
        show ();
        [%expect {| After |}]
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ ->
          Clock.at clock (Time_ns.add (Clock.now clock) (sec 1.)))
      ;;

      let%test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        is_invalidated_on_bind_rhs (fun _ ->
          Clock.at clock (Time_ns.add (Clock.now clock) (sec (-1.))))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.after clock (sec 1.))
      ;;

      let%test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.after clock (sec (-1.)))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let now = Clock.now clock in
        let at span = observe (Clock.at clock (Time_ns.add now span)) in
        let i1 = at (sec (-1.)) in
        let i2 = at (sec (-0.1)) in
        let i3 = at (sec 1.) in
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 Before);
        Clock.advance_clock_by clock (sec 0.5);
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 Before);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        assert (is i1 After);
        assert (is i2 After);
        assert (is i3 After)
      ;;

      let%expect_test _ =
        (* advancing the clock in the same stabilization cycle as creation *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = observe (Clock.after clock (sec 1.)) in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (is i After)
      ;;

      let%expect_test _ =
        (* firing an unnecessary [after] and then observing it *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let i = Clock.after clock (sec (-1.)) in
        stabilize_ [%here];
        let o = observe i in
        stabilize_ [%here];
        assert (is o After);
        let r = ref 0 in
        let i =
          Clock.after clock (sec 1.)
          >>| fun z ->
          incr r;
          z
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (!r = 0);
        stabilize_ [%here];
        let o = observe i in
        stabilize_ [%here];
        assert (!r = 1);
        assert (is o After)
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        does_raise (fun () -> Clock.at_intervals clock (sec (-1.)))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        does_raise (fun () -> Clock.at_intervals clock (sec 0.))
      ;;

      let%test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        is_invalidated_on_bind_rhs (fun _ -> Clock.at_intervals clock (sec 1.))
      ;;

      let%expect_test _ =
        (* advancing the clock does nothing by itself *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref 0 in
        let i = Clock.at_intervals clock (sec 1.) >>| fun () -> incr r in
        let o = observe i in
        assert (!r = 0);
        Clock.advance_clock_by clock (sec 2.);
        assert (!r = 0);
        disallow_future_use o
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref (-1) in
        let i = Clock.at_intervals clock (sec 1.) >>| fun () -> incr r in
        let o = observe i in
        stabilize_ [%here];
        let show_r () = print_s [%sexp (!r : int)] in
        show_r ();
        [%expect {| 0 |}];
        Clock.advance_clock_by clock (sec 0.5);
        stabilize_ [%here];
        show_r ();
        [%expect {| 0 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        Clock.advance_clock_by clock (sec 1.);
        show_r ();
        [%expect {| 1 |}];
        stabilize_ [%here];
        show_r ();
        [%expect {| 2 |}];
        Clock.advance_clock_by clock (sec 10.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 3 |}];
        disallow_future_use o;
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        show_r ();
        [%expect {| 3 |}];
        let o = observe i in
        stabilize_ [%here];
        show_r ();
        [%expect {| 4 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* advancing exactly to intervals doesn't skip any *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let r = ref (-1) in
        let o = observe (Clock.at_intervals clock (sec 1.) >>| fun () -> incr r) in
        stabilize_ [%here];
        [%test_result: int] !r ~expect:0;
        let base = Clock.now clock in
        let curr = ref base in
        for i = 1 to 20 do
          curr := Time_ns.next_multiple ~base ~after:!curr ~interval:(sec 1.) ();
          Clock.advance_clock clock ~to_:!curr;
          stabilize_ [%here];
          [%test_result: int] !r ~expect:i
        done;
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* [interval < alarm precision] raises *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        assert (does_raise (fun () -> Clock.at_intervals clock (sec 0.0005)))
      ;;

      let%expect_test _ =
        (* [at] in the past *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        assert (
          is_error
            (Clock.snapshot
               clock
               (const 14)
               ~at:(Time_ns.sub (Clock.now clock) (sec 1.))
               ~before:13))
      ;;

      let%expect_test _ =
        (* [at] in the future *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o =
          observe
            (ok_exn
               (Clock.snapshot
                  clock
                  (const 14)
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:13))
        in
        stabilize_ [%here];
        assert (value o = 13);
        stabilize_ [%here];
        Clock.advance_clock_by clock (sec 2.);
        assert (value o = 13);
        stabilize_ [%here];
        assert (value o = 14)
      ;;

      let%expect_test _ =
        (* [at] in the future, unobserved *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create_ [%here] 13 in
        let i =
          ok_exn
            (Clock.snapshot
               clock
               (Var.watch x)
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:15)
        in
        stabilize_ [%here];
        Var.set x 17;
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        Var.set x 19;
        let o = observe i in
        stabilize_ [%here];
        assert (value o = 17)
      ;;

      let%expect_test _ =
        (* [advance_clock] past [at] prior to stabilization. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let o =
          observe
            (ok_exn
               (Clock.snapshot
                  clock
                  (const 15)
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:13))
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (value o = 15)
      ;;

      let%expect_test _ =
        (* unobserved, [advance_clock] past [at] prior to stabilization. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create_ [%here] 13 in
        let i =
          ok_exn
            (Clock.snapshot
               clock
               (Var.watch x)
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:15)
        in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        Var.set x 17;
        let o = observe i in
        stabilize_ [%here];
        assert (value o = 13)
      ;;

      let%expect_test _ =
        (* invalidated *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let t =
          ok_exn
            (Clock.snapshot
               clock
               invalid
               ~at:(Time_ns.add (Clock.now clock) (sec 1.))
               ~before:13)
        in
        let o = observe t in
        stabilize_ [%here];
        assert (value o = 13);
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        assert (skip_invalidity_check || not (is_valid t));
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* [snapshot] nodes increment [num_nodes_became_necessary] *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i1 = State.(num_nodes_became_necessary t) in
        let c = const () in
        for _ = 1 to 5 do
          ignore
            (ok_exn
               (Clock.snapshot
                  clock
                  c
                  ~at:(Time_ns.add (Clock.now clock) (sec 1.))
                  ~before:())
             : _ t)
        done;
        Clock.advance_clock_by clock (sec 2.);
        let i2 = State.(num_nodes_became_necessary t) in
        (* the 5 [snapshot]s that became [freeze] plus the [const] *)
        [%test_result: int] i2 ~expect:(i1 + 6)
      ;;

      let relative_step_function clock ~init steps =
        let now = Clock.now clock in
        Step_function.create_exn
          ~init
          ~steps:
            (List.map steps ~f:(fun (after, a) ->
               Time_ns.add now (sec (Float.of_int after)), a))
      ;;

      let relative_step_function_incr clock ~init steps =
        Clock.step_function
          clock
          ~init
          (List.map steps ~f:(fun (after, a) ->
             Time_ns.add (Clock.now clock) (sec (Float.of_int after)), a))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        require
          (is_invalidated_on_bind_rhs (fun i -> Clock.step_function clock ~init:i []))
      ;;

      let%expect_test _ =
        let clock = Clock.create ~start:Time_ns.epoch () in
        require
          (is_invalidated_on_bind_rhs (fun i ->
             relative_step_function_incr clock ~init:i [ 1, i + 1 ]))
      ;;

      let%expect_test _ =
        (* no steps *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = Clock.step_function clock ~init:13 [] in
        let o = observe i in
        stabilize_ [%here];
        print_s [%sexp (value o : int)];
        [%expect {| 13 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* one step at a time *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14; 2, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* all steps in the past *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 2.)) () in
        let i = relative_step_function_incr clock ~init:13 [ -2, 14; -1, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* some steps in the past *)
        let clock = Clock.create ~start:(Time_ns.add Time_ns.epoch (sec 1.)) () in
        let i = relative_step_function_incr clock ~init:13 [ -1, 14; 1, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* cross multiple steps in one stabilization cycle *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14; 2, 15 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Clock.advance_clock_by clock (sec 1.5);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* cross step in same stabilization as creation *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14 ] in
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        Clock.advance_clock_by clock (sec 2.);
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* observe after step *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = relative_step_function_incr clock ~init:13 [ 1, 14 ] in
        stabilize_ [%here];
        Clock.advance_clock_by clock (sec 1.5);
        stabilize_ [%here];
        let o = observe i in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 14 |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* advancing exactly to steps doesn't skip steps *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let base = Clock.now clock in
        let curr = ref base in
        let steps = ref [] in
        for i = 1 to 20 do
          curr := Time_ns.next_multiple ~base ~after:!curr ~interval:(sec 1.) ();
          steps := (!curr, i) :: !steps
        done;
        let steps = List.rev !steps in
        let o = observe (Clock.step_function clock ~init:0 steps) in
        List.iter steps ~f:(fun (to_, _) ->
          Clock.advance_clock clock ~to_;
          stabilize_ [%here];
          print_s [%sexp (value o : int)]);
        [%expect
          {|
          1
          2
          3
          4
          5
          6
          7
          8
          9
          10
          11
          12
          13
          14
          15
          16
          17
          18
          19
          20
          |}];
        disallow_future_use o
      ;;

      let%expect_test "[incremental_step_function]" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (Step_function.constant 1) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 1 |}];
        Var.set x (Step_function.constant 2);
        stabilize_ [%here];
        show ();
        [%expect {| 2 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function with step in the past, present, and \
                       future"
        =
        let test ~step_at =
          let clock = Clock.create ~start:Time_ns.epoch () in
          let x = Var.create (Step_function.constant 1) in
          let o = observe (Clock.incremental_step_function clock (watch x)) in
          let show () = print_s [%sexp (value o : int)] in
          stabilize_ [%here];
          show ();
          [%expect {| 1 |}];
          Var.set x (relative_step_function clock ~init:2 [ step_at, 3 ]);
          stabilize_ [%here];
          show ();
          disallow_future_use o
        in
        test ~step_at:(-1);
        [%expect {| 3 |}];
        test ~step_at:0;
        [%expect {| 3 |}];
        test ~step_at:1;
        [%expect {| 2 |}]
      ;;

      let%expect_test "incremental step function; advance time and change function" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (relative_step_function clock ~init:13 [ 1, 14 ]) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Var.set x (relative_step_function clock ~init:15 [ 1, 16 ]);
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 16 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function; change to const and then advance time" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (relative_step_function clock ~init:13 [ 1, 14 ]) in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        Var.set x (Step_function.constant 15);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        Clock.advance_clock_by clock (sec 1.);
        stabilize_ [%here];
        show ();
        [%expect {| 15 |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function is invalidated when child is" =
        let b = Var.create true in
        let clock = Clock.create ~start:Time_ns.epoch () in
        let t =
          Clock.incremental_step_function
            clock
            (if_ (watch b) ~then_:(const 0) ~else_:invalid >>| Step_function.constant)
        in
        let o = observe t in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 0 |}];
        Var.set b false;
        stabilize_ [%here];
        print_s [%sexp (check_invalidity && is_valid t : bool)];
        [%expect {| false |}];
        disallow_future_use o
      ;;

      let%expect_test "incremental step function that becomes observable in a \
                       stabilization after its child stabilizes"
        =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x = Var.create (Step_function.constant 13) in
        let ox = observe (watch x) in
        let t = Clock.incremental_step_function clock (watch x) in
        stabilize_ [%here];
        let o = observe t in
        let show () = print_s [%sexp (value o : int)] in
        stabilize_ [%here];
        show ();
        [%expect {| 13 |}];
        disallow_future_use ox;
        disallow_future_use o
      ;;

      let%expect_test "incremental step function of const used in other places" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let c = const (Step_function.constant 13) in
        let c1 = c >>| fun s -> s in
        let o1 = observe c1 in
        let x = Clock.incremental_step_function clock c in
        let ox = observe x in
        stabilize_ [%here];
        disallow_future_use o1;
        disallow_future_use ox
      ;;

      let%expect_test _ =
        (* Equivalence between [step_function] and reimplementation with [at] *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let my_step_function ~init steps =
          let xs =
            Array.map (Array.of_list steps) ~f:(fun (time, x) ->
              map (Clock.at clock time) ~f:(function
                | Before -> None
                | After -> Some x))
          in
          array_fold xs ~init ~f:(fun acc x -> Option.value x ~default:acc)
        in
        let base = Clock.now clock in
        let steps =
          List.map
            ~f:(fun (d, v) -> Time_ns.add base (sec d), v)
            [ 1.0, 1
            ; 1.99999, 2
              (* It is unspecified whether this alarm has fired when the
                 time is 2. but this test relies on the two
                 step_functions having the same unspecified behaviour. *)
            ; 2.0, 3
            ; 3.00001, 4
            ; 4.0, 5
            ; 4.00001, 6
            ; 5.0, 6
            ; 6.0, 7
            ]
        in
        let o1 = observe (Clock.step_function clock ~init:0 steps) in
        let o2 = observe (my_step_function ~init:0 steps) in
        stabilize_ [%here];
        for i = 1 to 7 do
          Clock.advance_clock clock ~to_:(Time_ns.add base (sec (Float.of_int i)));
          stabilize_ [%here];
          print_s [%sexp (value o1 : int), (value o2 : int)];
          require (value o1 = value o2)
        done;
        [%expect
          {|
          (1 1)
          (3 3)
          (3 3)
          (5 5)
          (6 6)
          (7 7)
          (7 7)
          |}];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test "[Step_function.create_from_sequence]" =
        let clock = Clock.create ~start:Time_ns.epoch () in
        let x =
          Var.create
            (Step_function.create_from_sequence
               ~init:13
               ~steps:
                 (Sequence.unfold
                    ~init:(Clock.now clock, 14)
                    ~f:(fun (at, i) ->
                      print_s [%message "unfold" (i : int)];
                      Some ((at, i), (Time_ns.add at (sec 1.), i + 1)))))
        in
        let o = observe (Clock.incremental_step_function clock (watch x)) in
        let show () = print_s [%sexp (value o : int)] in
        for _ = 1 to 5 do
          Clock.advance_clock_by clock (sec 1.);
          stabilize_ [%here];
          show ()
        done;
        [%expect
          {|
          (unfold (i 14))
          (unfold (i 15))
          (unfold (i 16))
          (unfold (i 16))
          15
          (unfold (i 16))
          (unfold (i 17))
          (unfold (i 17))
          16
          (unfold (i 17))
          (unfold (i 18))
          (unfold (i 18))
          17
          (unfold (i 18))
          (unfold (i 19))
          (unfold (i 19))
          18
          (unfold (i 19))
          (unfold (i 20))
          (unfold (i 20))
          19
          |}];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* Advancing to a scheduled time shouldn't break things. *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let fut = Time_ns.add (Clock.now clock) (sec 1.0) in
        let o1 = observe (Clock.at clock fut) in
        let o2 = observe (ok_exn (Clock.snapshot clock (const 1) ~at:fut ~before:0)) in
        Clock.advance_clock clock ~to_:fut;
        stabilize_ [%here];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* alarms get cleaned up for invalidated time-based incrementals *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        List.iter
          [ (fun () -> Clock.after clock (sec 1.) >>| fun _ -> ())
          ; (fun () -> Clock.at_intervals clock (sec 1.))
          ; (fun () -> relative_step_function_incr clock ~init:() [ 1, () ])
          ]
          ~f:(fun create_time_based_incremental ->
            let num_alarms = Clock.timing_wheel_length clock in
            let x = Var.create_ [%here] 0 in
            let o =
              observe
                (bind (Var.watch x) ~f:(fun i ->
                   if i >= 0 then create_time_based_incremental () else return ()))
            in
            stabilize_ [%here];
            for i = 1 to 10 do
              Var.set x i;
              stabilize_ [%here];
              if check_invalidity
              then
                [%test_result: int]
                  ~expect:(num_alarms + 1)
                  (Clock.timing_wheel_length clock)
            done;
            Var.set x (-1);
            stabilize_ [%here];
            if check_invalidity
            then [%test_result: int] ~expect:num_alarms (Clock.timing_wheel_length clock);
            disallow_future_use o)
      ;;

      let on_update = on_update

      let%expect_test _ =
        let v = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        let o = observe (watch v) in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        stabilize_ [%here];
        check [];
        Var.set v 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        disallow_future_use o;
        Var.set v 15;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* on-change handlers of a node that changes but is not necessary at the end
             of a stabilization *)
        let v = Var.create_ [%here] 0 in
        let n = Var.watch v in
        let push, check = on_update_queue () in
        on_update n ~f:push;
        let o = observe n in
        stabilize_ [%here];
        check [ Necessary 0 ];
        disallow_future_use o;
        Var.set v 1;
        let o = observe (freeze n) in
        stabilize_ [%here];
        check [ Unnecessary ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* value changing with different observers *)
        let v = Var.create_ [%here] 13 in
        let o = observe (watch v) in
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        let o = observe (watch v) in
        stabilize_ [%here];
        disallow_future_use o;
        check [ Necessary 14 ]
      ;;

      let%expect_test _ =
        (* call at next stabilization *)
        let v = Var.create_ [%here] 13 in
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        let r = ref 0 in
        on_update (Var.watch v) ~f:(fun _ -> incr r);
        stabilize_ [%here];
        assert (!r = 1);
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* called at next stabilization with [Unnecessary] update *)
        let v = Var.create_ [%here] 13 in
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* transition from unnecessary to necessary and back *)
        let x = Var.create 13 in
        let push, check = on_update_queue () in
        on_update (watch x) ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe (watch x) in
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set x 14;
        stabilize_ [%here];
        check [ Changed (13, 14) ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* an indirectly necessary node *)
        let x = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        on_update (Var.watch x) ~f:push;
        let t = Var.watch x >>| fun i -> i + 1 in
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe t in
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ];
        let o = observe t in
        stabilize_ [%here];
        check [ Necessary 13 ];
        disallow_future_use o;
        stabilize_ [%here];
        check [ Unnecessary ]
      ;;

      let%expect_test _ =
        (* [on_update] doesn't make a node necessary *)
        let v = Var.create_ [%here] 13 in
        let push, check = on_update_queue () in
        on_update (watch v) ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        stabilize_ [%here];
        check [];
        let o = observe (Var.watch v) in
        stabilize_ [%here];
        check [ Necessary 14 ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* invalid from the start *)
        let push, check = on_update_queue () in
        on_update invalid ~f:push;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ]
      ;;

      let%expect_test _ =
        (* invalidation of an unnecessary node *)
        let v = Var.create_ [%here] 13 in
        let r = ref None in
        let o =
          observe
            (bind (watch v) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Unnecessary ];
        Var.set v 14;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o
      ;;

      let%expect_test _ =
        (* invalidation of a necessary node *)
        let v = Var.create_ [%here] 13 in
        let r = ref None in
        let o1 =
          observe
            (bind (watch v) ~f:(fun i ->
               r := Some (const i);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let o2 = observe i in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Necessary 13 ];
        Var.set v 14;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* invalidation of a necessary node after a change *)
        let v = Var.create_ [%here] 13 in
        let w = Var.create_ [%here] 14 in
        let r = ref None in
        let o1 =
          observe
            (bind (watch v) ~f:(fun _ ->
               r := Some (watch w >>| Fn.id);
               return ()))
        in
        stabilize_ [%here];
        let i = Option.value_exn !r in
        let o2 = observe i in
        let push, check = on_update_queue () in
        on_update i ~f:push;
        stabilize_ [%here];
        check [ Necessary 14 ];
        Var.set w 15;
        stabilize_ [%here];
        check [ Changed (14, 15) ];
        Var.set v 16;
        stabilize_ [%here];
        if check_invalidity then check [ Invalidated ];
        disallow_future_use o1;
        disallow_future_use o2
      ;;

      let%expect_test _ =
        (* making a node necessary from an on-update handler *)
        let x = Var.create_ [%here] 13 in
        let y = Var.create_ [%here] 14 in
        let r = ref None in
        let push_x, check_x = on_update_queue () in
        on_update (watch x) ~f:push_x;
        let o = observe (watch y) in
        let push_o, check_o = on_observer_update_queue () in
        Observer.on_update_exn o ~f:(fun u ->
          push_o u;
          r := Some (observe (watch x)));
        stabilize_ [%here];
        check_x [ Unnecessary ];
        check_o [ Initialized 14 ];
        let ox = Option.value_exn !r in
        Var.set x 15;
        stabilize_ [%here];
        check_x [ Necessary 15 ];
        check_o [];
        disallow_future_use o;
        disallow_future_use ox
      ;;

      let%expect_test _ =
        (* calling [advance_clock] in an on-update handler *)
        let clock = Clock.create ~start:Time_ns.epoch () in
        let i = Clock.after clock (sec 1.) in
        let o = observe i in
        let num_fires = ref 0 in
        on_update i ~f:(fun _ ->
          incr num_fires;
          Clock.advance_clock_by clock (sec 2.));
        assert (!num_fires = 0);
        stabilize_ [%here];
        assert (!num_fires = 1);
        stabilize_ [%here];
        assert (!num_fires = 2);
        disallow_future_use o
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
