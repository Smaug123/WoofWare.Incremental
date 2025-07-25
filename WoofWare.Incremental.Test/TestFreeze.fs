namespace WoofWare.Incremental.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.Incremental

[<TestFixture>]
module TestFreeze =
    [<Test>]
    let ``test 1`` () =
        let fix = IncrementalFixture.Make ()
        let I = fix.I

        let x = I.Var.Create 13
        let f = I.Freeze (I.Var.Watch x)
        let y = I.Observe f

        Node.isConst f |> shouldEqual false

        fix.Stabilize ()
        Observer.valueThrowing y |> shouldEqual 13
        Node.isConst f |> shouldEqual true

        let u = I.Var.Create 1
        let z =
            I.Var.Watch u
            |> I.Bind (fun _ -> I.Freeze (I.Var.Watch x))
            |> I.Observe
        fix.Stabilize ()

        Observer.valueThrowing z |> shouldEqual 13
        I.Var.Set u 2
        I.Var.Set x 14

        fix.Stabilize ()
        Observer.valueThrowing z |> shouldEqual 14

        I.Var.Set x 15
        fix.Stabilize ()
        Observer.valueThrowing z |> shouldEqual 14

        I.Var.Set u 3
        fix.Stabilize ()
        Observer.valueThrowing z |> shouldEqual 15

      (*
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


      *)
