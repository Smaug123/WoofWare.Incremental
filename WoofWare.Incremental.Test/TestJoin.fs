namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestJoin =
      module Join (X : sig
          val join : 'a t t -> 'a t
        end) =
      struct
        let join = X.join

        let%expect_test _ =
          (* [join] of a constant *)
          let o = observe (join (const (const 1))) in
          stabilize_ [%here];
          assert (value o = 1)
        ;;

        let%expect_test _ =
          (* graph changes only *)
          let a = const 3 in
          let b = const 4 in
          let x = Var.create_ [%here] a in
          let o = observe (join (watch x)) in
          let check where expect =
            stabilize_ where;
            [%test_result: int] (value o) ~expect
          in
          check [%here] 3;
          Var.set x b;
          check [%here] 4;
          Var.set x a;
          check [%here] 3
        ;;

        let%expect_test _ =
          let v1 = Var.create_ [%here] 1 in
          let v2 = Var.create_ [%here] 2 in
          let v3 = Var.create_ [%here] (Var.watch v1) in
          let o = observe (join (Var.watch v3)) in
          stabilize_ [%here];
          assert (value o = 1);
          Var.set v1 13;
          stabilize_ [%here];
          assert (value o = 13);
          Var.set v3 (Var.watch v2);
          stabilize_ [%here];
          assert (value o = 2);
          Var.set v3 (Var.watch v1);
          Var.set v1 14;
          stabilize_ [%here];
          assert (value o = 14)
        ;;

        let%expect_test _ =
          (* an invalid unused rhs doesn't invalidate the [join] *)
          let x = Var.create_ [%here] (const 0) in
          let lhs = Var.create_ [%here] 1 in
          let o1 =
            observe
              (bind (watch lhs) ~f:(fun i ->
                 Var.set x (const i);
                 return ()))
          in
          stabilize_ [%here];
          let o2 = observe (join (make_high (Var.watch x))) in
          stabilize_ [%here];
          Var.set lhs 2;
          (* invalidate *)
          Var.set x (const 3);
          stabilize_ [%here];
          assert (value o2 = 3);
          disallow_future_use o1;
          disallow_future_use o2
        ;;

        let%expect_test _ =
          (* checking that join can be invalidated *)
          let join = join (const invalid) in
          let o = observe join in
          stabilize_ [%here];
          disallow_future_use o;
          assert (skip_invalidity_check || not (is_valid join))
        ;;

        let%expect_test _ =
          (* changing the rhs from a node to its ancestor, which causes problems if
               we leave the node with a broken invariant while adding the ancestor. *)
          let num_calls = ref 0 in
          let rhs_var = Var.create 13 in
          let first =
            map (watch rhs_var) ~f:(fun i ->
              incr num_calls;
              i + 1)
          in
          let second = map first ~f:(fun i -> i + 1) in
          let lhs_var = Var.create first in
          let o = observe (join (watch lhs_var)) in
          stabilize_ [%here];
          [%test_result: int] !num_calls ~expect:1;
          Var.set lhs_var second;
          stabilize_ [%here];
          [%test_result: int] !num_calls ~expect:1;
          disallow_future_use o;
          stabilize_ [%here];
          Var.set rhs_var 14;
          stabilize_ [%here];
          [%test_result: int] !num_calls ~expect:1
        ;;
      end

      include Join (struct
          let join = join
        end)


