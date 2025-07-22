namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestCutoff =

      module Cutoff = struct
        open Cutoff

        type nonrec 'a t = 'a t

        let sexp_of_t = sexp_of_t
        let invariant = invariant
        let create = create

        (* tested below *)

        let _ = create
        let of_compare = of_compare
        let of_equal = of_equal
        let should_cutoff = should_cutoff

        let%expect_test _ =
          let t = of_compare Int.compare in
          assert (should_cutoff t ~old_value:0 ~new_value:0);
          assert (not (should_cutoff t ~old_value:0 ~new_value:1))
        ;;

        let%expect_test _ =
          let t = of_equal Int.equal in
          assert (should_cutoff t ~old_value:0 ~new_value:0);
          assert (not (should_cutoff t ~old_value:0 ~new_value:1))
        ;;

        let always = always

        let%expect_test _ =
          let x = Var.create_ [%here] 0 in
          set_cutoff (watch x) always;
          let r = ref 0 in
          let o = observe (watch x >>| fun _i -> incr r) in
          stabilize_ [%here];
          assert (!r = 1);
          List.iter
            [ 1, 1; 0, 1 ]
            ~f:(fun (v, expect) ->
              Var.set x v;
              stabilize_ [%here];
              assert (!r = expect));
          disallow_future_use o
        ;;

        let never = never

        let%expect_test _ =
          let x = Var.create_ [%here] 0 in
          set_cutoff (watch x) never;
          let r = ref 0 in
          let o = observe (watch x >>| fun _i -> incr r) in
          stabilize_ [%here];
          assert (!r = 1);
          List.iter
            [ 1, 2; 1, 3; 1, 4 ]
            ~f:(fun (v, expect) ->
              Var.set x v;
              stabilize_ [%here];
              assert (!r = expect));
          disallow_future_use o
        ;;

        let phys_equal = phys_equal

        let%expect_test _ =
          let r1 = ref () in
          let r2 = ref () in
          let x = Var.create_ [%here] r1 in
          set_cutoff (watch x) phys_equal;
          let r = ref 0 in
          let o = observe (watch x >>| fun _i -> incr r) in
          stabilize_ [%here];
          assert (!r = 1);
          List.iter
            [ r1, 1; r2, 2; r2, 2; r1, 3 ]
            ~f:(fun (v, expect) ->
              Var.set x v;
              stabilize_ [%here];
              assert (!r = expect));
          disallow_future_use o
        ;;

        let poly_equal = poly_equal

        let%expect_test _ =
          let r1a = ref 1 in
          let r1b = ref 1 in
          let r2 = ref 2 in
          let x = Var.create_ [%here] r1a in
          set_cutoff (watch x) poly_equal;
          let r = ref 0 in
          let o = observe (watch x >>| fun _i -> incr r) in
          stabilize_ [%here];
          assert (!r = 1);
          List.iter
            [ r1a, 1; r1b, 1; r2, 2; r1a, 3 ]
            ~f:(fun (v, expect) ->
              Var.set x v;
              stabilize_ [%here];
              assert (!r = expect));
          disallow_future_use o
        ;;

        let equal = equal
        let%test _ = equal never never
        let%test _ = not (equal never always)
      end

