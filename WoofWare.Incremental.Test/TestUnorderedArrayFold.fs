namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestUnorderedArrayFold =

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

