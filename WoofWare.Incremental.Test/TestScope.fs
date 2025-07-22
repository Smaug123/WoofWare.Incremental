namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestScope =

      module Scope = struct
        open Scope

        type nonrec t = t

        let top = top
        let is_top = is_top

        let%expect_test _ =
          let t = current () in
          assert (phys_equal t top)
        ;;

        let current = current
        let within = within

        let%expect_test _ =
          let o = observe (within (current ()) ~f:(fun () -> const 13)) in
          stabilize_ [%here];
          assert (value o = 13)
        ;;

        let%expect_test _ =
          (* escaping a [bind] *)
          let s = current () in
          let r = ref None in
          let x = Var.create_ [%here] 13 in
          let o =
            observe
              (bind (watch x) ~f:(fun i ->
                 r := Some (within s ~f:(fun () -> const i));
                 return ()))
          in
          stabilize_ [%here];
          let o2 = observe (Option.value_exn !r) in
          stabilize_ [%here];
          assert (value o2 = 13);
          Var.set x 14;
          stabilize_ [%here];
          assert (value o2 = 13);
          disallow_future_use o;
          stabilize_ [%here];
          assert (value o2 = 13)
        ;;

        let%expect_test _ =
          (* returning to a [bind] *)
          let r = ref None in
          let x = Var.create_ [%here] 13 in
          let o1 =
            observe
              (bind (watch x) ~f:(fun _i ->
                 r := Some (current ());
                 return ()))
          in
          stabilize_ [%here];
          let s = Option.value_exn !r in
          let o2 = observe (within s ~f:(fun () -> const 13)) in
          stabilize_ [%here];
          assert (value o2 = 13);
          Var.set x 14;
          disallow_future_use o2;
          stabilize_ [%here];
          disallow_future_use o1
        ;;

        let%test "top is top" = is_top top

        let%expect_test "scope inside bind is not top" =
          let i = Var.create_ [%here] true in
          let o =
            observe
              (bind (watch i) ~f:(fun b ->
                 assert (not (is_top (current ())));
                 return b))
          in
          stabilize_ [%here];
          print_s [%sexp (value o : bool)];
          [%expect {| true |}];
          Var.set i false;
          stabilize_ [%here];
          print_s [%sexp (value o : bool)];
          [%expect {| false |}]
        ;;
      end

