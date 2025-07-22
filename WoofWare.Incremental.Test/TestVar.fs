namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestVar =

      module Var = struct
        open Var

        type nonrec 'a t = 'a t [@@deriving sexp_of]

        let create_ ?use_current_scope _where value = create ?use_current_scope value
        let create = create
        let latest_value = latest_value
        let set = set
        let value = value
        let watch = watch
        let replace = replace

        let%expect_test _ =
          (* observing a var after stabilization *)
          let x = create_ [%here] 0 in
          stabilize_ [%here];
          let o = observe (watch x) in
          stabilize_ [%here];
          assert (Observer.value_exn o = 0)
        ;;

        let%expect_test _ =
          (* observing a set var after stabilization *)
          let x = create_ [%here] 0 in
          set x 1;
          stabilize_ [%here];
          let o = observe (watch x) in
          stabilize_ [%here];
          assert (Observer.value_exn o = 1)
        ;;

        let%expect_test _ =
          (* observing a replace var after stabilization *)
          let x = create_ [%here] 0 in
          replace x ~f:(( + ) 1);
          stabilize_ [%here];
          let o = observe (watch x) in
          stabilize_ [%here];
          assert (Observer.value_exn o = 1)
        ;;

        let%expect_test _ =
          (* observing and setting var after stabilization *)
          let x = create_ [%here] 0 in
          stabilize_ [%here];
          let o = observe (watch x) in
          set x 1;
          stabilize_ [%here];
          assert (Observer.value_exn o = 1)
        ;;

        let%expect_test _ =
          (* [set] without stabilizing *)
          let x = create_ [%here] 13 in
          assert (value x = 13);
          assert (latest_value x = 13);
          let o = observe (watch x) in
          stabilize_ [%here];
          assert (Observer.value_exn o = 13);
          set x 14;
          assert (value x = 14);
          assert (latest_value x = 14);
          assert (Observer.value_exn o = 13)
        ;;

        let%expect_test _ =
          (* [set] during stabilization *)
          let v0 = create_ [%here] 0 in
          let v1 = create_ [%here] 1 in
          let v2 = create_ [%here] 2 in
          let o0 = observe (watch v0) in
          let o1 =
            observe
              (watch v1
               >>| fun i ->
               let i0 = value v0 in
               set v0 i;
               assert (value v0 = i0);
               assert (latest_value v0 = i);
               let i2 = value v2 in
               set v2 i;
               assert (value v2 = i2);
               assert (latest_value v2 = i);
               i)
          in
          let o2 = observe (watch v2) in
          let var_values_are i0 i1 i2 = value v0 = i0 && value v1 = i1 && value v2 = i2 in
          let observer_values_are i0 i1 i2 =
            Observer.value_exn o0 = i0
            && Observer.value_exn o1 = i1
            && Observer.value_exn o2 = i2
          in
          assert (var_values_are 0 1 2);
          stabilize_ [%here];
          assert (observer_values_are 0 1 2);
          assert (var_values_are 1 1 1);
          stabilize_ [%here];
          assert (observer_values_are 1 1 1);
          assert (var_values_are 1 1 1);
          set v1 13;
          assert (observer_values_are 1 1 1);
          assert (var_values_are 1 13 1);
          stabilize_ [%here];
          assert (observer_values_are 1 13 1);
          assert (var_values_are 13 13 13);
          stabilize_ [%here];
          assert (observer_values_are 13 13 13);
          assert (var_values_are 13 13 13)
        ;;

        let%expect_test _ =
          (* [set] during stabilization gets the last value that was set *)
          let x = create_ [%here] 0 in
          let o =
            observe
              (map (watch x) ~f:(fun v ->
                 set x 1;
                 set x 2;
                 assert (latest_value x = 2);
                 v))
          in
          stabilize_ [%here];
          assert (value x = 2);
          stabilize_ [%here];
          [%test_result: int] (Observer.value_exn o) ~expect:2;
          disallow_future_use o
        ;;

        let%expect_test _ =
          (* [replace] during stabilization gets the latest value *)
          let x = create_ [%here] 0 in
          let o =
            observe
              (map (watch x) ~f:(fun v ->
                 set x 2;
                 replace x ~f:(fun v ->
                   assert (v = 2);
                   v + 1);
                 v))
          in
          stabilize_ [%here];
          print_s [%sexp (value x : int)];
          [%expect {| 3 |}];
          print_s [%sexp (Observer.value_exn o : int)];
          [%expect {| 0 |}];
          stabilize_ [%here];
          print_s [%sexp (Observer.value_exn o : int)];
          [%expect {| 3 |}];
          disallow_future_use o
        ;;

        let%expect_test _ =
          (* [create] during stabilization *)
          let o =
            observe
              (bind (const 13) ~f:(fun i ->
                 let v = create_ [%here] i in
                 watch v))
          in
          stabilize_ [%here];
          assert (Observer.value_exn o = 13)
        ;;

        let%expect_test _ =
          (* [create] and [set] during stabilization *)
          let o =
            observe
              (bind (const 13) ~f:(fun i ->
                 let v = create_ [%here] i in
                 let t = watch v in
                 set v 15;
                 t))
          in
          stabilize_ [%here];
          assert (Observer.value_exn o = 13)
        ;;

        let%expect_test _ =
          (* maybe invalidating a variable *)
          List.iter [ false; true ] ~f:(fun use_current_scope ->
            let lhs = Var.create 0 in
            let rhs = ref (const 0) in
            let o =
              observe
                (bind (watch lhs) ~f:(fun i ->
                   rhs := Var.watch (create_ [%here] ~use_current_scope i);
                   !rhs))
            in
            stabilize_ [%here];
            let rhs = !rhs in
            assert (is_valid rhs);
            set lhs 1;
            stabilize_ [%here];
            if check_invalidity
            then [%test_result: bool] (not (is_valid rhs)) ~expect:use_current_scope;
            assert (Observer.value_exn o = 1))
        ;;
      end

