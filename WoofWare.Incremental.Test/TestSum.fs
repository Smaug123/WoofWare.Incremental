namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestSum =

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

