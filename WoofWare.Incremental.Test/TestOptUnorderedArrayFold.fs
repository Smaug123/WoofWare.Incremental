namespace WoofWare.Incremental.Test

open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestOptUnorderedArrayFold =

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

