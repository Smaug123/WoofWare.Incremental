namespace WoofWare.Incremental.Test

open System.IO
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestDot =
    let saveDot (i : Incremental) : unit =
        let dotFile = Path.GetTempFileName ()
        i.SaveDot (fun s -> File.AppendAllText (dotFile, s))
        failwith "TODO: render dotfile"


      let print_dot_file node =
        let out = Format.formatter_of_out_channel Out_channel.stdout in
        Packed.save_dot out [ pack node ]
      ;;

      let%expect_test "plain node graphviz" =
        let n = return "hello" in
        print_dot_file n;
        [%expect
          {|
          digraph G {
            rankdir = BT
            n### [shape=Mrecord label="{{n###|Const|height=-1}}" ]
          }
          |}]
      ;;

      let%expect_test "annotated with info" =
        let n = return "hello" in
        set_user_info n (Some (Info.of_string "hello world"));
        print_dot_file n;
        [%expect
          {|
          digraph G {
            rankdir = BT
            n### [shape=Mrecord label="{{hello\ world}|{n###|Const|height=-1}}" ]
          }
          |}]
      ;;

      let%expect_test "annotated with label and attributes" =
        let n = return "hello" in
        append_user_info_graphviz
          n
          ~label:[ "hello"; "world" ]
          ~attrs:(String.Map.of_alist_exn [ "fillcolor", "green" ]);
        print_dot_file n;
        [%expect
          {|
          digraph G {
            rankdir = BT
            n### [shape=Mrecord label="{{hello|world}|{n###|Const|height=-1}}"  "fillcolor"="green"]
          }
          |}]
      ;;

