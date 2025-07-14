namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module NodeToDot =
    let forAnalyzer name kind height =
        let label =
          [ name; string<Kind<_>> kind; $"height=%d{height}" ]
        DotUserInfo.dot label Map.empty

    let printNode (name: string) (kind: Kind<'a>) (height: int) (user_info: DotUserInfo option) : string =
      let default' = forAnalyzer name kind height
      let info =
        match user_info with
        | None -> default'
        | Some user_info -> DotUserInfo.append default' user_info
      sprintf
        "%s\n"
        (DotUserInfo.toString "Mrecord" name (DotUserInfo.toDot info))

    let renderDot emitBindEdges out ts =
        seq {
            let node_name id =
                "n" ^ NodeId.toString id
            yield "digraph G {\n"
            yield "  rankdir = BT\n"

            let seen = For_analyzer.Node_id.Hash_set.create ()
            let bind_edges = ref [] in
            For_analyzer.traverse
              ts
              ~add_node:
                (fun
                  ~id
                  ~kind
                  ~cutoff:_
                  ~children
                  ~bind_children
                  ~user_info
                  ~recomputed_at:_
                  ~changed_at:_
                  ~height
                ->
                let name = node_name id in
                Hash_set.add seen id;
                print_node out ~name ~kind ~height ~user_info;
                List.iter children ~f:(fun child_id ->
                  Format.fprintf out "  %s -> %s\n" (node_name child_id) name);
                List.iter bind_children ~f:(fun bind_child_id ->
                  bind_edges := (bind_child_id, id) :: !bind_edges));
            if emit_bind_edges
            then
              List.iter !bind_edges ~f:(fun (bind_child_id, id) ->
                if Hash_set.mem seen bind_child_id
                then
                  Format.fprintf
                    out
                    "  %s -> %s [style=dashed]\n"
                    (node_name id)
                    (node_name bind_child_id));
            Format.fprintf out "}\n%!"
      }

    let saveDotToFile emitBindEdges file ts =
      Out_channel.with_file file ~f:(fun out ->
        let formatter = Format.formatter_of_out_channel out in
        save_dot ~emit_bind_edges formatter ts)
