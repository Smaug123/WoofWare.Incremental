namespace WoofWare.Incremental

type ExpertDependency<'a> = 'a ExpertEdge

[<RequireQualifiedAccess>]
module ExpertDependency =
  let create (onChange : _ option) child : 'a ExpertDependency =
      {
          Child = child
          Index = ValueNone
          OnChange = onChange |> Option.defaultValue ignore
      }

  let value (t : _ ExpertDependency) =
    let state = t.Child.State
    if Debug.globalFlag then
      Expert.assert_currently_running_node_is_parent
        state
        t.child
        "Dependency.value";
    (* Not exposing the _exn, because this function is advertised as being usable only
       inside the callbacks of parents, where it will not raise. *)
    Node.value_exn t.child
  ;;
end

module Node = struct
  type nonrec 'a t = 'a Node.t [@@deriving sexp_of]

  let create state ?(on_observability_change = fun ~is_now_observable:_ -> ()) f =
    State.Expert.create state ~on_observability_change f
  ;;

  let make_stale = State.Expert.make_stale
  let watch = Fn.id
  let invalidate = State.Expert.invalidate
  let add_dependency = State.Expert.add_dependency
  let remove_dependency = State.Expert.remove_dependency
end
