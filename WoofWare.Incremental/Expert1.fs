namespace WoofWare.Incremental

type Expert1Dependency<'a> = | Expert1Dependency of 'a ExpertEdge

[<RequireQualifiedAccess>]
module Expert1Dependency =
    let create (onChange : _ option) child : 'a Expert1Dependency =
        {
            Child = child
            Index = ValueNone
            OnChange = onChange |> Option.defaultValue ignore
        }
        |> Expert1Dependency

    let value (Expert1Dependency t) =
        let state = t.Child.State

        if Debug.globalFlag then
            Expert.assertCurrentlyRunningNodeIsParent state t.Child "Dependency.value"
        // Not exposing the _exn, because this function is advertised as being usable only
        // inside the callbacks of parents, where it will not raise.
        Node.valueThrowing t.Child

type Expert1Node<'a> = | Expert1Node of Node<'a>

[<RequireQualifiedAccess>]
module Expert1Node =

    let create state onObservabilityChange f =
        let onObservabilityChange = defaultArg onObservabilityChange (fun _ -> ())
        State.Expert.create state onObservabilityChange f

    let make_stale = State.Expert.make_stale
    let watch = Fn.id
    let invalidate = State.Expert.invalidate
    let add_dependency = State.Expert.add_dependency
    let remove_dependency = State.Expert.remove_dependency
