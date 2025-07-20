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
            State.Expert.assertCurrentlyRunningNodeIsParent state t.Child "Dependency.value"
        // Not exposing the _exn, because this function is advertised as being usable only
        // inside the callbacks of parents, where it will not raise.
        Node.valueThrowing t.Child

type Expert1Node<'a> = | Expert1Node of Node<'a>

[<RequireQualifiedAccess>]
module Expert1Node =

    let create (state: State) (onObservabilityChange: (bool -> unit) option) (f: unit -> 'a) : Expert1Node<'a> =
        let onObservabilityChange = defaultArg onObservabilityChange (fun _ -> ())
        State.Expert.create state onObservabilityChange f
        |> Expert1Node

    let makeStale (Expert1Node (n : Node<'a>)) : unit = State.Expert.makeStale n
    let watch (Expert1Node n: Expert1Node<'a>) : Node<'a> = n
    let invalidate (Expert1Node (n : Node<'a>)) : unit = State.Expert.invalidate n
    let addDependency (Expert1Node (n : Node<'a>)) (Expert1Dependency (e : ExpertEdge<'b>)) : unit = State.Expert.addDependency n e
    let removeDependency (Expert1Node n) (Expert1Dependency e) : unit = State.Expert.removeDependency n e
