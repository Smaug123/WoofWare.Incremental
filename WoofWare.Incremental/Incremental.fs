namespace WoofWare.Incremental

type Observer<'a> = Observer of Observer'<'a>

type Incremental =
    abstract Return<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    abstract VarCreate<'a> : 'a -> Var<'a>
    abstract VarSet<'a> : Var<'a> -> 'a -> unit
    abstract VarWatch : Var<'a> -> Node<'a>
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract State : State

type IncrementalImpl(state : State) =
    interface Incremental with
        member this.Return a = State.konst state a
        member this.Pack a = NodeCrate.make a
        member this.Map f a =
            State.map f a
        member this.VarCreate x = State.createVar state None x
        member this.VarWatch v = v.Watch
        member this.Bind f n = State.bind n f
        member this.Stabilize () = State.stabilize state
        member this.Observe n =
            State.createObserver None n
            |> Observer
        member this.VarSet var a = State.setVar var a
        member this.State = state

[<RequireQualifiedAccess>]
module Incremental =

    let make () : Incremental =
        let state = State.create 128
        IncrementalImpl state
        :> _
