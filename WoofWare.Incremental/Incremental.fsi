namespace WoofWare.Incremental

type Observer<'a>

type Incremental =
    abstract Return<'a> : 'a -> Node<'a>
    abstract Pack<'a> : Node<'a> -> NodeCrate
    abstract Map<'a, 'b> : ('a -> 'b) -> Node<'a> -> Node<'b>
    abstract VarCreate<'a> : 'a -> Var<'a>
    abstract VarSet<'a> : Var<'a> -> 'a -> unit
    abstract VarWatch<'a> : Var<'a> -> Node<'a>
    abstract Bind<'a, 'b> : ('a -> Node<'b>) -> Node<'a> -> Node<'b>
    abstract Stabilize : unit -> unit
    abstract Observe<'a> : Node<'a> -> Observer<'a>
    abstract State : State

[<RequireQualifiedAccess>]
module Incremental =
    val make : unit -> Incremental
