namespace WoofWare.Incremental.Test

open WoofWare.Incremental
open FsUnitTyped

type IncrementalFixture =
    private
        {
            Incremental : Incremental
            Invalid : int Node
        }

    static member Make () =
        let i = Incremental.make ()

        let invalid =
            let mutable r = None
            let x = i.Var.Create 13

            let o =
                i.Var.Watch x
                |> i.Bind (fun j ->
                    r <- Some (i.Const j)
                    i.Return ()
                )
                |> i.Observe

            i.Stabilize ()
            let t = r.Value
            i.Var.Set x 14
            i.Stabilize ()
            Observer.disallowFutureUse o
            t

        {
            Incremental = i
            Invalid = invalid
        }

    member this.I = this.Incremental

    member this.Stabilize () =
        State.invariant this.Incremental.State
        this.Incremental.Stabilize ()
        State.invariant this.Incremental.State
        Node.invariant ignore this.Invalid

[<AutoOpen>]
module Utils =
    let makeHigh (i : Incremental) (t : Node<'a>) : Node<'a> =
        let rec loop t n =
            if n = 0 then
                t
            else
                loop (i.Map2 (fun a _ -> a) t t) (n - 1)

        loop t 5

    let onUpdateQueue'<'update when 'update : equality> () =
        let mutable r : 'update list = []
        let add e = r <- e :: r

        let exp expect =
            List.rev r |> shouldEqual expect
            r <- []

        add, exp

    let inline onObserverUpdateQueue () = onUpdateQueue'<Observer.Update<int>> ()
    let inline onUpdateQueue () = onUpdateQueue'<NodeUpdate<int>> ()

    let isInvalid (fix : IncrementalFixture) (t : Node<'a>) =
        let o = fix.I.Observe t
        fix.Stabilize ()
        let result = Observer.value o |> Result.isError
        Observer.disallowFutureUse o

        result

    let isInvalidatedOnBindRhs (fix : IncrementalFixture) (f : int -> Node<'a>) : unit =
        let x = fix.I.Var.Create 13
        let r = ref None

        let o1 =
            fix.I.Var.Watch x
            |> fix.I.Bind (fun i ->
                r.Value <- Some (f i)
                fix.I.Return ()
            )
            |> fix.I.Observe

        fix.Stabilize ()
        let t = r.Value.Value
        let o2 = fix.I.Observe t
        fix.I.Var.Set x 14
        fix.Stabilize ()
        let result = isInvalid fix t
        Observer.disallowFutureUse o1
        Observer.disallowFutureUse o2
        result |> shouldEqual true
