namespace WoofWare.Incremental.Test

open WoofWare.Incremental

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

[<RequireQualifiedAccess>]
module Utils =
      let makeHigh (i : Incremental) (t : Node<'a>) : Node<'a> =
        let rec loop t n =
          if n = 0 then t else loop (i.Map2 (fun a _ -> a) t t) (n - 1)
        loop t 5

