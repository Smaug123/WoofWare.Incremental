namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal ArrayFold =
    let compute (t : ArrayFold<'a, 'b>) : 'b =
      let mutable result = t.Init
      for i = 0 to Array.length t.Children - 1 do
        result <- t.F result t.Children.[i].ValueOpt.Value
      result

    let invariant (invA: 'a -> unit) (invB: 'b -> unit) (t : ArrayFold<'a, 'b>) : unit =
        invB t.Init
        for child in t.Children do
            match child.ValueOpt with
            | ValueNone -> ()
            | ValueSome child -> invA child
