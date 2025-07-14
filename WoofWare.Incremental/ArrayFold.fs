namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal ArrayFold =
    let compute (t : ArrayFold<'a, 'b>) : 'b =
      let mutable result = t.Init
      for i = 0 to Array.length t.Children - 1 do
        result <- t.F result t.Children.[i].ValueOpt.Value
      result
