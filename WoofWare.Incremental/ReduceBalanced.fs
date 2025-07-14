namespace WoofWare.PlayFetch

open WoofWare.BalancedReducer

[<RequireQualifiedAccess>]
module internal ReduceBalanced =

    let create state children f reduce =
      let len = Array.length children
      if len = 0 then None else

      let reducer = BalancedReducer.create len reduce
      let node =
        Expert1.Node.create state (fun () ->
          BalancedReducer.compute reducer
        )
      for i = 0 to len - 1 do
        Expert1.Node.add_dependency
          node
          (Expert1.Dependency.create children.[i] (fun a ->
             BalancedReducer.set reducer i (f a)
         ))
      Some (Expert1.Node.watch node)
