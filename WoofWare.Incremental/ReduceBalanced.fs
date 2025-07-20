namespace WoofWare.Incremental

open WoofWare.BalancedReducer

[<RequireQualifiedAccess>]
module internal ReduceBalanced =

    let create (state : State) (children : 'a Node []) (f : 'a -> 'b) (reduce : 'b -> 'b -> 'b) : 'b Node option =
        let len = Array.length children

        if len = 0 then
            None
        else

        let reducer = BalancedReducer.create len reduce
        if Debug.globalFlag then BalancedReducer.invariant reducer
        let node = Expert1Node.create state None (fun () ->
            let a = BalancedReducer.compute reducer
            if Debug.globalFlag then BalancedReducer.invariant reducer
            a
        )

        for i = 0 to len - 1 do
            Expert1Node.addDependency
                node
                (Expert1Dependency.create (Some (fun a ->
                    BalancedReducer.set reducer i (f a)
                    if Debug.globalFlag then BalancedReducer.invariant reducer
                )) children.[i])

        Some (Expert1Node.watch node)
