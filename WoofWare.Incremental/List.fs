namespace WoofWare.PlayFetch

[<RequireQualifiedAccess>]
module internal List =

    let isSortedBy (projection : 'a -> 'b) (list : 'a list) : bool =
        let rec go (headProj : 'b) (l : 'a list) =
            match l with
            | [] -> true
            | next :: l ->
                let proj = projection next
                if headProj > proj then false else go proj l

        match list with
        | [] -> true
        | [ _ ] -> true
        | head :: rest ->
            let headProj = projection head
            go headProj rest
