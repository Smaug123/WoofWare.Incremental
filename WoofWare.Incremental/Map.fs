namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal Map =

    let merge (m1 : Map<'k, 'v>) (m2 : Map<'k, 'v>) (f : 'k -> 'v -> 'v -> 'v) =
        (m1, m2)
        ||> Map.fold (fun acc k v ->
            acc
            |> Map.change
                k
                (fun existing ->
                    match existing with
                    | None -> Some v
                    | Some existing -> Some (f k existing v)
                )
        )

    let addMulti (k : 'k) (v : 'v) (m : Map<'k, 'v list>) : Map<'k, 'v list> =
        m
        |> Map.change
            k
            (fun vs ->
                match vs with
                | None -> [ v ]
                | Some vs -> v :: vs
                |> Some
            )
