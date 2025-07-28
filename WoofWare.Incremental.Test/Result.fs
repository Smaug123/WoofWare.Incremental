namespace WoofWare.Incremental.Test

[<RequireQualifiedAccess>]
module Result =
    let get r =
        match r with
        | Ok o -> o
        | Error e -> failwith $"error: {e}"

    let getError r =
        match r with
        | Error e -> e
        | Ok o -> failwith $"unexpectedly OK: {o}"
