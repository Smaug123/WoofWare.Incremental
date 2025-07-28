namespace WoofWare.Incremental.Test

open System

[<RequireQualifiedAccess>]
module Gc =
    let inline collect () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()
