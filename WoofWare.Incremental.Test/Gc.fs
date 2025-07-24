namespace WoofWare.Incremental.Test

open System

[<RequireQualifiedAccess>]
module Gc =
    let collect () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()
