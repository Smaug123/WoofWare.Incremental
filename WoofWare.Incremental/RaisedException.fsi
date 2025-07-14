namespace WoofWare.PlayFetch

open System.Runtime.ExceptionServices

type RaisedException =
    {
        Exn : exn
        Backtrace : ExceptionDispatchInfo
    }

[<RequireQualifiedAccess>]
module RaisedException =
    val create : exn -> RaisedException

    val reraiseWithMessage : RaisedException -> string -> 'a

    val reraise : RaisedException -> 'a
