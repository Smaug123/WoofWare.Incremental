namespace WoofWare.PlayFetch

open System
open System.Runtime.ExceptionServices

type RaisedException =
    {
        Exn : exn
        Backtrace : ExceptionDispatchInfo
    }

[<RequireQualifiedAccess>]
module RaisedException =
    let create (e : exn) =
        {
            Exn = e
            Backtrace = ExceptionDispatchInfo.Capture e
        }

    let reraiseWithMessage (exn : RaisedException) (message : string) =
        raise (AggregateException (message, exn.Exn))

    let reraise (exn : RaisedException) =
        exn.Backtrace.Throw ()
        failwith "cannot reach"
