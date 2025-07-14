module WoofWare.PlayFetch.Observer

[<RequireQualifiedAccess>]
module internal Observer =
    let observing t = InternalObserver.observing t.Value
    let useIsAllowed t = InternalObserver.useIsAllowed t.Value
    let valueThrowing t = InternalObserver.valueThrowing t.Value
    let incrState t = InternalObserver.incrState t.Value

    let onUpdateThrowing t onUpdateHandler =
      InternalObserver.onUpdateThrowing t.Value onUpdateHandler
