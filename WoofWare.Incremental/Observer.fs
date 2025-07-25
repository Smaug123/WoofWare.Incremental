namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal Observer' =
    let observing<'a> (t : Observer'<'a>) = InternalObserver.observing t.Value
    let useIsAllowed<'a> (t : Observer'<'a>) = InternalObserver.useIsAllowed t.Value
    let valueThrowing<'a> (t : Observer'<'a>) = InternalObserver.valueThrowing t.Value
    let incrState<'a> (t : Observer'<'a>) = InternalObserver.incrState t.Value

    let onUpdateThrowing<'a> (t : Observer'<'a>) onUpdateHandler =
        InternalObserver.onUpdateThrowing t.Value onUpdateHandler
