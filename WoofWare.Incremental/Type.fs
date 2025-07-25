namespace WoofWare.Incremental

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type internal Type () =

    static member inline arePhysEqual<'a> (x : 'a) (y : 'a) : bool =
        if typeof<'a>.IsValueType then
            EqualityComparer.Default.Equals (x, y)
        else
            Object.ReferenceEquals (x, y)

    static member inline referenceEqual<'a when 'a : not struct> (x : 'a) (y : 'a) = Object.ReferenceEquals (x, y)

    static member inline referenceEqual'<'a, 'b when 'a : not struct and 'b : not struct> (x : 'a) (y : 'b) =
        Object.ReferenceEquals (x, y)
