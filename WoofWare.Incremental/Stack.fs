namespace WoofWare.Incremental

open System.Collections.Generic

[<RequireQualifiedAccess>]
module internal Stack =

    let isEmpty<'a> (s : Stack<'a>) : bool = s.Count = 0

    let push<'a> (a : 'a) (s : Stack<'a>) : unit =
        s.Push a

    let invariant (invA : 'a -> unit) (s : Stack<'a>) : unit =
        for i in s do
            invA i
