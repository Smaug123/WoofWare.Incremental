namespace WoofWare.Incremental

type Staged<'a> = | Staged of (unit -> 'a)

[<RequireQualifiedAccess>]
module internal Staged =

    let stage<'a> (f : unit -> 'a) : Staged<'a> = Staged.Staged f

    let unstage<'a> (Staged f) : 'a = f ()
