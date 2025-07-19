namespace WoofWare.Incremental

type Staged<'a, 'b> = | Staged of ('a -> 'b)

[<RequireQualifiedAccess>]
module internal Staged =

    let stage<'a, 'b> (f : 'a -> 'b) : Staged<'a, 'b> = Staged.Staged f

    let unstage<'a, 'b> (Staged f) (a : 'a) : 'b = f a
