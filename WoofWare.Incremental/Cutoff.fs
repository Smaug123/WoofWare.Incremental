namespace WoofWare.Incremental

[<NoEquality ; NoComparison>]
type 'a Cutoff =
    (* We specialize some cutoffs to avoid an indirect function call; in particular we
     specialize the default (and hence overwhelmingly common) case of physical
     equality. *)
    | Always
    | Never
    | PhysEqual
    | Compare of ('a -> 'a -> int)
    | Equal of ('a -> 'a -> bool)
    /// old -> new -> bool
    | F of ('a -> 'a -> bool)

[<RequireQualifiedAccess>]
module Cutoff =
    let create f = Cutoff.F f
    let ofCompare f = Cutoff.Compare f
    let ofEqual f = Cutoff.Equal f
    let never<'a> : 'a Cutoff = Cutoff.Never
    let always<'a> : 'a Cutoff = Cutoff.Always
    let polyEqual<'a when 'a : equality> : 'a Cutoff = Cutoff.Equal (fun a b -> a = b)

    let shouldCutoff (t : 'a Cutoff) (old : 'a) (newValue : 'a) : bool =
        match t with
        | Cutoff.PhysEqual -> Type.arePhysEqual old newValue
        | Cutoff.Never -> false
        | Cutoff.Always -> true
        | Cutoff.Compare f -> f old newValue = 0
        | Cutoff.Equal f -> f old newValue
        | Cutoff.F f -> f old newValue

    let physEqual<'a> = Cutoff<'a>.PhysEqual

    let invariant<'a> (_inv : 'a -> unit) (c : 'a Cutoff) =
        match c with
        | Cutoff.Always
        | Cutoff.Never
        | Cutoff.PhysEqual
        | Cutoff.Compare _
        | Cutoff.Equal _
        | Cutoff.F _ -> ()
