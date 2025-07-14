namespace WoofWare.PlayFetch

open System

[<NoEquality ; NoComparison>]
type internal 'a Cutoff =
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
module internal Cutoff =
    let create f = Cutoff.F f
    let ofCompare f = Cutoff.Compare f
    let ofEqual f = Cutoff.Equal f
    let never<'a> : 'a Cutoff = Cutoff.Never
    let always<'a> : 'a Cutoff = Cutoff.Always
    let polyEqual<'a when 'a : equality> : 'a Cutoff = Cutoff.Equal (fun a b -> a = b)

    let shouldCutoff t old newValue =
        match t with
        | Cutoff.PhysEqual -> Object.ReferenceEquals (old, newValue)
        | Cutoff.Never -> false
        | Cutoff.Always -> true
        | Cutoff.Compare f -> f old newValue = 0
        | Cutoff.Equal f -> f old newValue
        | Cutoff.F f -> f old newValue

    let equal t1 t2 =
        match t1, t2 with
        | Cutoff.Always, Cutoff.Always -> true
        | Cutoff.Always, _ -> false
        | Cutoff.Never, Cutoff.Never -> true
        | Cutoff.Never, _ -> false
        | Cutoff.PhysEqual, Cutoff.PhysEqual -> true
        | Cutoff.PhysEqual, _ -> false
        | Cutoff.Compare f1, Cutoff.Compare f2 -> Object.ReferenceEquals (f1, f2)
        | Cutoff.Compare _, _ -> false
        | Cutoff.Equal f1, Cutoff.Equal f2 -> Object.ReferenceEquals (f1, f2)
        | Cutoff.Equal _, _ -> false
        | Cutoff.F f1, F f2 -> Object.ReferenceEquals (f1, f2)
        | Cutoff.F _, _ -> false

    let physEqual = Cutoff.PhysEqual
