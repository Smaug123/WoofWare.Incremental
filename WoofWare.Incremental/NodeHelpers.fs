namespace WoofWare.Incremental

open System
open TypeEquality

[<RequireQualifiedAccess>]
module internal NodeHelpers =
    let typeEqualIfPhysSame<'a, 'b> (t1 : Node<'a>) (t2 : Node<'b>) : Teq<'a, 'b> option =
        (* This is type-safe assuming no one can give the same incremental node two different
           types.  This is true because the field [mutable old_value_opt : 'a option] prevents
           both subtyping and parameteric polymorphism.  But this allows to break
           abstractions, as in someone could write:

           {[
             type t
             type u (* = t underneath *)
             val create : unit -> t Incr.t * u Incr.t (* the two incrementals are phys_equal *)
           ]}

           and we would figure out that type t = u.  However, we could add a Type_equal.Id to
           nodes and do the same, so it seems to be a more general issue. *)
        if Object.ReferenceEquals (t1, t2) then
            Teq.tryRefl<'a, 'b>
        else
            None

    let isValid<'a> (n : Node<'a>) : bool =
        match n.Kind with
        | Kind.Invalid -> false
        | _ -> true

    let isNecessary<'a> (n : 'a Node) : bool =
        n.NumParents > 0
        || n.Observers.IsSome
        || (
            match n.Kind with
            | Kind.Freeze _ -> true
            | _ -> false
        )
        || n.ForceNecessary
