namespace WoofWare.Incremental

type private SequenceUnfoldEval<'a, 'ret> =
    abstract Eval<'state> : init : 'state -> f : ('state -> ('a * 'state) option) -> 'ret

type private SequenceUnfoldCrate<'a> =
    abstract Apply<'ret> : SequenceUnfoldEval<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module private SequenceUnfoldCrate =
    let make init f =
        { new SequenceUnfoldCrate<_> with
            member _.Apply e = e.Eval init f
        }

type Sequence<'a> =
    private
    | Empty
    | OfList of 'a list
    | Unfold of SequenceUnfoldCrate<'a>

[<RequireQualifiedAccess>]
module Sequence =
    let empty<'a> () : Sequence<'a> = Sequence.Empty

    let ofList<'a> (l : 'a list) : 'a Sequence = Sequence.OfList l

    let next<'a> (s : 'a Sequence) : ('a * 'a Sequence) option =
        match s with
        | Sequence.Empty -> None
        | Sequence.OfList l ->
            match l with
            | [] -> None
            | head :: rest -> Some (head, Sequence.OfList rest)
        | Sequence.Unfold c ->
            { new SequenceUnfoldEval<_, _> with
                member _.Eval init f =
                    match f init with
                    | None ->
                        // TODO: we could be more efficient here by storing that the seq is empty;
                        // shouldn't need to evaluate f again when we're asked again
                        None
                    | Some (result, init) -> (result, Sequence.Unfold (SequenceUnfoldCrate.make init f)) |> Some
            }
            |> c.Apply

    let fold<'a, 'acc> (init : 'acc) (f : 'acc -> 'a -> 'acc) (s : 'a Sequence) : 'acc =
        match s with
        | Sequence.Empty -> init
        | Sequence.OfList l -> List.fold f init l
        | Sequence.Unfold cr ->
            // hylomorphisms r us
            { new SequenceUnfoldEval<_, _> with
                member _.Eval<'state> (origInit : 'state) (origF : 'state -> ('a * 'state) option) : 'acc =
                    let mutable result = init
                    let mutable unfoldState = origInit
                    let mutable next = Unchecked.defaultof<_>

                    while (next <- origF unfoldState
                           next.IsSome) do
                        let elt, next' = next.Value
                        unfoldState <- next'
                        result <- f result elt

                    result
            }
            |> cr.Apply

    let unfold<'state, 'a> (init : 'state) (f : 'state -> ('a * 'state) option) : 'a Sequence =
        Sequence.Unfold (SequenceUnfoldCrate.make init f)

    let head (s : 'a Sequence) : 'a option = next s |> Option.map fst
