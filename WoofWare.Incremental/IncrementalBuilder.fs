namespace WoofWare.Incremental

[<Sealed>]
type IncrementalBuilder (incr : Incremental) =

    member _.Return<'a> (x : 'a) : Node<'a> = incr.Return x

    member _.ReturnFrom<'a> (node : Node<'a>) : Node<'a> = node

    member _.Bind<'a, 'b> (node : Node<'a>, f : 'a -> Node<'b>) : Node<'b> = incr.Bind f node

    member _.MergeSources<'a, 'b> (n1 : Node<'a>, n2 : Node<'b>) : Node<'a * 'b> = incr.Both n1 n2

    member _.BindReturn<'a, 'b> (node : Node<'a>, f : 'a -> 'b) : Node<'b> = incr.Map f node

[<RequireQualifiedAccess>]
module IncrementalBuilder =
    let create (incr : Incremental) : IncrementalBuilder = IncrementalBuilder incr
