namespace WoofWare.Incremental.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Incremental

[<TestFixture>]
module TestRecomputeHeap =

    /// Builds a recompute heap with at least one height bucket containing two or more nodes, and
    /// returns the heap, a genuine non-head node from such a bucket, and the observers that keep
    /// the heap's nodes necessary (the caller must keep them rooted).
    let private heapWithNonHeadNode () : RecomputeHeap * NodeCrate * Observer<int> list =
        let i = Incremental.make ()

        // Several leaf var nodes; observing + stabilizing makes their watch nodes necessary at
        // height 0.
        let vars = [ for _ in 1..4 -> i.Var.Create 0 ]
        let observers = vars |> List.map (fun v -> i.Observe (i.Var.Watch v))
        i.Stabilize ()

        // Setting each var while not stabilizing re-adds its (now stale, still necessary) watch
        // node to the recompute heap. All four share height 0, so they pile into one bucket.
        vars |> List.iter (fun v -> i.Var.Set v 1)

        let rh = i.State.RecomputeHeap

        // Sanity: the uncorrupted heap satisfies its invariant.
        RecomputeHeap.invariant rh

        let nonHead =
            rh.NodesByHeight
            |> Array.tryPick (fun bucket ->
                match bucket with
                | ValueSome head ->
                    match NodeCrate.nextInRecomputeHeap head with
                    | ValueSome second -> Some second
                    | ValueNone -> None
                | ValueNone -> None
            )

        match nonHead with
        | Some n -> rh, n, observers
        | None -> failwith "test setup failed: expected a height bucket with >= 2 nodes"

    /// Overwrite a node crate's HeightInRecomputeHeap. ('ret cannot be instantiated with `unit`,
    /// so we return a dummy and discard it.)
    let private setHeightInRecomputeHeap (crate : NodeCrate) (h : int) : unit =
        crate.Apply
            { new NodeEval<int> with
                member _.Eval node =
                    node.HeightInRecomputeHeap <- h
                    0
            }
        |> ignore

    /// `RecomputeHeap.invariant` must walk *every* node in each height bucket (following
    /// `NextInRecomputeHeap`), not merely inspect the bucket's head. This corrupts a non-head
    /// node's recorded height and asserts the invariant notices.
    [<Test>]
    let ``invariant walks every node in each height bucket, not just the head`` () =
        let rh, nonHead, observers = heapWithNonHeadNode ()

        // Corrupt the non-head node so its recorded height no longer matches its bucket index.
        // (Stays >= 0, so it still counts as "in the recompute heap": this isolates the
        // per-bucket height check rather than the length-block membership check.)
        setHeightInRecomputeHeap nonHead (NodeCrate.heightInRecomputeHeap nonHead + 1)

        let threw =
            try
                RecomputeHeap.invariant rh
                false
            with _ ->
                true

        threw |> shouldEqual true

        // Keep the observers rooted so the watch nodes stayed necessary throughout.
        ignore observers

    /// The length-counting block must assert that every counted node is genuinely in the
    /// recompute heap, walking the whole bucket. A non-head node marked as not-in-heap
    /// (height < 0) is still linked into the list, so the count is unchanged; only the per-node
    /// membership assertion catches it. Because that block runs first, its message — not the
    /// later "bad height" check — is what surfaces.
    [<Test>]
    let ``invariant asserts every counted node is in the recompute heap`` () =
        let rh, nonHead, observers = heapWithNonHeadNode ()

        // Mark a non-head node as not-in-heap while leaving it linked into the bucket.
        setHeightInRecomputeHeap nonHead -1

        let message =
            try
                RecomputeHeap.invariant rh
                None
            with e ->
                Some e.Message

        match message with
        | None -> failwith "expected invariant to throw, but it did not"
        | Some m -> m.Contains "in recompute heap" |> shouldEqual true

        ignore observers
