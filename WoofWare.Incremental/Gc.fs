namespace WoofWare.Incremental

open System.Runtime.CompilerServices

type private FinalizerAction<'a when 'a : not struct> (action : 'a -> unit, target : 'a) =
    override this.Finalize () =
        try
            action target
        with e ->
            System.Diagnostics.Debug.WriteLine ("unhandled exception in finalizer: " + e.Message)
            reraise ()

[<RequireQualifiedAccess>]
module internal Gc =
    // Table to associate finalizer objects with targets
    let private finalizerTable = ConditionalWeakTable<obj, obj> ()

    /// Do not throw inside `action`! `action` is run inside a finalizer, so throwing may
    /// bring down the entire process.
    /// We also *strongly recommend* that it be thread-safe, because it will run concurrently with other user code
    /// (on the finalizer thread).
    /// Probably also don't use `action` to resurrect its input (e.g. by storing it in some array); this is intended
    /// to be called as the object is being finalized.
    ///
    /// You can only do this once to any given `target`.
    let addFinalizerIgnore<'a when 'a : not struct> (target : 'a) (action : 'a -> unit) : unit =
        let finalizer = FinalizerAction (action, target)
        finalizerTable.Add (target, finalizer)
