namespace WoofWare.Incremental

open System.Collections.Concurrent

// TODO: Jane Street's Thread_safe_queue doesn't block, so this isn't
// an exact match.
// https://ocaml.janestreet.com/ocaml-core/v0.13/doc/core_kernel/Thread_safe_queue/index.html
type ThreadSafeQueue<'v> = ConcurrentQueue<'v>

[<RequireQualifiedAccess>]
module ThreadSafeQueue =
    let dequeueUntilEmpty<'a> (f : 'a -> unit) (queue : ThreadSafeQueue<'a>) : unit =
        let mutable cont = true

        while cont do
            match queue.TryDequeue () with
            | false, _ -> cont <- false
            | true, v -> f v

    let create<'a> () = ConcurrentQueue<'a> ()
    let enqueue<'a> (v : 'a) (t : ThreadSafeQueue<'a>) = t.Enqueue v
