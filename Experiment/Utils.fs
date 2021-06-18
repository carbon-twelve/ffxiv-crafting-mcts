namespace FfxivCrafting

open System.Collections.Generic
open System.Runtime.CompilerServices
open FSharpPlus

module Utils =
    [<Extension>]
    type DictionaryExtensions =
        [<Extension>]
        static member inline GetOrDefault(dict: Dictionary<'TKey, 'TValue>, key: 'TKey, defaultValue: 'TValue): 'TValue =
            match Dict.tryGetValue key dict with
            | Some value -> value
            | None -> defaultValue
        [<Extension>]
        static member inline ComputeIfAbsent(dict: Dictionary<'TKey, 'TValue>, key: 'TKey, func: unit -> 'TValue): 'TValue =
            match Dict.tryGetValue key dict with
            | Some value -> value
            | None ->
                let value = func ()
                dict.Add(key, value)
                value

    let undefined() = failwith ""

    type ProgressEnumerator<'T>(enumerator: IEnumerator<'T>, size: int, step: int) =
        let stopwatch = System.Diagnostics.Stopwatch()

        let mutable iterCount = 0

        let mutable lastTime = 0L

        interface IEnumerator<'T> with
            member _.Current = enumerator.Current
            member _.Dispose() = enumerator.Dispose()
        
        interface System.Collections.IEnumerator with
            member this.Current = (this :> IEnumerator<'T>).Current :> obj
            member _.MoveNext() =
                if stopwatch.IsRunning then
                    iterCount <- iterCount + 1
                    lastTime <- stopwatch.ElapsedMilliseconds
                    let averageMillis = float stopwatch.ElapsedMilliseconds / float iterCount
                    printfn
                        "%.1f%% %.1fs/it %.1fs/%.1fs"
                        (float iterCount / float size * 100.)
                        (averageMillis / 1000.)
                        (float stopwatch.ElapsedMilliseconds / 1000.)
                        (float (size - iterCount) * averageMillis / 1000.)
                else
                    stopwatch.Start()
                enumerator.MoveNext()
            member _.Reset() =
                stopwatch.Reset()
                enumerator.Reset()

    type ProgressEnumerable<'T>(seq: 'T seq, size, step) =
        interface IEnumerable<'T> with
            member _.GetEnumerator(): IEnumerator<'T> =
                new ProgressEnumerator<'T>(seq.GetEnumerator(), size, step) :> IEnumerator<'T>
        interface System.Collections.IEnumerable with
            member this.GetEnumerator(): System.Collections.IEnumerator =
                (this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

    let progress (start: int) (step: int) (``end``: int) =
        ProgressEnumerable(seq { start .. ``end`` }, ``end`` - start + 1, step)

    type FixedSizedQueue<'T>(size: int) =
        let queue = Queue<'T>(size)

        member _.EnqueueAll(items: 'T seq) =
            items
            |> Seq.iter (fun item ->
                if queue.Count = size then queue.Dequeue() |> ignore
                queue.Enqueue(item))
        
        member _.InnerQueue = queue