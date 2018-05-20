[<AutoOpen>]
module Array
    open System.Linq

    /// Same as Array.take, but doesn't throw an exception if the source collection contains less than items than specified..
    let takeSafe (count: int) (array: 'a[]) : 'a[] =
        Enumerable.Take(array, count) |> Array.ofSeq
