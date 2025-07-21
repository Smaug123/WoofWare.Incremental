namespace WoofWare.Incremental

[<RequireQualifiedAccess>]
module internal Seq =

    let tryReduce<'a> (f : 'a -> 'a -> 'a) (l : 'a seq) : 'a option =
        use enum = l.GetEnumerator ()

        if not (enum.MoveNext ()) then
            None
        else

        let mutable result = enum.Current

        while enum.MoveNext () do
            result <- f result enum.Current

        Some result

    let tryMin<'a when 'a : comparison> (l : 'a seq) = tryReduce min l
    let tryMax<'a when 'a : comparison> (l : 'a seq) = tryReduce max l
