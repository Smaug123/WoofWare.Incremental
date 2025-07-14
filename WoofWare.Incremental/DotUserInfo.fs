namespace WoofWare.PlayFetch

open System.Text

type internal Dot =
    {
        Label : Set<string list>
        Attributes : Map<string, string>
    }

type internal DotUserInfo =
    | Dot of Dot
    | Info of string
    | Append of prior : DotUserInfo * next : DotUserInfo

[<RequireQualifiedAccess>]
module internal DotUserInfo =
    let info i = DotUserInfo.Info i

    let dot label attrs =
        {
            Label = Set.singleton label
            Attributes = attrs
        }
        |> DotUserInfo.Dot

    let append prior next = DotUserInfo.Append (prior, next)

    let rec toDot (dui : DotUserInfo) =
        match dui with
        | DotUserInfo.Dot dot -> dot
        | DotUserInfo.Append (prior, next) ->
            let prior = toDot prior
            let next = toDot next
            let label = Set.union prior.Label next.Label
            let attrs = Map.merge prior.Attributes next.Attributes (fun _ _ r -> r)

            {
                Label = label
                Attributes = attrs
            }
        | DotUserInfo.Info i ->
            {
                Label = Set.singleton [ i ]
                Attributes = Map.empty
            }

    let escapeDotString (s : string) : string =
        // https://graphviz.org/doc/info/lang.html
        let result = StringBuilder ()
        result.Append '"' |> ignore<StringBuilder>
        result.Append (s.Replace ("\"", "\\\"")) |> ignore<StringBuilder>
        result.Append '"' |> ignore<StringBuilder>
        result.ToString ()

    let escapeRecordLabel (s : string) : string =
        // https://graphviz.org/doc/info/shapes.html
        let result = StringBuilder ()

        for c in s do
            match c with
            | '<'
            | '>'
            | '{'
            | '}'
            | '|'
            | '\\'
            | ' ' -> result.Append '\\' |> ignore<StringBuilder>
            | _ -> ()

            result.Append c |> ignore<StringBuilder>

        result.ToString ()

    let toString (shape : string) (name : string) (dot : Dot) : string =
        let label =
            dot.Label
            |> Seq.map (fun cols -> "{" + String.concat "|" (Seq.map escapeRecordLabel cols) + "}")
            |> String.concat "|"
            |> fun s -> "{" + s + "}"

        let attributes =
            dot.Attributes
            |> Seq.map (fun (KeyValue (k, v)) -> $" %s{escapeDotString k}=%s{escapeDotString v}")
            |> String.concat " "

        $"  %s{name} [shape=%s{shape} label=%s{escapeDotString label} %s{attributes}]"
