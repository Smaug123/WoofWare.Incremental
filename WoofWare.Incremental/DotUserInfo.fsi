namespace WoofWare.PlayFetch

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
    val info : string -> DotUserInfo
    val dot : label : string list -> attrs : Map<string, string> -> DotUserInfo
    val toDot : DotUserInfo -> Dot
    val append : DotUserInfo -> DotUserInfo -> DotUserInfo
    val toString : shape : string -> name : string -> Dot -> string
