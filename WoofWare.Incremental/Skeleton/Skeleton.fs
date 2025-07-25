namespace WoofWare.Incremental.Skeleton

open System.Collections.Generic
open WoofWare.Incremental

// module Node_id = Incremental.For_analyzer.Node_id
// module Dot_user_info = Incremental.For_analyzer.Dot_user_info

type Skeleton =
    {
        Nodes : Node IReadOnlyList
        Seen : NodeId IReadOnlySet
        NumStabilizes : int
    }

    override this.ToString () : string =
        let nodes = this.Nodes |> Seq.map string<Node> |> String.concat "\n"
        let seen = this.Seen |> Seq.sort |> Seq.map NodeId.toString |> String.concat " "

        $"nodes:\n%s{nodes}\nseen: (%s{seen})\nnumStabilizes:%i{this.NumStabilizes}"

type RenderTarget =
    | Dot
    | GraphEasy

[<RequireQualifiedAccess>]
module Skeleton =
    let normalizeIds (skeleton : Skeleton) =
        let nodes = skeleton.Nodes

        if nodes.Count = 0 then
            skeleton
        else
            let hd = nodes.[0]

            let lowestId =
                nodes
                |> Seq.map (fun node -> NodeId.toInt node.Id)
                |> Seq.tryMin
                |> Option.defaultValue (NodeId.toInt hd.Id)

            let newId nodeId =
                // Nodes are one-indexed
                NodeId.toInt nodeId - lowestId + 1 |> NodeId.ofInt

            let nodes =
                nodes
                |> Seq.map (fun node ->
                    let id = newId node.Id
                    let children = Seq.map newId node.Children |> ResizeArray
                    let bindChildren = Seq.map newId node.BindChildren |> ResizeArray

                    { node with
                        Id = id
                        Children = children
                        BindChildren = bindChildren
                    }
                )
                |> ResizeArray

            let seen = Seq.map newId skeleton.Seen |> HashSet

            { skeleton with
                Nodes = nodes
                Seen = seen
            }

    let snapshot (normalize : bool option) state =
        let normalize = defaultArg normalize false
        let seen = HashSet ()
        let nodes = ResizeArray ()

        let addNode id kind cutoff children bindChildren userInfo recomputedAt changedAt height =
            seen.Add id |> ignore<bool>

            let newNode =
                {
                    WoofWare.Incremental.Skeleton.Node.Id = id
                    Kind = kind
                    Cutoff = cutoff
                    Children = children
                    BindChildren = bindChildren
                    UserInfo = userInfo
                    RecomputedAt = recomputedAt
                    ChangedAt = changedAt
                    Height = height
                }

            nodes.Add newNode

        ForAnalyzer.traverse (State.directlyObserved state) addNode

        let skeleton =
            {
                Skeleton.Nodes = nodes
                Seen = seen
                NumStabilizes = State.numStabilizes state
            }

        if normalize then normalizeIds skeleton else skeleton

    let nodeName (nodeId : NodeId) : string = "n" + NodeId.toString nodeId

    let makeNode (node : Node) extraAttrs (renderTarget : RenderTarget) =
        let name = nodeName node.Id

        let baseNodeDot =
            DotUserInfo.dot
                [ name ; (ForAnalyzer.Kind.toString node.Kind) ; $"height=%d{node.Height}" ]
                (match renderTarget with
                 | RenderTarget.Dot -> Map.ofList [ "fontname", "Sans Serif" ]
                 | RenderTarget.GraphEasy -> Map.empty)

        let info =
            let nodeInfo =
                match node.UserInfo with
                | None -> baseNodeDot
                | Some userInfo -> DotUserInfo.append baseNodeDot userInfo

            let attrsOpt = extraAttrs node

            match attrsOpt with
            | None -> nodeInfo
            | Some attrs -> DotUserInfo.append nodeInfo attrs

        let shape =
            match renderTarget with
            | RenderTarget.Dot -> "Mrecord"
            | RenderTarget.GraphEasy -> "box"

        DotUserInfo.toString shape name (DotUserInfo.toDot info)

    /// The parameters' names reflect the ordering of these nodes in the [Incr] graph where
    /// the children of a node are the inputs (e.g. a Var would be the child of a Map), but it
    /// seems more intuitive to visualize it in the opposite direction
    let edge from to_ =
        $"%s{nodeName to_} -> %s{nodeName from}"

    let makeEdges (nodes : Node list) desiredNodes =
        nodes
        |> Seq.collect (fun fromNode ->
            let fromNodeId = fromNode.Id

            fromNode.Children
            |> Seq.choose (fun toNodeId ->
                if Set.contains toNodeId desiredNodes then
                    Some (edge fromNodeId toNodeId)
                else
                    None
            )
        )
        |> Seq.toList

    let bindEdge from to_ = ""
    // Text_block.textf {|%s -> %s [style=dashed]|} (node_name to_) (node_name from)

    /// Note that the direction of information flow is flipped in bind nodes as compared to
    /// regular child nodes
    let makeBindEdges (nodes : Node list) desiredNodes (seen : IReadOnlySet<NodeId>) =
        nodes
        |> Seq.collect (fun toNode ->
            let toNodeId = toNode.Id

            toNode.BindChildren
            |> Seq.choose (fun fromNodeId ->
                if Set.contains fromNodeId desiredNodes && seen.Contains fromNodeId then
                    bindEdge fromNodeId toNodeId |> Some
                else
                    None
            )
        )
        |> Seq.toList

    let findConnectedNodes (startNodes : NodeId list) (edges : Map<NodeId, NodeId list>) =
        let rec recurseNode (nodeId : NodeId) (seen : Set<NodeId>) =
            if Set.contains nodeId seen then
                seen
            else
                let seen = Set.add nodeId seen
                let nodeIds = Map.tryFind nodeId edges |> Option.defaultValue []
                (seen, nodeIds) ||> List.fold (fun seen childId -> recurseNode childId seen)

        (Set.empty, startNodes)
        ||> List.fold (fun seen nodeId -> recurseNode nodeId seen)

    let toDot
        (extraAttrs : (Node -> DotUserInfo option) option)
        (renderTarget : RenderTarget option)
        (filteredNodes : Node list option)
        (renderRelation : RenderRelation option)
        (s : Skeleton)
        =
        let extraAttrs = extraAttrs |> Option.defaultValue (fun _ -> None)
        let renderTarget = renderTarget |> Option.defaultValue RenderTarget.Dot
        let filteredNodes = filteredNodes |> Option.defaultValue []
        let renderRelation = renderRelation |> Option.defaultValue RenderRelation.All
        let nodes = s.Nodes
        let seen = s.Seen

        let desiredNodes =
            let childEdges =
                lazy
                    (Map.empty, nodes)
                    ||> Seq.fold (fun childEdges node ->
                        let addedBind =
                            (childEdges, node.BindChildren)
                            ||> Seq.fold (fun edges bindId -> Map.addMulti bindId node.Id edges)

                        (addedBind, node.Children)
                        ||> Seq.fold (fun edges childId -> Map.addMulti node.Id childId edges)
                    )

            let parentEdges =
                lazy
                    (Map.empty, childEdges.Force ())
                    ||> Map.fold (fun parentEdges nodeId children ->
                        (parentEdges, children)
                        ||> List.fold (fun parentEdges childId -> Map.addMulti childId nodeId parentEdges)
                    )

            let findConnectedNodes =
                findConnectedNodes (filteredNodes |> List.map (fun node -> node.Id))

            match renderRelation with
            | RenderRelation.Ancestors -> findConnectedNodes (childEdges.Force ())
            | RenderRelation.Descendants -> findConnectedNodes (parentEdges.Force ())
            | RenderRelation.Both ->
                Set.union (findConnectedNodes (parentEdges.Force ())) (findConnectedNodes (childEdges.Force ()))
            | RenderRelation.All -> nodes |> Seq.map (fun node -> node.Id) |> Set.ofSeq

        let desiredNodeList =
            nodes
            |> Seq.filter (fun node -> Set.contains node.Id desiredNodes)
            |> Seq.toList

        let edges = makeEdges desiredNodeList desiredNodes
        let bindEdges = makeBindEdges desiredNodeList desiredNodes seen

        let nodes =
            desiredNodeList |> List.map (fun node -> makeNode node extraAttrs renderTarget)

        [
            "digraph G {"
            "  rankdir = TB"
            "  bgcolor = transparent"
            yield! (nodes @ edges @ bindEdges) |> Seq.map (fun s -> "  " + s)
            "}"
        ]
        |> String.concat "\n"
