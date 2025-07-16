namespace WoofWare.Incremental


[<RequireQualifiedAccess>]
module internal OnlyInDebug =
    let create () =
      {
          CurrentlyRunningNode = None
          ExpertNodesCreatedByCurrentNode = []
      }

    let invariant t =
        ()

