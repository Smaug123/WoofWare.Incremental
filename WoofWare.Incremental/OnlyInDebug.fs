namespace WoofWare.Incremental


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal OnlyInDebug =
    let create () =
      {
          CurrentlyRunningNode = None
          ExpertNodesCreatedByCurrentNode = []
      }

    let invariant (t : OnlyInDebug) : unit =
        ()

