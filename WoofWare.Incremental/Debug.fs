namespace WoofWare.Incremental

/// Module letting you put Incremental into a "debug mode" with more checks.
[<RequireQualifiedAccess>]
module Debug =
    /// Set this to `true` to cause Incremental to run many consistency checks during execution.
    let mutable globalFlag = false
