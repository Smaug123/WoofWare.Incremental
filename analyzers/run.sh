#!/bin/sh

nix develop --command dotnet build || exit 1
nix develop --command dotnet restore analyzers/analyzers.fsproj || exit 1
nix build .#fsharp-analyzers || exit 1

echo "GITHUB: $GITHUB_WORKSPACE"
echo "PWD: $(pwd)"
ls -la

nix run .#fsharp-analyzers -- --verbosity detailed --analyzers-path ./.analyzerpackages/G-Research.FSharp.Analyzers/*/ --project WoofWare.Incremental/WoofWare.Incremental.fsproj --report gr.sarif
exit_code=$?

nix run .#fsharp-analyzers -- --verbosity detailed --analyzers-path ./.analyzerpackages/WoofWare.FSharpAnalyzers/*/ --project WoofWare.Incremental/WoofWare.Incremental.fsproj --report woofware.sarif
exit_code_2=$?

if [ "$exit_code" -eq 0 ]; then
    if [ "$exit_code_2" -eq 0 ]; then
        exit 0
    else
        exit "$exit_code_2"
    fi
else
    exit "$exit_code"
fi
