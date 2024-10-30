open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let projectRoot = Path.getFullName "."
let srcPath = Path.getFullName "src/Core"
let deployPath = Path.getFullName "Release"
let testsPath = Path.getFullName "tests"

Target.create "clean" (fun _ -> Shell.cleanDir deployPath)

Target.create "buildRelease" (fun _ -> run dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] srcPath)

Target.create "buildDocker" (
    fun param ->
        let args = param.Context.Arguments
        let version =
            args
            |> List.pairwise
            |> List.fold (fun version args ->
                match args with
                | ("--version", version') -> version'
                | ("--build-arg", version') when version'.StartsWith("VERSION=") ->
                    if version <> "" then version else
                        version'.Replace("VERSION=", "", ignoreCase)
                | _ -> version) ""

        if version <> "" then Docker.validateSemVer version

        let ociLabelArgs = Docker.OciLabels.ofVersion version |> Docker.toDockerArgs
        let dockerArgs = "build" :: args @ ociLabelArgs @ [ projectRoot ]

        run docker dockerArgs projectRoot)

Target.create "run" (fun _ -> run dotnet [ "build" ] srcPath)

Target.create "test" (fun _ -> run dotnet [ "run" ] testsPath)

Target.create "format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [ "clean" ==> "buildRelease" ]

[<EntryPoint>]
let main args = runOrDefault args
