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
        let args = "build" :: param.Context.Arguments @ [ projectRoot ]
        run docker args projectRoot)

Target.create "run" (fun _ -> run dotnet [ "build" ] srcPath)

Target.create "test" (fun _ -> run dotnet [ "run" ] testsPath)

Target.create "format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [ "clean" ==> "buildRelease" ]

[<EntryPoint>]
let main args = runOrDefault args
