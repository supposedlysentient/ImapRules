open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let srcPath = Path.getFullName "src/Sieve"
let deployPath = Path.getFullName "deploy"
let testsPath = Path.getFullName "tests"

Target.create "Clean" (fun _ -> Shell.cleanDir deployPath)

Target.create "Bundle" (fun _ -> run dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] srcPath)

Target.create "Run" (fun _ -> run dotnet [ "build" ] srcPath)

Target.create "RunTests" (fun _ -> run dotnet [ "run" ] testsPath)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

open Fake.Core.TargetOperators

let dependencies = [ "Clean" ==> "Run" ]

[<EntryPoint>]
let main args = runOrDefault args
