// Learn more about F# at http://fsharp.org

open System
open NuGet.Versioning
open ServerNuGet

[<EntryPoint>]
let main argv =
    async {
        let package = {
            Id = "Cake.Core"
            Version = NuGetVersion("1.0.0")
            Framework = "net5.0"
        }

        let! graph = package |> getGraph

        printfn "Hello World from F#!"
        return 0 // return an integer exit code
    } |> Async.RunSynchronously
