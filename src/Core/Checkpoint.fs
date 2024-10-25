module Checkpoint

open System.IO

let readCheckpoint (path: string) =
    File.ReadAllText(path).Trim() |> uint32

let writeCheckpoint (path: string) (id: uint) =
    File.WriteAllText(path, id.ToString())

type ICheckpoint =
    abstract member Read: unit -> uint
    abstract member Write: uint -> unit
    inherit System.IDisposable

type Checkpoint (path: string) =
    let mutable stream: Stream option = None
    member private this.Stream =
        match stream with
        | Some openStream ->
            openStream
        | None ->
            let openStream = File.Open(path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
            stream <- Some openStream
            openStream

    interface ICheckpoint with
        member this.Read () =
            try
                use readStream = File.Open(path, FileMode.OpenOrCreate, FileAccess.Read, FileShare.Read)
                use reader = new StreamReader(readStream)
                let text = reader.ReadToEnd()
                match text.Trim() with
                | "" -> 0u // checkpoint represents last message processed
                | s -> System.UInt32.Parse s
            with _ -> failwith $"Failed to read checkpoint from {path}"

        member this.Write (id: uint) =
            try
                this.Stream.Position <- 0
                let writer = new StreamWriter(this.Stream) // do not dispose, it closes the stream
                writer.AutoFlush <- true
                id |> writer.Write
            with _ -> failwith $"Failed to write checkpoint to {path}"

    interface System.IDisposable with
        member this.Dispose() =
            match stream with
            | Some openStream -> openStream.Dispose()
            | _ -> ()
