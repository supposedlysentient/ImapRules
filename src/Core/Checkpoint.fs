module Checkpoint

open System.IO

type ICheckpoint =
    abstract member Read: unit -> uint
    abstract member Write: uint -> unit
    inherit System.IDisposable

let sanitiseFilename (name: string) =
    if System.String.IsNullOrWhiteSpace name then
        failwith "Filename is empty or whitespace"

    let fileChars = Set " !#$%&()+-.0123456789<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijklmnopqrstuvwxyz{|}~¡¢£¤¥¦µ¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

    System.Text.Encoding.ASCII.GetBytes name
    |> Seq.map char
    |> Seq.map (
        function
        | '\\'
        | '/' -> '_'
        | c when fileChars.Contains c -> c
        | _ -> '?')
    |> Array.ofSeq
    |> System.String

type Checkpoint (basePath: string, name: string, validity: uint) =
    do Directory.CreateDirectory basePath |> ignore
    let name = sanitiseFilename name
    let path = Path.Combine (basePath, $"{name}_{validity}")

    let mutable stream: Stream option = None
    member private this.Stream =
        match stream with
        | Some openStream ->
            openStream
        | None ->
            let openStream = File.Open (path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.Read)
            stream <- Some openStream
            openStream

    interface ICheckpoint with
        member this.Read () =
            try
                use readStream = File.Open (path, FileMode.OpenOrCreate, FileAccess.Read, FileShare.Read)
                use reader = new StreamReader (readStream)
                let text = reader.ReadToEnd ()
                match text.Trim () with
                | "" -> 0u // checkpoint represents last message processed
                | s -> System.UInt32.Parse s
            with _ -> failwith $"Failed to read checkpoint from {path}"

        member this.Write (id: uint) =
            try
                this.Stream.Position <- 0
                let writer = new StreamWriter (this.Stream) // do not dispose, it closes the stream
                writer.AutoFlush <- true
                id |> writer.Write
            with _ -> failwith $"Failed to write checkpoint to {path}"

    interface System.IDisposable with
        member this.Dispose () =
            match stream with
            | Some openStream -> openStream.Dispose ()
            | _ -> ()
