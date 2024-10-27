module Mocks

open System.Collections.Generic
open System.Threading
open FSharp.Reflection
open MimeKit
open MailKit
open MailKit.Net.Imap
open Config
open Checkpoint

let config = {
    server = "mail.example.com"
    port = 143
    sslOptions = SslOptions.None
    username = "mockuser"
    password = "mockpass"
    rulePath = []
    checkpointPath = ""
}

let UTF8 = System.Text.Encoding.UTF8

type MockAddressList = string seq

type MockMessageData = {
    // Envelope
    Bcc: MockAddressList
    Cc: MockAddressList
    Date: System.DateTimeOffset option
    From: MockAddressList
    InReplyTo: string
    MessageId: string
    ReplyTo: MockAddressList
    Sender: MockAddressList
    Subject: string
    To: MockAddressList

    // MessageSummary
    Headers: Map<string, string>
    Size: uint32 option
} with

    static member Default = {
        Bcc = []
        Cc = []
        Date = None
        From = []
        InReplyTo = ""
        MessageId = ""
        ReplyTo = []
        Sender = []
        Subject = ""
        To = []

        Headers = Map []
        Size = None
    }

let asAddressList (addresses: string seq) =
    InternetAddressList [|
        for a in addresses do
            MailboxAddress ("blah blah", a)
    |]

type MockEnvelope (data: MockMessageData) as this =
    inherit Envelope ()

    let fieldMap =
        Map [
            for field in FSharpType.GetRecordFields (typeof<MockMessageData>) do
                (field.Name, field.GetValue data)
        ]

    let props = typeof<Envelope>.GetProperties ()

    do
        for prop in props do
            let value = fieldMap[prop.Name]

            match prop.PropertyType with
            | T when T = typeof<InternetAddressList> -> prop.SetValue (this, asAddressList (downcast value))
            | T when typeof<System.Nullable>.IsAssignableFrom T ->
                let ctor = T.GetConstructors () |> Seq.head // Nullable<T> only has one ctor

                match downcast value with
                | Some v -> prop.SetValue (this, ctor.Invoke [| v |])
                | _ -> prop.SetValue (this, ctor.Invoke [| null |])
            | _ -> prop.SetValue (this, value)

    interface System.IEquatable<Envelope> with
        member this.Equals other =
            props
            |> Array.fold (fun (isEqual: bool) p -> isEqual && (p.GetValue this = p.GetValue other)) true

    override this.Equals other =
        match other with
        | :? Envelope as o -> (this :> System.IEquatable<_>).Equals o
        | _ -> false

    override this.GetHashCode () = (this :> Envelope).GetHashCode ()

    override this.ToString () =
        let format (al: InternetAddressList) =
            [
                for a in al do
                    (a :?> MailboxAddress).Address
            ]
            |> String.concat ";"

        $"{{From: {format this.From}; To: {format this.To}; Subject: {this.Subject}}}"

type MockMessageSummary (data: MockMessageData) as this =
    inherit MessageSummary (0)
    let headers = HeaderList ()

    do
        for h in data.Headers do
            headers.Add (Header (UTF8, h.Key, h.Value))

    let size =
        match data.Size with
        | None -> System.Nullable<uint32>()
        | Some u -> System.Nullable<uint32> u

    do
        typeof<MessageSummary>
            .GetProperty("Envelope")
            .SetValue (this, MockEnvelope (data))

    do typeof<MessageSummary>.GetProperty("Headers").SetValue (this, headers)
    do typeof<MessageSummary>.GetProperty("Size").SetValue (this, size)

    interface System.IEquatable<IMessageSummary> with
        member this.Equals other = this.Envelope = other.Envelope // TODO

    override this.Equals other =
        match other with
        | :? IMessageSummary as o -> (this :> System.IEquatable<_>).Equals o
        | _ -> false

    override this.GetHashCode () = (this :> IMessageSummary).GetHashCode ()

    override this.ToString () = this.Envelope.ToString ()

let folderArgs =
    typeof<ImapFolderConstructorArgs>
        .GetConstructor(
            System.Reflection.BindingFlags.NonPublic
            ||| System.Reflection.BindingFlags.Instance,
            [||]
        )
        .Invoke [||]
    :?> ImapFolderConstructorArgs

let emptyMsg = MockMessageSummary MockMessageData.Default

type MockFolder () =
    inherit ImapFolder (folderArgs)
    let mutable msgs: IMessageSummary seq = []
    let mutable fetchCallCount = 0
    interface IMailFolder

    override this.Fetch (min: int, max: int, request: IFetchRequest, ct: CancellationToken) =
        fetchCallCount <- fetchCallCount + 1
        List<IMessageSummary>(msgs |> Seq.skip min |> Seq.truncate (1 + max - min))

    override this.Open (access: FolderAccess, ct: CancellationToken) = access
    member this.FetchCallCount = fetchCallCount

    member this.Messages
        with get () = msgs
        and set (m) = msgs <- m :> IMessageSummary seq

type MockClient () =
    inherit ImapClient ()
    let mutable connectCallCount = 0
    let mutable authCallCount = 0
    let inbox = MockFolder ()
    override this.Inbox = inbox

    override this.Connect (host: string, port: int, options: SslOptions, ct: CancellationToken) =
        connectCallCount <- connectCallCount + 1

    override this.Authenticate (encoding: System.Text.Encoding, creds: System.Net.ICredentials, ct: CancellationToken) =
        authCallCount <- authCallCount + 1

    member this.ConnectCallCount = connectCallCount
    member this.AuthCallCount = authCallCount

type MockCheckpoint () =
    interface ICheckpoint with
        member this.Read () = 0u
        member this.Write id = ()
        member this.Dispose () = ()
