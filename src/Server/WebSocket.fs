module WebSocket

    open Suave
    open Suave.Sockets
    open Suave.Sockets.Control
    open Suave.WebSocket
    open System.Text
    open System.Linq
    open System.Collections.Generic
    open Newtonsoft.Json

    type ClientState = { product: string option; lastLine: int }

    let PUSH_INTERVAL_MS = 5000

    let emptyState = { product = None; lastLine = 0}

    let connectedWebSockets : Dictionary<WebSocket, ClientState> =
        new Dictionary<WebSocket, ClientState>()

    let ws (webSocket : WebSocket) (_: HttpContext) =
        socket {

            connectedWebSockets.Add(webSocket, emptyState)

            let mutable loop = true
            try

                while loop do

                      let! msg = webSocket.read ()
                      match msg with
                      | (Text, data, true) ->
                          let receivedString = Encoding.UTF8.GetString data

                          // TODO: Some error checking on the data would be nice
                          let productName = receivedString.Substring(receivedString.IndexOf('|') + 1)
                          let oldState = connectedWebSockets.[webSocket]
                          let newState = { oldState with product = Some productName}
                          connectedWebSockets.[webSocket] <- newState

                          // Echo the message back for now, should return success/failure
                          let byteResponse =
                              sprintf "Response to %s" receivedString
                              |> Encoding.ASCII.GetBytes
                              |> ByteSegment

                          do! webSocket.send Text byteResponse true

                      | (Close, _, _) ->
                          loop <- false
                      | (code, _, _) ->
                          printfn "Unhandled ws code: %A" code

                connectedWebSockets.Remove(webSocket) |> ignore
            with ex ->
                printfn "Unhandled exception in WebSocket loop: %s" ex.Message
        }

    let sendNewLogEntries (logPath: string) (ws:WebSocket) (lastReadRow: int) =
        async {

            let (newLines, newLastRead) =
                LogReader.getNewLines logPath lastReadRow

            let parsedLines =
                newLines
                |> Array.map parseLine
                |> Array.filter Option.isSome
                |> Array.map Option.get

            if parsedLines.Length > 0 then
                let message =
                    parsedLines
                    |> Array.rev  // Reverse so we take the last 20
                    |> Array.takeSafe 20
                    |> JsonConvert.SerializeObject

                let! _ = ws.send Text (ByteSegment <| Encoding.UTF8.GetBytes message) true
                ()
            else
                ()

            return newLastRead
        }

    let announcer logBaseDir =
        async {

            while true do
                match connectedWebSockets.Count with
                | 0 -> ()
                | _ ->

                    for client in connectedWebSockets.ToArray() do
                        match client.Value.product with
                        | Some productName ->

                            match LogReader.getLogPathForProduct logBaseDir productName with
                            | Some logPath ->
                                let! newLastRead = sendNewLogEntries logPath client.Key client.Value.lastLine

                                connectedWebSockets.[client.Key] <- {client.Value with lastLine = newLastRead}
                            | None ->
                                // Log file not found, just ignore?
                                ()
                        | None ->
                            // No sub set up, do nothing
                            ()

                System.Threading.Thread.Sleep PUSH_INTERVAL_MS
        }

