module LogViewer

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System.IO
open Suave.WebSocket
open System.Text
open Suave.Json
open Suave.RequestErrors
open System.Net

[<EntryPoint>]
let main argv =

    let logFolder =
        if argv.Length = 1 then
            argv.[0]
        else
            printfn "No log directory specified"
            System.Environment.Exit -1
            ""

    let getProductNames: WebPart =
        fun (x: HttpContext) ->
            async {
                let response =
                    LogReader.getProductNames logFolder
                    |> toJson
                    |> Encoding.ASCII.GetString

                return! OK response x
          }

    let getLogs (productName: string): WebPart =
        fun (x: HttpContext) ->
          async {

              let start =
                match x.request.queryParam "start" with
                  | Choice1Of2 startStr ->
                        match System.Int32.TryParse startStr with
                        | true, x -> x
                        | _ -> -1 // Error silently ignored, start at first item
                  | Choice2Of2 _ ->
                    -1 // No start specified, start at first item

              let limit =
                match x.request.queryParam "limit" with
                  | Choice1Of2 limitStr ->
                        match System.Int32.TryParse limitStr with
                        | true, x -> x
                        | _ -> -1 // Errors silently ignored, no limit
                  | Choice2Of2 _ ->
                    -1 // No limit specified, return all

              let skipStart (items: 'a[]) =
                if start = -1 then
                    items // Skipping nothing
                else if start > items.Length then
                    Array.empty // Start past the end, give nothing back. TODO: BAD_REQUEST
                else
                    Array.skip (System.Math.Min(start, items.Length)) items

              let takeLimit items =
                if limit = 0 then
                    Array.empty
                else if limit > -1 then
                    Array.takeSafe limit items
                else
                    items

              match LogReader.getLogPathForProduct logFolder productName with
              | Some productLogPath ->
                  let logEntries =
                    LogReader.getNewLines productLogPath 0
                    |> fst
                    |> Array.map parseLine
                    |> Array.filter Option.isSome
                    |> Array.map Option.get
                    |> skipStart
                    |> takeLimit
                    |> Newtonsoft.Json.JsonConvert.SerializeObject

                  return! OK logEntries x
              | None ->
                return! NOT_FOUND "Invalid Product Name" x
          }

    let app =
      choose
        [ GET >=> choose
            [ path "/" >=> (Files.browseFileHome "index.html")
              pathScan "/page/%s" (fun _ -> (Files.browseFileHome "index.html")) // SPA-esq navigation
              path "/products" >=> getProductNames
              pathScan "/logs/%s" getLogs
              path "/ws" >=> handShake WebSocket.ws
              Files.browseHome ]
        ]

    Async.Start <| WebSocket.announcer logFolder

    let config = { defaultConfig with homeFolder = Some <| Path.GetFullPath "./public"; bindings = [ HttpBinding.create HTTP IPAddress.Any 8080us] }

    startWebServer config app

    0 // return an integer exit code
