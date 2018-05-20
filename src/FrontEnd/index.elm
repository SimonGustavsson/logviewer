module Main exposing (..)

import Html exposing (Html, div, span, Attribute, pre, text, h1, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, style, class)
import Http exposing (..)
import Json.Decode exposing (..)
import WebSocket exposing (..)
import List.Extra exposing (minimumBy)
import Dom.Scroll
import Task
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import RemoteData exposing (WebData, RemoteData(NotAsked, Loading, Failure, Success))
import Json.Decode exposing (Decoder, int, string, map3)


type alias LogEntry =
    { id : Int
    , type_ : String
    , date : String
    , message : String
    }


type Route
    = Home
    | ProductPage (Maybe String)


type alias Model =
    { logEntries : WebData (List LogEntry)
    , productName : Maybe String
    , productNames : WebData (List String)
    , history : List (Maybe Route)
    }


type Msg
    = NewMessage String
    | UrlChanged Navigation.Location
    | NewUrl String
    | UpdateProductNames (WebData (List String))
    | NoOp
    | LogsReceived (WebData (List String))
    | LoadOlder
    | LogEntriesReceived (WebData (List LogEntry))


main : Program Never Model Msg
main =
    Navigation.program UrlChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> WebSocket.listen "ws://127.0.0.1:8080/ws" NewMessage
        }


getHistoryCmd : String -> Cmd Msg
getHistoryCmd productName =
    Http.get ("/logs/" ++ productName) (list string)
        |> RemoteData.sendRequest
        |> Cmd.map LogsReceived


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        getProductNamesCmd =
            Http.get "/products" (list string)
                |> RemoteData.sendRequest
                |> Cmd.map UpdateProductNames

        emptyModel =
            { logEntries = RemoteData.NotAsked
            , productName = Nothing
            , productNames = RemoteData.Loading
            , history = [ Url.parsePath route location ]
            }

        ( modelWithRoute, routeCmd ) =
            handleLocationChange emptyModel location
    in
        ( modelWithRoute
        , Cmd.batch [ getProductNamesCmd, routeCmd ]
        )


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map ProductPage (s "page" <?> stringParam "product")
        ]


handleLocationChange : Model -> Navigation.Location -> ( Model, Cmd Msg )
handleLocationChange model location =
    case Url.parsePath route location of
        Just validRoute ->
            let
                modelWithHistory =
                    { model | history = Just validRoute :: model.history }
            in
                case validRoute of
                    ProductPage (Just productName) ->
                        let
                            newModel =
                                { modelWithHistory | productName = Just productName, logEntries = RemoteData.Loading }

                            subCmd =
                                WebSocket.send "ws://127.0.0.1:8080/ws" ("subscribe|" ++ productName)
                        in
                            ( newModel, subCmd )

                    ProductPage Nothing ->
                        ( { modelWithHistory | productName = Nothing }, Cmd.none )

                    _ ->
                        ( modelWithHistory, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


logEntryDecoder : Decoder LogEntry
logEntryDecoder =
    map4 LogEntry
        (field "id" Json.Decode.int)
        (field "type_" string)
        (field "date" string)
        (field "message" string)


parseLogEntry : String -> Maybe LogEntry
parseLogEntry str =
    let
        decoderResult =
            decodeString logEntryDecoder str
    in
        case decoderResult of
            Ok stuff ->
                Just stuff

            Err e ->
                let
                    _ =
                        Debug.log e "Foo? "
                in
                    Nothing


parseEntries : List String -> List LogEntry
parseEntries stringMessages =
    List.map parseLogEntry stringMessages
        |> List.filter
            (\e ->
                case e of
                    Just _ ->
                        True

                    Nothing ->
                        False
            )
        |> List.filterMap identity


addEntriesToLog : WebData (List LogEntry) -> List LogEntry -> WebData (List LogEntry)
addEntriesToLog oldData newEntries =
    case oldData of
        Success _ ->
            RemoteData.map
                (\oldEntries ->
                    (oldEntries ++ newEntries) |> List.sortBy .id
                )
                oldData

        _ ->
            -- No old entries
            RemoteData.succeed (newEntries |> List.sortBy .id)


addNewEntries : Model -> List LogEntry -> Model
addNewEntries model unparsedMessages =
    { model | logEntries = addEntriesToLog model.logEntries (unparsedMessages) }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )

        LoadOlder ->
            let
                lowestIdOrZero =
                    case model.logEntries of
                        Success ez ->
                            case minimumBy .id ez of
                                Just minIdEntry ->
                                    minIdEntry.id

                                Nothing ->
                                    0

                        _ ->
                            0

                loadCmd =
                    case model.productName of
                        Just activeProductName ->
                            Http.get ("/logs/" ++ activeProductName ++ "?start=" ++ toString (max 0 (lowestIdOrZero - 20)) ++ "&limit=20") (list logEntryDecoder)
                                |> RemoteData.sendRequest
                                |> Cmd.map LogEntriesReceived

                        Nothing ->
                            Cmd.none
            in
                ( model, loadCmd )

        LogEntriesReceived webData ->
            case webData of
                Success data ->
                    ( addNewEntries model data, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UrlChanged location ->
            handleLocationChange model location

        NewMessage m ->
            case decodeString (list logEntryDecoder) m of
                Ok messages ->
                    ( addNewEntries model messages
                    , Dom.Scroll.toBottom "history" |> Task.attempt (always NoOp)
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdateProductNames response ->
            let
                activateSingleProductCmd =
                    case response of
                        Success [ singleName ] ->
                            Navigation.newUrl ("/page/?product=" ++ singleName)

                        _ ->
                            Cmd.none
            in
                ( { model | productNames = response }, activateSingleProductCmd )

        LogsReceived response ->
            let
                entries =
                    RemoteData.map
                        (\strEntries ->
                            parseEntries strEntries |> List.sortBy .id
                        )
                        response
            in
                ( { model | logEntries = entries }, Cmd.none )


getEntryTypeName : LogEntry -> String
getEntryTypeName entry =
    if String.contains "[ERROR]" entry.type_ then
        "error"
    else if String.contains "[WARNING]" entry.type_ then
        "warning"
    else
        "info"


hasAnyProduct : Model -> Bool
hasAnyProduct model =
    case model.productNames of
        Success productNames ->
            List.any (\_ -> True) productNames

        _ ->
            False


viewEntries : Model -> List (Html Msg)
viewEntries model =
    case model.logEntries of
        NotAsked ->
            -- Note: This state isn't used for log entries as we use WebSockets
            [ span [ style [ ( "color", "white" ) ] ] [ text "Please select a product." ] ]

        Loading ->
            [ span [ class "loadingLabel" ] [ text "Loading" ] ]

        Failure err ->
            [ span [ style [ ( "color", "white" ) ] ] [ text ("Failed to load entries. " ++ toString err) ] ]

        Success allEntries ->
            case allEntries of
                [] ->
                    case model.productName of
                        Just _ ->
                            [ text "Log contains no entries.." ]

                        Nothing ->
                            if hasAnyProduct model then
                                [ text "No product selected" ]
                            else
                                [ text "" ]

                entries ->
                    entries
                        |> List.map
                            (\e ->
                                div [ class "entry" ]
                                    [ pre [ class (getEntryTypeName e) ] [ text (String.padLeft 9 ' ' e.type_) ]
                                    , pre [ class "entryDate" ] [ text e.date ]
                                    , pre
                                        [ class ((getEntryTypeName e) ++ " logMessage") ]
                                        [ text (" [" ++ ((toString e.id) ++ "] " ++ e.message)) ]
                                    ]
                            )


viewProductNames : Model -> List (Html Msg)
viewProductNames model =
    let
        isActiveProduct productName =
            case model.productName of
                Nothing ->
                    False

                Just activeName ->
                    productName == activeName

        productClassName productName =
            case isActiveProduct productName of
                True ->
                    "product activeProduct"

                False ->
                    "product"
    in
        case model.productNames of
            NotAsked ->
                [ text "Initializing..." ]

            Loading ->
                [ span [ class "loadingLabel" ] [ text "Loading" ] ]

            Failure err ->
                [ text ("Unable to load product names. " ++ toString err) ]

            Success productNames ->
                productNames |> List.map (\pn -> span [ class (productClassName pn), onClick (NewUrl ("/page/?product=" ++ pn)) ] [ text pn ])


view : Model -> Html Msg
view model =
    let
        productNames =
            viewProductNames model

        entries =
            viewEntries model

        showLoadOlder =
            case model.logEntries of
                Success unwrappedEntries ->
                    not <| List.any (\e -> e.id == 0) unwrappedEntries

                _ ->
                    False

        loadOlderLink =
            if showLoadOlder then
                span [ class "loadOlder", style [ ( "color", "white" ) ], onClick LoadOlder ] [ text "Load older" ]
            else
                text ""
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "height", "100%" ) ] ]
            [ div [ style [ ( "color", "#eee" ), ( "text-align", "center" ), ( "background", "#111" ), ( "padding", "3px 0" ) ] ]
                (List.append [ (span [] [ text "Products:" ]) ] productNames)
            , div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-grow", "1" )
                    , ( "height", "100%" )
                    , ( "max-height", "100%" )
                    , ( "justify-content", "flex-end" )
                    , ( "flex-direction", "column" )
                    ]
                ]
                [ loadOlderLink
                , div
                    [ id "history", style [ ( "overflow-y", "auto" ) ] ]
                    entries
                ]
            ]
