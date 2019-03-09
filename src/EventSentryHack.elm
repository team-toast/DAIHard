module EventSentryHack exposing (EventSentry, Msg(..), init, pollForChanges, update)

import Eth
import Eth.Decode
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder)
import Task


type alias EventSentry eventType msg =
    { httpProvider : Eth.Types.HttpProvider
    , contractAddress : Address
    , eventDecoder : Decoder eventType
    , msgConstructor : Result Http.Error (List (Eth.Types.Event eventType)) -> msg
    , nextBlockToScan : Int
    , tagger : Msg -> msg
    }


type Msg
    = NoOp
    | LatestBlocknumFetchResult (Result Http.Error Int)


init :
    Eth.Types.HttpProvider
    -> Address
    -> Decoder eventType
    -> (Result Http.Error (List (Eth.Types.Event eventType)) -> msg)
    -> Int
    -> (Msg -> msg)
    -> EventSentry eventType msg
init httpProvider contractAddress eventDecoder msgConstructor startBlock tagger =
    { httpProvider = httpProvider
    , contractAddress = contractAddress
    , eventDecoder = eventDecoder
    , msgConstructor = msgConstructor
    , nextBlockToScan = startBlock
    , tagger = tagger
    }


update : Msg -> EventSentry eventType msg -> ( EventSentry eventType msg, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LatestBlocknumFetchResult result ->
            case result of
                Ok latestBlocknum ->
                    if latestBlocknum >= model.nextBlockToScan then
                        let
                            cmd =
                                fetchEventsCmd
                                    model.httpProvider
                                    model.contractAddress
                                    ( Eth.Types.BlockNum model.nextBlockToScan, Eth.Types.BlockNum latestBlocknum )
                                    model.eventDecoder
                                    model.msgConstructor

                            newModel =
                                { model | nextBlockToScan = latestBlocknum + 1 }
                        in
                        ( newModel, cmd )

                    else
                        ( model, Cmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "Error fetching blocknum via EventSentryHack" errstr
                    in
                    ( model, Cmd.none )


pollForChanges : EventSentry eventType msg -> Cmd msg
pollForChanges sentry =
    fetchLatestBlockCmd sentry.httpProvider LatestBlocknumFetchResult
        |> Cmd.map sentry.tagger


fetchLatestBlockCmd : Eth.Types.HttpProvider -> (Result Http.Error Int -> Msg) -> Cmd Msg
fetchLatestBlockCmd httpProvider msgConstructor =
    Eth.getBlockNumber httpProvider
        |> Task.attempt msgConstructor


fetchEventsCmd :
    Eth.Types.HttpProvider
    -> Address
    -> ( Eth.Types.BlockId, Eth.Types.BlockId )
    -> Decoder eventType
    -> (Result Http.Error (List (Eth.Types.Event eventType)) -> msg)
    -> Cmd msg
fetchEventsCmd httpProvider contractAddress ( fromBlock, toBlock ) eventDecoder msgConstructor =
    let
        logFilter =
            { address = contractAddress
            , fromBlock = fromBlock
            , toBlock = toBlock
            , topics = []
            }
    in
    Eth.getDecodedLogs
        httpProvider
        logFilter
        (Eth.Decode.event eventDecoder)
        |> Task.attempt msgConstructor
