module EventHack exposing (fetchEvents, withBlockrange)

import Eth
import Eth.Decode
import Eth.Types exposing (Address)
import Http
import Json.Decode exposing (Decoder)
import Task


fetchEvents :
    Eth.Types.HttpProvider
    -> Address
    -> (Address -> Eth.Types.LogFilter)
    -> ( Eth.Types.BlockId, Eth.Types.BlockId )
    -> Decoder eventType
    -> (Result Http.Error (List (Eth.Types.Event eventType)) -> msg)
    -> Cmd msg
fetchEvents httpProvider contractAddress lfMaker blockrange eventDecoder msgConstructor =
    let
        logFilter =
            lfMaker contractAddress
                |> withBlockrange blockrange
    in
    Eth.getDecodedLogs
        httpProvider
        logFilter
        (Eth.Decode.event eventDecoder)
        |> Task.attempt msgConstructor


withBlockrange : ( Eth.Types.BlockId, Eth.Types.BlockId ) -> Eth.Types.LogFilter -> Eth.Types.LogFilter
withBlockrange ( fromBlock, toBlock ) lf =
    { lf
        | fromBlock = fromBlock
        , toBlock = toBlock
    }
