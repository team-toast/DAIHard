module Contracts.Wrappers exposing (getCreationInfoFromIdCmd, getInitiatedEventDataSentryCmd, getNumTradesCmd, getParametersAndStateCmd, getParametersCmd, getParametersStateAndPhaseInfoCmd, getStateCmd, openTrade)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.DAIHardNativeFactory as DHF
import Contracts.Generated.DAIHardNativeTrade as DHT
import Contracts.Types exposing (..)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address, Call)
import Eth.Utils
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Http
import Json.Decode
import Json.Encode
import PaymentMethods
import Task
import Time
import TokenValue exposing (TokenValue)


openTrade : Network -> CreateParameters -> Call Address
openTrade network parameters =
    DHF.createOpenTrade
        (Config.factoryAddress network)
        parameters.initiatorAddress
        Config.devFeeAddress
        (parameters.initiatingParty == Seller)
        (TokenValue.getEvmValue parameters.tradeAmount)
        (TokenValue.getEvmValue <| defaultBuyerDeposit parameters.tradeAmount)
        (TokenValue.getEvmValue <| defaultAbortPunishment parameters.tradeAmount)
        (TokenValue.getEvmValue parameters.pokeReward)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoabortInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        (TokenValue.getEvmValue <| getDevFee parameters.tradeAmount)
        (encodeTerms <| Terms parameters.price parameters.paymentMethods)
        parameters.initiatorCommPubkey
        |> EthHelpers.updateCallValue
            (TokenValue.getEvmValue <| calculateFullInitialDeposit parameters)


getNumTradesCmd : EthHelpers.EthNode -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTradesCmd ethNode msgConstructor =
    Eth.call ethNode.http (DHF.numTrades (Config.factoryAddress ethNode.network))
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : EthHelpers.EthNode -> BigInt -> (Result Http.Error DHF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd ethNode ttId msgConstructor =
    Eth.call ethNode.http (DHF.createdTrades (Config.factoryAddress ethNode.network) ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd ethNode address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd ethNode address parametersMsgConstructor
        , getStateCmd ethNode address stateMsgConstructor
        ]


getParametersStateAndPhaseInfoCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getParametersStateAndPhaseInfoCmd ethNode address parametersMsgConstructor stateMsgConstructor phaseStartInfoConstructor =
    Cmd.batch
        [ getParametersCmd ethNode address parametersMsgConstructor
        , getStateCmd ethNode address stateMsgConstructor
        , getPhaseStartInfoCmd ethNode address phaseStartInfoConstructor
        ]


getParametersCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> Cmd msg
getParametersCmd ethNode ttAddress msgConstructor =
    Eth.call ethNode.http (DHT.getParameters ttAddress)
        |> Task.map decodeParameters
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode ttAddress msgConstructor =
    Eth.call ethNode.http (DHT.getState ttAddress)
        |> Task.map decodeState
        |> Task.attempt msgConstructor


getPhaseStartInfoCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getPhaseStartInfoCmd ethNode ttAddress msgConstructor =
    Eth.call ethNode.http (DHT.getPhaseStartInfo ttAddress)
        |> Task.map decodePhaseStartInfo
        |> Task.attempt msgConstructor


getInitiatedEventDataSentryCmd : EventSentry msg -> TradeCreationInfo -> (Result Json.Decode.Error DHT.Initiated -> msg) -> ( EventSentry msg, Cmd msg )
getInitiatedEventDataSentryCmd eventSentry creationInfo msgConstructor =
    let
        logToMsg : Eth.Types.Log -> msg
        logToMsg log =
            (Eth.Decode.event DHT.initiatedDecoder log).returnData
                |> msgConstructor

        logFilter =
            { fromBlock = Eth.Types.BlockNum creationInfo.blocknum
            , toBlock = Eth.Types.BlockNum creationInfo.blocknum
            , address = creationInfo.address
            , topics = [ Just <| Eth.Utils.keccak256 "Initiated(string,string)" ]
            }
    in
    EventSentry.watchOnce
        logToMsg
        eventSentry
        logFilter
