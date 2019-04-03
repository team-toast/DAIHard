module Contracts.Wrappers exposing (decodeParameters, getCreationInfoFromIdCmd, getDevFeeCmd, getExtraFeesCmd, getNumTradesCmd, getOpenedEventDataSentryCmd, getParametersAndStateCmd, getParametersCmd, getStateCmd, openTrade)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types exposing (..)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address, Call)
import Eth.Utils
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Http
import Json.Decode
import Json.Encode
import Network exposing (..)
import PaymentMethods
import Task
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


openTrade : Network -> CreateParameters -> Call Address
openTrade network parameters =
    let
        encodedPaymentMethods =
            Json.Encode.list PaymentMethods.encode
                parameters.paymentMethods
                |> Json.Encode.encode 0

        encodedFiatData =
            FiatValue.encode parameters.fiatPrice
                |> Json.Encode.encode 0
    in
    DHF.openDAIHardTrade
        (factoryAddress network)
        parameters.initiatorAddress
        (openModeToInitiatorIsBuyer parameters.openMode)
        (TokenValue.getBigInt parameters.tradeAmount)
        (TokenValue.getBigInt parameters.pokeReward)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoabortInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        encodedFiatData
        encodedPaymentMethods
        parameters.initiatorCommPubkey


getDevFeeCmd : EthHelpers.EthNode -> BigInt -> (Result Http.Error BigInt -> msg) -> Cmd msg
getDevFeeCmd ethNode tradeAmount msgConstructor =
    Eth.call ethNode.http (DHF.getDevFee (factoryAddress ethNode.network) tradeAmount)
        |> Task.attempt msgConstructor


getExtraFeesCmd : EthHelpers.EthNode -> BigInt -> (Result Http.Error DHF.GetExtraFees -> msg) -> Cmd msg
getExtraFeesCmd ethNode tradeAmount msgConstructor =
    Eth.call ethNode.http (DHF.getExtraFees (factoryAddress ethNode.network) tradeAmount)
        |> Task.attempt msgConstructor


getNumTradesCmd : EthHelpers.EthNode -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTradesCmd ethNode msgConstructor =
    Eth.call ethNode.http (DHF.getNumTrades (factoryAddress ethNode.network))
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : EthHelpers.EthNode -> BigInt -> (Result Http.Error DHF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd ethNode ttId msgConstructor =
    Eth.call ethNode.http (DHF.createdTrades (factoryAddress ethNode.network) ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd ethNode address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd ethNode address parametersMsgConstructor
        , getStateCmd ethNode address stateMsgConstructor
        ]


getParametersCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> Cmd msg
getParametersCmd ethNode ttAddress msgConstructor =
    Eth.call ethNode.http (DHT.getParameters ttAddress)
        |> Task.map (decodeParameters tokenDecimals)
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.EthNode -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode ttAddress msgConstructor =
    Eth.call ethNode.http (DHT.getState ttAddress)
        |> Task.map (decodeState tokenDecimals)
        |> Task.attempt msgConstructor


getOpenedEventDataSentryCmd : EventSentry msg -> TradeCreationInfo -> (Result Json.Decode.Error DHT.Opened -> msg) -> ( EventSentry msg, Cmd msg )
getOpenedEventDataSentryCmd eventSentry creationInfo msgConstructor =
    let
        logToMsg : Eth.Types.Log -> msg
        logToMsg log =
            (Eth.Decode.event DHT.openedDecoder log).returnData
                |> msgConstructor

        logFilter =
            { fromBlock = Eth.Types.BlockNum creationInfo.blocknum
            , toBlock = Eth.Types.BlockNum creationInfo.blocknum
            , address = creationInfo.address
            , topics = [ Just <| Eth.Utils.keccak256 "Opened(string,string)" ]
            }
    in
    EventSentry.watchOnce
        logToMsg
        eventSentry
        logFilter


decodeParameters : Int -> DHT.GetParameters -> Result String TradeParameters
decodeParameters numDecimals encodedParameters =
    let
        autorecallIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autorecallInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"

        depositDeadlineIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoabortInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"

        autoreleaseIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoreleaseInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"

        decodedFiatPrice =
            Json.Decode.decodeString
                FiatValue.decoder
                encodedParameters.totalPrice
                |> Result.mapError Json.Decode.errorToString
    in
    Result.map4
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval fiatPrice ->
            { openMode = initiatorIsBuyerToOpenMode encodedParameters.initiatorIsBuyer
            , tradeAmount = TokenValue.tokenValue numDecimals encodedParameters.daiAmount
            , buyerDeposit = TokenValue.tokenValue numDecimals encodedParameters.buyerDeposit
            , fiatPrice = fiatPrice
            , autorecallInterval = autorecallInterval
            , autoabortInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , pokeReward = TokenValue.tokenValue numDecimals encodedParameters.pokeReward
            }
        )
        autorecallIntervalResult
        depositDeadlineIntervalResult
        autoreleaseIntervalResult
        decodedFiatPrice
