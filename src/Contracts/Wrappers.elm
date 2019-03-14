module Contracts.Wrappers exposing (createSell, decodeParameters, getCreationInfoFromIdCmd, getDevFeeCmd, getNumTradesCmd, getParametersAndStateCmd, getParametersCmd, getStateCmd)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.Toastytrade as TT
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Types exposing (..)
import Eth
import Eth.Types exposing (Address, Call)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Http
import Json.Decode
import Json.Encode
import Task
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import TransferMethods


createSell : Address -> CreateParameters -> Call Address
createSell contractAddress parameters =
    let
        encodedTransferMethods =
            Json.Encode.list TransferMethods.encodeTransferMethod
                parameters.transferMethods
                |> Json.Encode.encode 0

        encodedFiatData =
            FiatValue.encode parameters.fiatPrice
                |> Json.Encode.encode 0
    in
    TTF.openToastytrade
        contractAddress
        parameters.initiatorAddress
        (openModeToInitiatorIsBuyer parameters.openMode)
        (TokenValue.getBigInt parameters.tradeAmount)
        (TokenValue.getBigInt parameters.buyerDeposit)
        (TokenValue.getBigInt parameters.pokeReward)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoabortInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        encodedFiatData
        encodedTransferMethods
        parameters.initiatorCommPubkey


getDevFeeCmd : EthHelpers.EthNode -> Address -> BigInt -> (Result Http.Error BigInt -> msg) -> Cmd msg
getDevFeeCmd ethNode factoryAddress tradeAmount msgConstructor =
    Eth.call ethNode.http (TTF.getDevFee factoryAddress tradeAmount)
        |> Task.attempt msgConstructor


getNumTradesCmd : EthHelpers.EthNode -> Address -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTradesCmd ethNode factoryAddress msgConstructor =
    Eth.call ethNode.http (TTF.getNumToastytradeSells factoryAddress)
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : EthHelpers.EthNode -> Address -> BigInt -> (Result Http.Error TTF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd ethNode factoryAddress ttId msgConstructor =
    Eth.call ethNode.http (TTF.createdTrades factoryAddress ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Result String CreateParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd ethNode tokenDecimals address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd ethNode tokenDecimals address parametersMsgConstructor
        , getStateCmd ethNode tokenDecimals address stateMsgConstructor
        ]


getParametersCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Result String CreateParameters) -> msg) -> Cmd msg
getParametersCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TT.getParameters ttAddress)
        |> Task.map (decodeParameters numDecimals)
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TT.getState ttAddress)
        |> Task.map (decodeState numDecimals)
        |> Task.attempt msgConstructor


decodeParameters : Int -> TT.GetParameters -> Result String CreateParameters
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

        decodedTransferMethodsResult =
            Json.Decode.decodeString
                (Json.Decode.list TransferMethods.transferMethodDecoder)
                encodedParameters.fiatTransferMethods
                |> Result.mapError Json.Decode.errorToString

        decodedFiatPrice =
            Json.Decode.decodeString
                FiatValue.decoder
                encodedParameters.totalPrice
                |> Result.mapError Json.Decode.errorToString
    in
    Result.map5
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval decodedTransferMethods fiatPrice ->
            { openMode = initiatorIsBuyerToOpenMode encodedParameters.initiatorIsBuyer
            , tradeAmount = TokenValue.tokenValue numDecimals encodedParameters.tokenAmount
            , fiatPrice = fiatPrice
            , transferMethods = decodedTransferMethods
            , initiatorCommPubkey = encodedParameters.initiatorCommPubkey
            , autorecallInterval = autorecallInterval
            , autoabortInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , buyerDeposit = TokenValue.tokenValue numDecimals encodedParameters.buyerDeposit
            , pokeReward = TokenValue.tokenValue numDecimals encodedParameters.pokeReward
            }
        )
        autorecallIntervalResult
        depositDeadlineIntervalResult
        autoreleaseIntervalResult
        decodedTransferMethodsResult
        decodedFiatPrice
