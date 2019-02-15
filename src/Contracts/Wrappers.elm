module Contracts.Wrappers exposing (createSell, decodeParameters, getAddressFromIdCmd, getNumTTsCmd, getParametersAndStateCmd, getParametersCmd, getStateCmd)

import BigInt exposing (BigInt)
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Types exposing (..)
import Eth
import Eth.Types exposing (Address, Call)
import EthHelpers
import Flip exposing (flip)
import Http
import Json.Decode
import Task
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


createSell : Address -> FullParameters -> Call Address
createSell contractAddress parameters =
    TTF.createToastytradeSell
        contractAddress
        parameters.initiatorAddress
        (TokenValue.getBigInt parameters.uncoiningAmount)
        (TokenValue.getBigInt parameters.price)
        (TokenValue.getBigInt parameters.responderDeposit)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.depositDeadlineInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        (if parameters.transferMethods == "" then
            "NODATA"

         else
            parameters.transferMethods
        )


getNumTTsCmd : EthHelpers.EthNode -> Address -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTTsCmd ethNode factoryAddress msgConstructor =
    Eth.call ethNode.http (TTF.getNumToastytradeSells factoryAddress)
        |> Task.attempt msgConstructor


getAddressFromIdCmd : EthHelpers.EthNode -> Address -> BigInt -> (Result Http.Error Address -> msg) -> Cmd msg
getAddressFromIdCmd ethNode factoryAddress ttId msgConstructor =
    Eth.call ethNode.http (TTF.createdSells factoryAddress ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe FullParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd ethNode tokenDecimals address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd ethNode tokenDecimals address parametersMsgConstructor
        , getStateCmd ethNode tokenDecimals address stateMsgConstructor
        ]


getParametersCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe FullParameters) -> msg) -> Cmd msg
getParametersCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TTS.getParameters ttAddress)
        |> Task.map (decodeParameters numDecimals)
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TTS.getState ttAddress)
        |> Task.map (decodeState numDecimals)
        |> Task.attempt msgConstructor


decodeParameters : Int -> TTS.GetParameters -> Maybe FullParameters
decodeParameters numDecimals encodedParameters =
    let
        maybeAutorecallInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autorecallInterval

        maybeDepositDeadlineInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.depositDeadlineInterval

        maybeAutoreleaseInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoreleaseInterval
    in
    Maybe.map3
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval ->
            { uncoiningAmount = TokenValue.tokenValue numDecimals encodedParameters.sellAmount
            , price = TokenValue.tokenValue numDecimals encodedParameters.price
            , transferMethods = encodedParameters.logisticsString
            , autorecallInterval = autorecallInterval
            , depositDeadlineInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , responderDeposit = TokenValue.tokenValue numDecimals encodedParameters.responderDeposit
            }
        )
        maybeAutorecallInterval
        maybeDepositDeadlineInterval
        maybeAutoreleaseInterval
