module Contracts.Wrappers exposing (createSell, decodeParameters, getCreationInfoFromIdCmd, getDevFeeCmd, getNumTTsCmd, getParametersAndStateCmd, getParametersCmd, getStateCmd)

import BigInt exposing (BigInt)
import Contracts.Generated.Toastytrade as TT
import Contracts.Generated.ToastytradeFactory as TTF
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


createSell : Address -> CreateParameters -> Call Address
createSell contractAddress parameters =
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
        parameters.totalPriceString
        parameters.transferMethods
        parameters.initiatorCommPubkey


getDevFeeCmd : EthHelpers.EthNode -> Address -> BigInt -> (Result Http.Error BigInt -> msg) -> Cmd msg
getDevFeeCmd ethNode factoryAddress tradeAmount msgConstructor =
    Eth.call ethNode.http (TTF.getDevFee factoryAddress tradeAmount)
        |> Task.attempt msgConstructor


getNumTTsCmd : EthHelpers.EthNode -> Address -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTTsCmd ethNode factoryAddress msgConstructor =
    Eth.call ethNode.http (TTF.getNumToastytradeSells factoryAddress)
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : EthHelpers.EthNode -> Address -> BigInt -> (Result Http.Error TTF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd ethNode factoryAddress ttId msgConstructor =
    Eth.call ethNode.http (TTF.createdTrades factoryAddress ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe CreateParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd ethNode tokenDecimals address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd ethNode tokenDecimals address parametersMsgConstructor
        , getStateCmd ethNode tokenDecimals address stateMsgConstructor
        ]


getParametersCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe CreateParameters) -> msg) -> Cmd msg
getParametersCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TT.getParameters ttAddress)
        |> Task.map (decodeParameters numDecimals)
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TT.getState ttAddress)
        |> Task.map (decodeState numDecimals)
        |> Task.attempt msgConstructor


decodeParameters : Int -> TT.GetParameters -> Maybe CreateParameters
decodeParameters numDecimals encodedParameters =
    let
        maybeAutorecallInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autorecallInterval

        maybeDepositDeadlineInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoabortInterval

        maybeAutoreleaseInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoreleaseInterval
    in
    Maybe.map3
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval ->
            { openMode = initiatorIsBuyerToOpenMode encodedParameters.initiatorIsBuyer
            , tradeAmount = TokenValue.tokenValue numDecimals encodedParameters.tokenAmount
            , totalPriceString = encodedParameters.totalPrice
            , transferMethods = encodedParameters.fiatTransferMethods
            , initiatorCommPubkey = encodedParameters.initiatorCommPubkey
            , autorecallInterval = autorecallInterval
            , autoabortInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , buyerDeposit = TokenValue.tokenValue numDecimals encodedParameters.buyerDeposit
            , pokeReward = TokenValue.tokenValue numDecimals encodedParameters.pokeReward
            }
        )
        maybeAutorecallInterval
        maybeDepositDeadlineInterval
        maybeAutoreleaseInterval
