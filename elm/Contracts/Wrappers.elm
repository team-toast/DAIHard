module Contracts.Wrappers exposing (getAllowanceCmd, getCreationInfoFromIdCmd, getInitiatedEventDataSentryCmd, getNumTradesCmd, getParametersAndStateCmd, getParametersCmd, getParametersStateAndPhaseInfoCmd, getPhaseCmd, getStateCmd, openTrade)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardNativeFactory as DHNF
import Contracts.Generated.DAIHardNativeTrade as DHNT
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types exposing (..)
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address, Call)
import Eth.Utils
import Flip exposing (flip)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Http
import Json.Decode
import Json.Encode
import PaymentMethods
import Currencies exposing (Price)
import Task
import Time
import TokenValue exposing (TokenValue)


openTrade : FactoryType -> CreateParameters -> Call Address
openTrade factoryType parameters =
    let
        callConstructor =
            case factoryType of
                Token _ ->
                    DHF.createOpenTrade

                Native _ ->
                    DHNF.createOpenTrade
    in
    callConstructor
        (Config.factoryAddress factoryType)
        parameters.initiatorAddress
        (Config.devFeeAddress factoryType)
        (parameters.initiatorRole == Seller)
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
        |> (case factoryType of
                Native _ ->
                    EthHelpers.updateCallValue
                        (TokenValue.getEvmValue <| calculateFullInitialDeposit parameters)

                _ ->
                    identity
           )


getAllowanceCmd : TokenFactoryType -> Address -> Address -> (Result Http.Error BigInt -> msg) -> Cmd msg
getAllowanceCmd tokenType owner spender msgConstructor =
    Eth.call
        (EthHelpers.httpProviderForFactory (Token tokenType))
        (TokenContract.allowance
            (Config.tokenContractAddress tokenType)
            owner
            spender
        )
        |> Task.attempt msgConstructor


getNumTradesCmd : FactoryType -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTradesCmd factoryType msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHF.numTrades (Config.factoryAddress factoryType))
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : FactoryType -> BigInt -> (Result Http.Error DHF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd factoryType ttId msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHF.createdTrades (Config.factoryAddress factoryType) ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : FactoryType -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd factoryType address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd factoryType address parametersMsgConstructor
        , getStateCmd factoryType address stateMsgConstructor
        ]


getParametersStateAndPhaseInfoCmd : FactoryType -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getParametersStateAndPhaseInfoCmd factoryType address parametersMsgConstructor stateMsgConstructor phaseStartInfoConstructor =
    Cmd.batch
        [ getParametersCmd factoryType address parametersMsgConstructor
        , getStateCmd factoryType address stateMsgConstructor
        , getPhaseStartInfoCmd factoryType address phaseStartInfoConstructor
        ]


getParametersCmd : FactoryType -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> Cmd msg
getParametersCmd factoryType ttAddress msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHT.getParameters ttAddress)
        |> Task.map decodeParameters
        |> Task.attempt msgConstructor


getStateCmd : FactoryType -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd factoryType ttAddress msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHT.getState ttAddress)
        |> Task.map decodeState
        |> Task.attempt msgConstructor


getPhaseCmd : FactoryType -> Address -> (Result Http.Error (Maybe Phase) -> msg) -> Cmd msg
getPhaseCmd factoryType ttAddress msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHT.phase ttAddress)
        |> Task.map bigIntToPhase
        |> Task.attempt msgConstructor


getPhaseStartInfoCmd : FactoryType -> Address -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getPhaseStartInfoCmd factoryType ttAddress msgConstructor =
    Eth.call (EthHelpers.httpProviderForFactory factoryType) (DHT.getPhaseStartInfo ttAddress)
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
