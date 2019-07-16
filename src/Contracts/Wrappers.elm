module Contracts.Wrappers exposing (getAllowanceCmd, getCreationInfoFromIdCmd, getInitiatedEventDataSentryCmd, getNumTradesCmd, getParametersAndStateCmd, getParametersCmd, getParametersStateAndPhaseInfoCmd, getStateCmd, openTrade)

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
        Config.devFeeAddress
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


getAllowanceCmd : EthHelpers.Web3Context -> TokenFactoryType -> Address -> Address -> (Result Http.Error BigInt -> msg) -> Cmd msg
getAllowanceCmd web3Context tokenFactoryType owner spender msgConstructor =
    Eth.call
        web3Context.httpProvider
        (TokenContract.allowance
            (Config.tokenContractAddress tokenFactoryType)
            owner
            spender
        )
        |> Task.attempt msgConstructor


getNumTradesCmd : EthHelpers.Web3Context -> (Result Http.Error BigInt -> msg) -> Cmd msg
getNumTradesCmd web3Context msgConstructor =
    Eth.call web3Context.httpProvider (DHF.numTrades (Config.factoryAddress web3Context.factoryType))
        |> Task.attempt msgConstructor


getCreationInfoFromIdCmd : EthHelpers.Web3Context -> BigInt -> (Result Http.Error DHF.CreatedTrade -> msg) -> Cmd msg
getCreationInfoFromIdCmd web3Context ttId msgConstructor =
    Eth.call web3Context.httpProvider (DHF.createdTrades (Config.factoryAddress web3Context.factoryType) ttId)
        |> Task.attempt msgConstructor


getParametersAndStateCmd : EthHelpers.Web3Context -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getParametersAndStateCmd web3Context address parametersMsgConstructor stateMsgConstructor =
    Cmd.batch
        [ getParametersCmd web3Context address parametersMsgConstructor
        , getStateCmd web3Context address stateMsgConstructor
        ]


getParametersStateAndPhaseInfoCmd : EthHelpers.Web3Context -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> (Result Http.Error (Maybe State) -> msg) -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getParametersStateAndPhaseInfoCmd web3Context address parametersMsgConstructor stateMsgConstructor phaseStartInfoConstructor =
    Cmd.batch
        [ getParametersCmd web3Context address parametersMsgConstructor
        , getStateCmd web3Context address stateMsgConstructor
        , getPhaseStartInfoCmd web3Context address phaseStartInfoConstructor
        ]


getParametersCmd : EthHelpers.Web3Context -> Address -> (Result Http.Error (Result String TradeParameters) -> msg) -> Cmd msg
getParametersCmd web3Context ttAddress msgConstructor =
    Eth.call web3Context.httpProvider (DHT.getParameters ttAddress)
        |> Task.map decodeParameters
        |> Task.attempt msgConstructor


getStateCmd : EthHelpers.Web3Context -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd web3Context ttAddress msgConstructor =
    Eth.call web3Context.httpProvider (DHT.getState ttAddress)
        |> Task.map decodeState
        |> Task.attempt msgConstructor


getPhaseStartInfoCmd : EthHelpers.Web3Context -> Address -> (Result Http.Error (Maybe PhaseStartInfo) -> msg) -> Cmd msg
getPhaseStartInfoCmd web3Context ttAddress msgConstructor =
    Eth.call web3Context.httpProvider (DHT.getPhaseStartInfo ttAddress)
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
