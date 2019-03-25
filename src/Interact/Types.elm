module Interact.Types exposing (CommMessage, ContractMsg(..), EncryptedMessage, Event, EventInfo(..), FullCommInfo, InitiatorOrResponder(..), MessageContent(..), Model, Msg(..), PartialCommInfo, SecureCommInfo(..), StateChangeInfo(..), checkIfCommInfoLoaded, getUserRole, partialCommInfo, updateInitiatorPubkey, updateResponderPubkey)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.Toastytrade as TT
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Types
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers
import Http
import Json.Decode
import Time


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , tokenAddress : Address
    , tokenDecimals : Int
    , trade : Contracts.Types.Trade
    , history : Array Event
    , secureCommInfo : SecureCommInfo
    , messageInput : String
    , eventSentry : EventSentry Msg
    }


type Msg
    = CreationInfoFetched (Result Http.Error TTF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Result String Contracts.Types.TradeParameters))
    | ContractAction ContractMsg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | EventsFetched Eth.Types.Log
    | MessageInputChanged String
    | MessageSubmit
    | EncryptionFinished Json.Decode.Value
    | DecryptionFinished Json.Decode.Value
    | EventSentryMsg EventSentry.Msg


type ContractMsg
    = Poke
    | Commit
    | Recall
    | Claim
    | Abort
    | Release
    | Burn


type InitiatorOrResponder
    = Initiator
    | Responder


type alias Event =
    { eventInfo : EventInfo
    , blocknum : Int
    , time : Maybe Time.Posix
    }


type EventInfo
    = Statement CommMessage
    | StateChange StateChangeInfo


type alias CommMessage =
    { who : InitiatorOrResponder
    , message : MessageContent
    , blocknum : Int
    }


type MessageContent
    = FailedDecode
    | Encrypted ( EncryptedMessage, EncryptedMessage )
    | FailedDecrypt
    | Decrypted String


type alias EncryptedMessage =
    { encapsulatedKey : String
    , iv : String
    , tag : String
    , message : String
    }


type StateChangeInfo
    = Opened
    | Committed Address
    | Recalled
    | Claimed
    | Aborted
    | Released
    | Burned


type SecureCommInfo
    = PartiallyLoadedCommInfo PartialCommInfo
    | LoadedCommInfo FullCommInfo


type alias PartialCommInfo =
    { initiatorPubkey : Maybe String
    , responderPubkey : Maybe String
    }


type alias FullCommInfo =
    { initiatorPubkey : String
    , responderPubkey : String
    }


partialCommInfo : SecureCommInfo
partialCommInfo =
    PartiallyLoadedCommInfo <| PartialCommInfo Nothing Nothing


updateResponderPubkey : String -> SecureCommInfo -> SecureCommInfo
updateResponderPubkey pubkey commInfo =
    case commInfo of
        PartiallyLoadedCommInfo pInfo ->
            { pInfo | responderPubkey = Just pubkey }
                |> checkIfCommInfoLoaded

        LoadedCommInfo info ->
            LoadedCommInfo { info | responderPubkey = pubkey }


updateInitiatorPubkey : String -> SecureCommInfo -> SecureCommInfo
updateInitiatorPubkey pubkey commInfo =
    case commInfo of
        PartiallyLoadedCommInfo pInfo ->
            { pInfo | initiatorPubkey = Just pubkey }
                |> checkIfCommInfoLoaded

        LoadedCommInfo info ->
            LoadedCommInfo { info | initiatorPubkey = pubkey }


checkIfCommInfoLoaded : PartialCommInfo -> SecureCommInfo
checkIfCommInfoLoaded pInfo =
    case ( pInfo.initiatorPubkey, pInfo.responderPubkey ) of
        ( Just initiatorPubkey, Just responderPubkey ) ->
            LoadedCommInfo <|
                FullCommInfo
                    initiatorPubkey
                    responderPubkey

        _ ->
            PartiallyLoadedCommInfo pInfo


getUserRole : Contracts.Types.FullTradeInfo -> Address -> Maybe InitiatorOrResponder
getUserRole tradeInfo userAddress =
    if userAddress == tradeInfo.parameters.initiatorAddress then
        Just Initiator

    else
        tradeInfo.state.responder
            |> Maybe.andThen
                (\responder ->
                    if userAddress == responder then
                        Just Responder

                    else
                        Nothing
                )
