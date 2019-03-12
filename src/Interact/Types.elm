module Interact.Types exposing (CommMessage, EncryptedMessage, Event, EventInfo(..), InitiatorOrResponder(..), MessageContent(..), Model, Msg(..), StateChangeInfo(..), TradeFullInfo, TradeInfo(..), TradePartialInfo, getUserRole, partialInfo, updateParameters, updateState)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.Toastytrade as TT
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import EventSentryHack exposing (EventSentry)
import Http
import Json.Decode
import RenderContract.Types
import Time


partialInfo : CreationInfo -> TradeInfo
partialInfo creationInfo =
    PartiallyLoaded (TradePartialInfo creationInfo Nothing Nothing)


updateParameters : Contracts.Types.CreateParameters -> TradeInfo -> TradeInfo
updateParameters parameters ttsInfo =
    case ttsInfo of
        NothingLoaded ->
            let
                _ =
                    Debug.log "Trying to update parameters, but there is no creationInfo! What the heck???" ""
            in
            NothingLoaded

        PartiallyLoaded pInfo ->
            { pInfo | parameters = Just parameters }
                |> checkIfLoaded

        Loaded info ->
            Loaded { info | parameters = parameters }


updateState : Contracts.Types.State -> TradeInfo -> TradeInfo
updateState state ttsInfo =
    case ttsInfo of
        NothingLoaded ->
            let
                _ =
                    Debug.log "Trying to update state, but there is no creationInfo! What the heck???" ""
            in
            NothingLoaded

        PartiallyLoaded pInfo ->
            { pInfo | state = Just state }
                |> checkIfLoaded

        Loaded info ->
            Loaded { info | state = state }


checkIfLoaded : TradePartialInfo -> TradeInfo
checkIfLoaded pInfo =
    case ( pInfo.parameters, pInfo.state ) of
        ( Just parameters, Just state ) ->
            Loaded (TradeFullInfo pInfo.creationInfo parameters state)

        _ ->
            PartiallyLoaded pInfo


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , tokenAddress : Address
    , tokenDecimals : Int
    , tradeId : BigInt
    , tradeInfo : TradeInfo
    , history : Array Event
    , messageInput : String
    , eventSentry : Maybe (EventSentry Contracts.Types.ToastytradeEvent Msg)
    }


type Msg
    = CreationInfoFetched (Result Http.Error TTF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Result String Contracts.Types.CreateParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | ToastytradeEventsFetched (Result Http.Error (List (Eth.Types.Event Contracts.Types.ToastytradeEvent)))
    | MessageInputChanged String
    | MessageSubmit
    | EncryptionFinished Json.Decode.Value
    | DecryptionFinished Json.Decode.Value
    | EventSentryMsg EventSentryHack.Msg


type TradeInfo
    = NothingLoaded
    | PartiallyLoaded TradePartialInfo
    | Loaded TradeFullInfo


type alias TradePartialInfo =
    { creationInfo : CreationInfo
    , parameters : Maybe Contracts.Types.CreateParameters
    , state : Maybe Contracts.Types.State
    }


type alias CreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias TradeFullInfo =
    { creationInfo : CreationInfo
    , parameters : Contracts.Types.CreateParameters
    , state : Contracts.Types.State
    }


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
    | Recalled
    | Committed Address
    | Aborted
    | Claimed
    | Released
    | Burned
    | RedundantEvent


getUserRole : TradeFullInfo -> Address -> Maybe InitiatorOrResponder
getUserRole ttsInfo userAddress =
    if userAddress == ttsInfo.parameters.initiatorAddress then
        Just Initiator

    else
        ttsInfo.state.responder
            |> Maybe.andThen
                (\responder ->
                    if userAddress == responder then
                        Just Responder

                    else
                        Nothing
                )
