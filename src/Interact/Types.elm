module Interact.Types exposing (CommMessage, EncryptedMessage, InitiatorOrResponder(..), Model, Msg(..), TTSFullInfo, TTSInfo(..), TTSPartialInfo, getUserRole, partialInfo, updateParameters, updateState)

import BigInt exposing (BigInt)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import EventSentryHack exposing (EventSentry)
import Http
import Json.Decode
import RenderContract.Types
import Time


partialInfo : CreationInfo -> TTSInfo
partialInfo creationInfo =
    PartiallyLoaded (TTSPartialInfo creationInfo Nothing Nothing)


updateParameters : Contracts.Types.FullParameters -> TTSInfo -> TTSInfo
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


updateState : Contracts.Types.State -> TTSInfo -> TTSInfo
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


checkIfLoaded : TTSPartialInfo -> TTSInfo
checkIfLoaded pInfo =
    case ( pInfo.parameters, pInfo.state ) of
        ( Just parameters, Just state ) ->
            Loaded (TTSFullInfo pInfo.creationInfo parameters state)

        _ ->
            PartiallyLoaded pInfo


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , tokenAddress : Address
    , tokenDecimals : Int
    , ttsId : BigInt
    , ttsInfo : TTSInfo
    , messages : List CommMessage
    , messageInput : String
    , eventSentries : Maybe ( EventSentry TTS.InitiatorStatementLog Msg, EventSentry TTS.ResponderStatementLog Msg )
    }


type Msg
    = CreationInfoFetched (Result Http.Error TTF.CreatedSell)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Maybe Contracts.Types.FullParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | InitiatorStatementsFetched (Result Http.Error (List (Eth.Types.Event TTS.InitiatorStatementLog)))
    | ResponderStatementsFetched (Result Http.Error (List (Eth.Types.Event TTS.ResponderStatementLog)))
    | MessageInputChanged String
    | MessageSubmit
    | EncryptionFinished Json.Decode.Value
    | InitiatorStatementEventSentryMsg EventSentryHack.Msg
    | ResponderStatementEventSentryMsg EventSentryHack.Msg


type TTSInfo
    = NothingLoaded
    | PartiallyLoaded TTSPartialInfo
    | Loaded TTSFullInfo


type alias TTSPartialInfo =
    { creationInfo : CreationInfo
    , parameters : Maybe Contracts.Types.FullParameters
    , state : Maybe Contracts.Types.State
    }


type alias CreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias TTSFullInfo =
    { creationInfo : CreationInfo
    , parameters : Contracts.Types.FullParameters
    , state : Contracts.Types.State
    }


type InitiatorOrResponder
    = Initiator
    | Responder


type alias CommMessage =
    { who : InitiatorOrResponder
    , message : String
    , blocknum : Int
    }


type alias EncryptedMessage =
    { encapsulatedKey : String
    , iv : String
    , tag : String
    , message : String
    }


getUserRole : Contracts.Types.FullParameters -> Contracts.Types.State -> Address -> Maybe InitiatorOrResponder
getUserRole parameters state userAddress =
    if userAddress == parameters.initiatorAddress then
        Just Initiator

    else
        state.responder
            |> Maybe.andThen
                (\responder ->
                    if userAddress == responder then
                        Just Responder

                    else
                        Nothing
                )
