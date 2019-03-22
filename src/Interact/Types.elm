module Interact.Types exposing (CommMessage, EncryptedMessage, Event, EventInfo(..), InitiatorOrResponder(..), MessageContent(..), Model, Msg(..), StateChangeInfo(..), getUserRole)

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
import RenderContract.Types
import Time


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , tokenAddress : Address
    , tokenDecimals : Int
    , trade : Contracts.Types.Trade
    , history : Array Event
    , messageInput : String
    , eventSentry : EventSentry Msg
    }


type Msg
    = CreationInfoFetched (Result Http.Error TTF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Result String Contracts.Types.CreateParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | EventsFetched (Eth.Types.Log)
    | MessageInputChanged String
    | MessageSubmit
    | EncryptionFinished Json.Decode.Value
    | DecryptionFinished Json.Decode.Value
    | EventSentryMsg EventSentry.Msg


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
