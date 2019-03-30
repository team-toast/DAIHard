module Trade.Types exposing
    ( ContractMsg(..)
    , Model
    , Msg(..)
    , PhaseState(..)
    , StatsModel(..)
    )

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers
import Http
import Json.Decode
import Time
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.Types as ChatHistory


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , trade : CTypes.Trade
    , stats : StatsModel
    , eventSentry : EventSentry Msg
    , chatHistoryModel : Maybe ChatHistory.Model
    , eventsWaitingForChatHistory : List ( Int, CTypes.DAIHardEvent )
    , showChatHistory : Bool
    , secureCommInfo : SecureCommInfo
    }


type Msg
    = CreationInfoFetched (Result Http.Error DHF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe CTypes.State))
    | ParametersFetched (Result Http.Error (Result String CTypes.TradeParameters))
    | ContractAction ContractMsg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | ToggleChat
    | EventLogFetched Eth.Types.Log
    | EventSentryMsg EventSentry.Msg
    | ChatHistoryMsg ChatHistory.Msg
    | MessageSubmitMined (Result String Eth.Types.TxReceipt)
    | EncryptionFinished Json.Decode.Value


type ContractMsg
    = Poke
    | Commit
    | Recall
    | Claim
    | Abort
    | Release
    | Burn


type StatsModel
    = Waiting
    | Scanning
    | DoneLoading


type PhaseState
    = NotStarted
    | Active
    | Finished
