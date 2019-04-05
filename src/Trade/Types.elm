module Trade.Types exposing
    ( ContractAction(..)
    , Model
    , Msg(..)
    , PhaseState(..)
    , StatsModel(..)
    , TxChainStatus(..)
    )

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address, TxHash, TxReceipt)
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
    , expandedPhase : CTypes.Phase
    , chatHistoryModel : Maybe ChatHistory.Model
    , eventsWaitingForChatHistory : List ( Int, CTypes.DAIHardEvent )
    , showChatHistory : Bool
    , secureCommInfo : SecureCommInfo
    , eventSentry : EventSentry Msg
    , allowance : Maybe BigInt
    , txChainStatus : TxChainStatus
    }


type Msg
    = CreationInfoFetched (Result Http.Error DHF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe CTypes.State))
    | ParametersFetched (Result Http.Error (Result String CTypes.TradeParameters))
    | AllowanceFetched (Result Http.Error BigInt)
    | CommitClicked CTypes.FullTradeInfo UserInfo BigInt
    | AbortCommit
    | ConfirmCommit CTypes.FullTradeInfo UserInfo BigInt
    | CommitSigned (Result String TxHash)
    | CommitMined (Result String TxReceipt)
    | StartContractAction ContractAction
    | ActionMined ContractAction (Result String TxReceipt)
    | ActionSigned ContractAction (Result String TxHash)
    | ApproveSigned (Result String TxHash)
    | Refresh Time.Posix
    | ExpandPhase CTypes.Phase
    | ToggleChat
    | EventLogFetched Eth.Types.Log
    | EventSentryMsg EventSentry.Msg
    | ChatHistoryMsg ChatHistory.Msg
    | MessageSubmitMined (Result String TxReceipt)
    | EncryptionFinished Json.Decode.Value


type TxChainStatus
    = NoTx
    | ConfirmingCommit CTypes.FullTradeInfo UserInfo BigInt
    | ApproveNeedsSig
    | ApproveMining TxHash
    | CommitNeedsSig
    | CommitMining TxHash
    | ActionNeedsSig ContractAction
    | ActionMining ContractAction TxHash
    | TxError String


type ContractAction
    = Poke
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
