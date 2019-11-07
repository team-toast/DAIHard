module Trade.Types exposing (ContractAction(..), Model, Msg(..), PhaseState(..), TxChainStatus(..), UpdateResult, actionName, justModelUpdate)

import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import Routing
import Time
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.Types as ChatHistory
import Wallet


type alias Model =
    { wallet : Wallet.State
    , trade : CTypes.Trade
    , expandedPhase : CTypes.Phase
    , chatHistoryModel : Maybe ChatHistory.Model
    , eventsWaitingForChatHistory : List ( Int, CTypes.DAIHardEvent )
    , showChatHistory : Bool
    , showStatsModal : Bool
    , showOptions : Bool
    , secureCommInfo : SecureCommInfo
    , eventSentry : EventSentry Msg
    , allowance : Maybe BigInt
    , txChainStatus : Maybe TxChainStatus
    , blocknumOnInit : Maybe Int
    }


type Msg
    = CurrentBlockFetched (Result Http.Error Int)
    | CreationInfoFetched (Result Http.Error DHF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe CTypes.State))
    | ParametersFetched (Result Http.Error (Result String CTypes.TradeParameters))
    | PhaseInfoFetched (Result Http.Error (Maybe CTypes.PhaseStartInfo))
    | AllowanceFetched (Result Http.Error BigInt)
    | ToggleShowOptions Bool
    | DuplicateClicked TradeReference
    | CommitClicked CTypes.FullTradeInfo UserInfo BigInt
    | AbortAction
    | ConfirmCommit CTypes.FullTradeInfo UserInfo BigInt
    | CommitSigned (Result String TxHash)
    | CommitMined (Result String TxReceipt)
    | ContractActionClicked ContractAction
    | StartContractAction ContractAction
    | ActionMined ContractAction (Result String TxReceipt)
    | ActionSigned ContractAction (Result String TxHash)
    | ApproveSigned (Result String TxHash)
    | Refresh Time.Posix
    | ExpandPhase CTypes.Phase
    | ToggleChat
    | ToggleStatsModal
    | ViewUserHistory BuyerOrSeller
    | EventLogFetched Eth.Types.Log
    | EventSentryMsg EventSentry.Msg
    | ChatHistoryMsg ChatHistory.Msg
    | MessageSubmitMined (Result String TxReceipt)
    | EncryptionFinished Json.Decode.Value
    | Web3Connect
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []


type TxChainStatus
    = ConfirmingCommit UserInfo BigInt
    | ApproveNeedsSig
    | ApproveMining TxHash
    | CommitNeedsSig
    | CommitMining TxHash
    | ConfirmingAction ContractAction
    | ActionNeedsSig ContractAction
    | ActionMining ContractAction TxHash


type ContractAction
    = Poke
    | Recall
    | Claim
    | Abort
    | Release
    | Burn


actionName : ContractAction -> String
actionName action =
    case action of
        Poke ->
            "poke"

        Recall ->
            "recall"

        Claim ->
            "claim"

        Abort ->
            "abort"

        Release ->
            "release"

        Burn ->
            "burn"


type PhaseState
    = NotStarted
    | Active
    | Finished
