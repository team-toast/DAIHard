module Trade.Types exposing (ContractMsg(..), InitiatorOrResponder(..), Model, Msg(..), getUserRole)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
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
    , trade : Contracts.Types.Trade
    , eventSentry : EventSentry Msg
    }


type Msg
    = CreationInfoFetched (Result Http.Error DHF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Result String Contracts.Types.TradeParameters))
    | ContractAction ContractMsg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | EventLogFetched Eth.Types.Log
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
