module Trade.Types exposing
    ( BuyerOrSeller(..)
    , ContractMsg(..)
    , InitiatorOrResponder(..)
    , Model
    , Msg(..)
    , StatsModel(..)
    , getBuyerOrSeller
    , getInitiatorOrResponder
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


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , trade : CTypes.Trade
    , stats : StatsModel
    , eventSentry : EventSentry Msg
    }


type Msg
    = CreationInfoFetched (Result Http.Error DHF.CreatedTrade)
    | StateFetched (Result Http.Error (Maybe CTypes.State))
    | ParametersFetched (Result Http.Error (Result String CTypes.TradeParameters))
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


type BuyerOrSeller
    = Buyer
    | Seller


type StatsModel
    = Waiting
    | Scanning
    | Finished


getInitiatorOrResponder : CTypes.FullTradeInfo -> Address -> Maybe InitiatorOrResponder
getInitiatorOrResponder tradeInfo userAddress =
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


getBuyerOrSeller : CTypes.FullTradeInfo -> Address -> Maybe BuyerOrSeller
getBuyerOrSeller tradeInfo userAddress =
    getInitiatorOrResponder tradeInfo userAddress
        |> Maybe.map
            (\initiatorOrResponder ->
                case ( initiatorOrResponder, tradeInfo.parameters.openMode ) of
                    ( Initiator, CTypes.SellerOpened ) ->
                        Seller

                    ( Initiator, CTypes.BuyerOpened ) ->
                        Buyer

                    ( Responder, CTypes.SellerOpened ) ->
                        Buyer

                    ( Responder, CTypes.BuyerOpened ) ->
                        Seller
            )
