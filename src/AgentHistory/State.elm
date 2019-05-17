module AgentHistory.State exposing (init, subscriptions, update, updateUserInfo)

import AgentHistory.Types exposing (..)
import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Network exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)


init : EthHelpers.EthNode -> Address -> BuyerOrSeller -> Maybe UserInfo -> ( Model, Cmd Msg )
init ethNode agentAddress agentRole maybeUserInfo =
    ( { ethNode = ethNode
      , agentAddress = agentAddress
      , agentRole = agentRole
      , userInfo = maybeUserInfo
      , viewPhase = CTypes.Open
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        ViewUserRoleChanged role ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                (Just (Routing.AgentHistory prevModel.agentAddress role))

        ViewPhaseChanged phase ->
            UpdateResult
                { prevModel | viewPhase = phase }
                Cmd.none
                ChainCmd.none
                Nothing

        Poke address ->
            let
                txParams =
                    DHT.poke
                        address
                        |> Eth.toSend

                customSend =
                    { onMined = Nothing
                    , onSign = Nothing
                    , onBroadcast = Nothing
                    }

                chainCmd =
                    ChainCmd.custom
                        customSend
                        txParams
            in
            UpdateResult
                prevModel
                Cmd.none
                chainCmd
                Nothing

        TradeClicked id ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                (Just (Routing.Trade id))

        NoOp ->
            noUpdate prevModel


noUpdate : Model -> UpdateResult
noUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        Nothing


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
