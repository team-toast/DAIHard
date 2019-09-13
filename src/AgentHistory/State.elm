module AgentHistory.State exposing (init, runCmdDown, subscriptions, update)

import AgentHistory.Types exposing (..)
import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown
import CmdUp
import CommonTypes exposing (..)
import Config exposing (..)
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Filters.State as Filters
import Filters.Types as Filter
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import PaymentMethods exposing (PaymentMethod)
import PriceFetch
import Currencies exposing (Price)
import Routing
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.State as TradeTable
import TradeTable.Types as TradeTable
import UserNotice as UN
import Wallet


init : Wallet.State -> Address -> ( Model, Cmd Msg )
init wallet agentAddress =
    ( { wallet = wallet
      , agentAddress = agentAddress
      , filters =
            Filters.init
                [ Filter.phases True True True False
                , Filter.offerType True True
                , Filter.role agentAddress True True
                ]
      , tradeTable =
            TradeTable.init
                ( TradeTable.Phase, TradeTable.Ascending )
      , now = Time.millisToPosix 0
      , prices = []
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
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
                []

        TradeClicked factory id ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ CmdUp.GotoRoute (Routing.Trade factory id) ]

        FiltersMsg filtersMsg ->
            justModelUpdate
                { prevModel
                    | filters =
                        prevModel.filters |> Filters.update filtersMsg
                }

        TradeTableMsg tradeTableMsg ->
            let
                ttUpdateResult =
                    prevModel.tradeTable
                        |> TradeTable.update tradeTableMsg
            in
            UpdateResult
                { prevModel
                    | tradeTable = ttUpdateResult.model
                }
                (Cmd.map TradeTableMsg ttUpdateResult.cmd)
                (ChainCmd.map TradeTableMsg ttUpdateResult.chainCmd)
                (List.map (CmdUp.map TradeTableMsg) ttUpdateResult.cmdUps)

        UpdateNow time ->
            justModelUpdate
                { prevModel | now = time }

        PricesFetched fetchResult ->
            case fetchResult of
                Ok pricesAndTimestamps ->
                    let
                        newPrices =
                            pricesAndTimestamps
                                |> List.map (Tuple.mapSecond (PriceFetch.checkAgainstTime prevModel.now))
                    in
                    justModelUpdate
                        { prevModel | prices = newPrices }

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice UN.cantFetchPrices ]

        Refresh ->
            UpdateResult
                prevModel
                (PriceFetch.fetch PricesFetched)
                ChainCmd.none
                []

        NoOp ->
            justModelUpdate prevModel


runCmdDown : CmdDown.CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            justModelUpdate { prevModel | wallet = wallet }

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 500 UpdateNow
        , Time.every 5000 (always Refresh)
        ]
