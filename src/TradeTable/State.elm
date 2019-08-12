module TradeTable.State exposing (init, update)

import AppCmd exposing (AppCmd)
import ChainCmd exposing (ChainCmd)
import Routing
import TradeTable.Filters.State as Filters
import TradeTable.Filters.Types as Filters
import TradeTable.Types exposing (..)


init : ( ColType, Ordering ) -> Filters.Model -> Model
init =
    Model


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        TradeClicked factory id ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.GotoRoute <| Routing.Trade factory id ]

        ChangeSort newOrderCol ->
            let
                newOrderBy =
                    if Tuple.first prevModel.orderBy == newOrderCol then
                        prevModel.orderBy
                            |> Tuple.mapSecond flipOrdering

                    else
                        ( newOrderCol, Ascending )
            in
            justModelUpdate
                { prevModel
                    | orderBy = newOrderBy
                }

        FiltersMsg filtersMsg ->
            justModelUpdate
                { prevModel
                    | filtersModel =
                        prevModel.filtersModel |> Filters.update filtersMsg
                }

        NoOp ->
            justModelUpdate prevModel
