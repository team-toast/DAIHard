module TradeTable.State exposing (init, update)

import CmdUp exposing (CmdUp)
import ChainCmd exposing (ChainCmd)
import Routing
import TradeTable.Types exposing (..)


init : ( ColType, Ordering ) -> Model
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
                [ CmdUp.GotoRoute <| Routing.Trade factory id ]

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

        NoOp ->
            justModelUpdate prevModel
