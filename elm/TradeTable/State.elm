module TradeTable.State exposing (init, update)

import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Routing
import TradeTable.Types exposing (..)


init : ( ColType, Ordering ) -> Model
init =
    Model


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        TradeClicked tradeRef ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ CmdUp.GotoRoute <| Routing.Trade tradeRef
                , CmdUp.gTag
                    "trade clicked"
                    "navigation"
                    (tradeRef.factory
                        |> factoryName
                    )
                    tradeRef.id
                ]

        ChangeSort newOrderCol ->
            let
                newOrderBy =
                    if Tuple.first prevModel.orderBy == newOrderCol then
                        prevModel.orderBy
                            |> Tuple.mapSecond flipOrdering

                    else
                        ( newOrderCol, Ascending )

                gTagVal =
                    if Tuple.second newOrderBy == Ascending then
                        1

                    else
                        0
            in
            UpdateResult
                { prevModel
                    | orderBy = newOrderBy
                }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag
                    "change sort"
                    "input"
                    (colTypeToString newOrderCol)
                    gTagVal
                ]

        NoOp ->
            justModelUpdate prevModel
