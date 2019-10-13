module TradeTable.Types exposing (ColType(..), Model, Msg(..), Ordering(..), UpdateResult, flipOrdering, justModelUpdate)

import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes


type alias Model =
    { orderBy : ( ColType, Ordering )
    }


type Msg
    = TradeClicked TradeReference
    | ChangeSort ColType
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


type ColType
    = Phase
    | Expires
    | Offer
    | Price
    | ResponderProfit
    | PaymentWindow
    | BurnWindow


type Ordering
    = Ascending
    | Descending


flipOrdering : Ordering -> Ordering
flipOrdering ordering =
    case ordering of
        Ascending ->
            Descending

        Descending ->
            Ascending


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []
