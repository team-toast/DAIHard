module TradeTable.Types exposing (ColType(..), Model, Msg(..), Ordering(..), UpdateResult, flipOrdering, justModelUpdate)

import AppCmd exposing (AppCmd)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Types as CTypes


type alias Model =
    { orderBy : ( ColType, Ordering )
    }


type Msg
    = TradeClicked FactoryType Int
    | ChangeSort ColType
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , appCmds : List (AppCmd Msg)
    }


type ColType
    = Phase
    | Expires
    | Offer
    | FiatPrice
    | Margin
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