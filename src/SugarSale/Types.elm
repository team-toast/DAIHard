module SugarSale.Types exposing (Model, Msg(..), UpdateResult, justModelUpdate)

import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , testMode : Bool
    }


type Msg
    = NoOp
    | CmdUp (CmdUp Msg)


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { model = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , cmdUps = []
    }
