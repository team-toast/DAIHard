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
    , currentBlock : Maybe Int
    , buckets : List Bucket
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


type alias Bucket =
    { startBlock : Int
    , state : BucketState
    }


type BucketState
    = NotStarted
    | Active (Maybe (List Buy))
    | Concluded (Maybe (List Buy))


getBuysDefaultEmpty : Bucket -> List Buy
getBuysDefaultEmpty bucket =
    case bucket.state of
        Active (Just buys) ->
            buys

        Concluded (Just buys) ->
            buys

        _ ->
            []


totalValueEntered : Bucket -> TokenValue
totalValueEntered bucket =
    bucket
        |> getBuysDefaultEmpty
        |> List.map .valueEntered
        |> List.foldl TokenValue.add TokenValue.zero


totalTokensExited : Bucket -> TokenValue
totalTokensExited bucket =
    bucket
        |> getBuysDefaultEmpty
        |> List.map .tokensExited
        |> List.foldl TokenValue.add TokenValue.zero


type alias Buy =
    { valueEntered : TokenValue
    , tokensExited : TokenValue
    , referralAddress : Maybe Address
    }
