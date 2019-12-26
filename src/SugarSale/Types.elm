module SugarSale.Types exposing (Bucket, Buy, Model, Msg(..), SugarSale, UpdateResult, getBuysDefaultEmpty, justModelUpdate, totalTokensExited, totalValueEntered)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Http
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , testMode : Bool
    , currentBlock : Maybe BigInt
    , saleStartblock : Maybe BigInt
    , sugarSale : Maybe SugarSale
    }


type alias SugarSale =
    { pastBuckets : List Bucket
    , activeBucket : Bucket
    }


type Msg
    = NoOp
    | CmdUp (CmdUp Msg)
    | Refresh
    | BlocknumFetched (Result Http.Error Int)
    | SaleStartblockFetched (Result Http.Error BigInt)


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
    { startBlock : BigInt
    , buys : Maybe (List Buy)
    }


getBuysDefaultEmpty : Bucket -> List Buy
getBuysDefaultEmpty bucket =
    bucket.buys
        |> Maybe.withDefault []


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
