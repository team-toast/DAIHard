module SugarSale.Types exposing (Bucket, BucketState(..), BucketView(..), Buy, Model, Msg(..), SugarSale, UpdateResult, getActiveBucketId, getBucketInfo, getBuysDefaultEmpty, justModelUpdate, makeBlankBucket, totalTokensExited, totalValueEntered)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Http
import List.Extra
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , testMode : Bool
    , currentBlock : Maybe Int
    , saleStartblock : Maybe Int
    , sugarSale : Maybe SugarSale
    , bucketView : BucketView
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


type alias SugarSale =
    { startBlock : Int
    , pastBuckets : List Bucket
    , activeBucket : Bucket
    }


getBucketInfo : SugarSale -> Int -> Bool -> ( BucketState, Bucket )
getBucketInfo sugarSale bucketId testMode =
    case sugarSale.pastBuckets |> List.Extra.getAt bucketId of
        Just pastBucket ->
            ( Past, pastBucket )

        Nothing ->
            if bucketId == List.length sugarSale.pastBuckets then
                ( Active, sugarSale.activeBucket )

            else
                ( Future, makeBlankBucket testMode sugarSale.startBlock bucketId )


getActiveBucketId : SugarSale -> Int -> Bool -> Int
getActiveBucketId sugarSale currentBlock testMode =
    (currentBlock - sugarSale.startBlock)
        // Config.sugarSaleBlocksPerBucket testMode


type alias Bucket =
    { startBlock : Int
    , buys : Maybe (List Buy)
    }


makeBlankBucket : Bool -> Int -> Int -> Bucket
makeBlankBucket testMode sugarSaleStartblock bucketId =
    Bucket
        (sugarSaleStartblock
            + (Config.sugarSaleBlocksPerBucket testMode
                * bucketId
              )
        )
        Nothing


type BucketState
    = Past
    | Active
    | Future


type BucketView
    = ViewActive
    | ViewId Int


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
