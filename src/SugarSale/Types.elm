module SugarSale.Types exposing (Bucket, BucketState(..), BucketView(..), Buy, Model, Msg(..), SugarSale, UpdateResult, focusedBucketId, getActiveBucketId, getBucketInfo, getBuysForUserDefaultEmpty, justModelUpdate, makeBlankBucket, numBucketsToSide, visibleBucketIds)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Time as TimeHelpers
import Http
import List.Extra
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , testMode : Bool
    , now : Time.Posix
    , saleStartTime : Maybe Time.Posix
    , sugarSale : Maybe SugarSale
    , bucketView : BucketView
    }


type Msg
    = NoOp
    | CmdUp (CmdUp Msg)
    | Refresh Time.Posix
    | SaleStartTimestampFetched (Result Http.Error BigInt)
    | BucketValueEnteredFetched Int (Result Http.Error BigInt)


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
    { startTime : Time.Posix
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
                ( Future, makeBlankBucket testMode sugarSale.startTime bucketId )


focusedBucketId : SugarSale -> BucketView -> Time.Posix -> Bool -> Int
focusedBucketId sugarSale bucketView now testMode =
    case bucketView of
        ViewActive ->
            getActiveBucketId sugarSale now testMode

        ViewId id ->
            id


visibleBucketIds : SugarSale -> BucketView -> Time.Posix -> Bool -> List Int
visibleBucketIds sugarSale bucketView now testMode =
    let
        centerBucketId =
            focusedBucketId sugarSale bucketView now testMode
    in
    List.range
        (max (centerBucketId - numBucketsToSide) 0)
        (centerBucketId + numBucketsToSide)


getActiveBucketId : SugarSale -> Time.Posix -> Bool -> Int
getActiveBucketId sugarSale now testMode =
    (TimeHelpers.sub now sugarSale.startTime
        |> TimeHelpers.posixToSeconds
    )
        // (Config.sugarSaleBucketInterval testMode
                |> TimeHelpers.posixToSeconds
           )


type alias Bucket =
    { startTime : Time.Posix
    , totalValueEntered : Maybe TokenValue
    , buysForUser : Maybe (List Buy)
    }


makeBlankBucket : Bool -> Time.Posix -> Int -> Bucket
makeBlankBucket testMode sugarSaleStartTime bucketId =
    Bucket
        (TimeHelpers.posixToSeconds sugarSaleStartTime
            + (TimeHelpers.posixToSeconds (Config.sugarSaleBucketInterval testMode)
                * bucketId
              )
            |> TimeHelpers.secondsToPosix
        )
        Nothing
        Nothing


type BucketState
    = Past
    | Active
    | Future


type BucketView
    = ViewActive
    | ViewId Int


getBuysForUserDefaultEmpty : Bucket -> List Buy
getBuysForUserDefaultEmpty bucket =
    bucket.buysForUser
        |> Maybe.withDefault []


type alias Buy =
    { valueEntered : TokenValue
    , tokensExited : TokenValue
    , referralAddress : Maybe Address
    }


numBucketsToSide =
    2
