module SugarSale.Types exposing (Bucket, BucketState(..), BucketUserExitInfo, BucketView(..), Model, Msg(..), SugarSale, UpdateResult, buyToBucketExitInfo, focusedBucketId, getActiveBucketId, getBucketInfo, justModelUpdate, makeBlankBucket, numBucketsToSide, updateAllPastOrActiveBuckets, updatePastOrActiveBucketAt, visibleBucketIds)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.SugarSale.Generated.BucketSale as SugarSaleContract
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Eth as EthHelpers
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
    | UserBuyFetched Address Int (Result Http.Error SugarSaleContract.Buy)


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


updateAllPastOrActiveBuckets : (Bucket -> Bucket) -> SugarSale -> SugarSale
updateAllPastOrActiveBuckets func sugarSale =
    { sugarSale
        | pastBuckets =
            sugarSale.pastBuckets
                |> List.map func
        , activeBucket =
            sugarSale.activeBucket |> func
    }


updatePastOrActiveBucketAt : Int -> (Bucket -> Bucket) -> SugarSale -> Maybe SugarSale
updatePastOrActiveBucketAt bucketId updateFunc sugarSale =
    if bucketId == List.length sugarSale.pastBuckets then
        Just <|
            { sugarSale
                | activeBucket =
                    sugarSale.activeBucket |> updateFunc
            }

    else if bucketId < List.length sugarSale.pastBuckets then
        Just <|
            { sugarSale
                | pastBuckets =
                    sugarSale.pastBuckets
                        |> List.Extra.updateAt bucketId updateFunc
            }

    else
        Nothing


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
    , userExitInfo : Maybe BucketUserExitInfo
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


buyToBucketExitInfo : SugarSaleContract.Buy -> BucketUserExitInfo
buyToBucketExitInfo genBuy =
    BucketUserExitInfo
        ((BigInt.compare genBuy.valueEntered (BigInt.fromInt 0) == GT)
            && (BigInt.compare genBuy.tokensExited (BigInt.fromInt 0) == EQ)
        )
        (not <| EthHelpers.addressIs0x0 genBuy.referralAddress)


type alias BucketUserExitInfo =
    { hasTokensToClaim : Bool
    , hasReferallBonus : Bool
    }


numBucketsToSide =
    2
