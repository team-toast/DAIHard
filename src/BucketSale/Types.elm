module BucketSale.Types exposing (AllowanceState(..), Bucket, BucketSale, BucketState(..), BucketView(..), Buy, Model, Msg(..), UpdateResult, activeBucketTimeLeft, bucketStartTime, buyFromBindingBuy, getActiveBucketId, getBucketInfo, getClaimableTokens, getEffectivePricePerToken, getFocusedBucketId, justModelUpdate, makeBlankBucket, numBucketsToSide, updateAllPastOrActiveBuckets, updatePastOrActiveBucketAt, visibleBucketIds)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.BucketSale.Generated.BucketSale as BucketSaleBindings
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
    , timezone : Maybe Time.Zone
    , saleStartTime : Maybe Time.Posix
    , bucketSale : Maybe BucketSale
    , bucketView : BucketView
    , daiInput : String
    , dumbCheckboxesClicked : ( Bool, Bool )
    , daiAmount : Maybe (Result String TokenValue)
    , referrer : Maybe Address
    , allowanceState : AllowanceState
    }


type Msg
    = NoOp
    | CmdUp (CmdUp Msg)
    | TimezoneGot Time.Zone
    | Refresh
    | UpdateNow Time.Posix
    | SaleStartTimestampFetched (Result Http.Error BigInt)
    | BucketValueEnteredFetched Int (Result Http.Error BigInt)
    | UserBuyFetched Address Int (Result Http.Error BucketSaleBindings.Buy)
    | BucketClicked Int
    | DaiInputChanged String
    | FirstDumbCheckboxClicked Bool
    | SecondDumbCheckboxClicked Bool
    | UnlockDaiButtonClicked
    | AllowanceFetched (Result Http.Error BigInt)
    | DaiUnlockSigned (Result String TxHash)
    | DaiUnlockMined (Result String TxReceipt)
    | EnterButtonClicked UserInfo Int TokenValue (Maybe Address)
    | EnterSigned (Result String TxHash)
    | EnterMined (Result String TxReceipt)
    | ExitButtonClicked UserInfo Int
    | ExitSigned (Result String TxHash)
    | ExitMined (Result String TxReceipt)


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


type AllowanceState
    = Loading
    | Loaded TokenValue
    | UnlockMining


type alias BucketSale =
    { startTime : Time.Posix
    , pastBuckets : List Bucket
    , activeBucket : Bucket
    }


getBucketInfo : BucketSale -> Int -> Bool -> ( BucketState, Bucket )
getBucketInfo bucketSale bucketId testMode =
    case bucketSale.pastBuckets |> List.Extra.getAt bucketId of
        Just pastBucket ->
            ( Past, pastBucket )

        Nothing ->
            if bucketId == List.length bucketSale.pastBuckets then
                ( Active, bucketSale.activeBucket )

            else
                ( Future, makeBlankBucket testMode bucketSale.startTime bucketId )


updateAllPastOrActiveBuckets : (Bucket -> Bucket) -> BucketSale -> BucketSale
updateAllPastOrActiveBuckets func bucketSale =
    { bucketSale
        | pastBuckets =
            bucketSale.pastBuckets
                |> List.map func
        , activeBucket =
            bucketSale.activeBucket |> func
    }


updatePastOrActiveBucketAt : Int -> (Bucket -> Bucket) -> BucketSale -> Maybe BucketSale
updatePastOrActiveBucketAt bucketId updateFunc bucketSale =
    if bucketId == List.length bucketSale.pastBuckets then
        Just <|
            { bucketSale
                | activeBucket =
                    bucketSale.activeBucket |> updateFunc
            }

    else if bucketId < List.length bucketSale.pastBuckets then
        Just <|
            { bucketSale
                | pastBuckets =
                    bucketSale.pastBuckets
                        |> List.Extra.updateAt bucketId updateFunc
            }

    else
        Nothing


getFocusedBucketId : BucketSale -> BucketView -> Time.Posix -> Bool -> Int
getFocusedBucketId bucketSale bucketView now testMode =
    case bucketView of
        ViewActive ->
            getActiveBucketId bucketSale now testMode

        ViewId id ->
            id


visibleBucketIds : BucketSale -> BucketView -> Time.Posix -> Bool -> List Int
visibleBucketIds bucketSale bucketView now testMode =
    let
        centerBucketId =
            getFocusedBucketId bucketSale bucketView now testMode
    in
    List.range
        (max (centerBucketId - numBucketsToSide) 0)
        (centerBucketId + numBucketsToSide)


getActiveBucketId : BucketSale -> Time.Posix -> Bool -> Int
getActiveBucketId bucketSale now testMode =
    (TimeHelpers.sub now bucketSale.startTime
        |> TimeHelpers.posixToSeconds
    )
        // (Config.bucketSaleBucketInterval testMode
                |> TimeHelpers.posixToSeconds
           )


activeBucketTimeLeft : BucketSale -> Time.Posix -> Bool -> Time.Posix
activeBucketTimeLeft bucketSale now testMode =
    let
        nextBucketId =
            getActiveBucketId bucketSale now testMode + 1
    in
    TimeHelpers.sub
        (bucketStartTime bucketSale nextBucketId testMode)
        now


bucketStartTime : BucketSale -> Int -> Bool -> Time.Posix
bucketStartTime bucketSale bucketId testMode =
    TimeHelpers.add
        bucketSale.startTime
        (TimeHelpers.secondsToPosix <|
            TimeHelpers.posixToSeconds (Config.bucketSaleBucketInterval testMode)
                * bucketId
        )


type alias Bucket =
    { startTime : Time.Posix
    , totalValueEntered : Maybe TokenValue
    , userBuy : Maybe Buy
    }


makeBlankBucket : Bool -> Time.Posix -> Int -> Bucket
makeBlankBucket testMode bucketSaleStartTime bucketId =
    Bucket
        (TimeHelpers.posixToSeconds bucketSaleStartTime
            + (TimeHelpers.posixToSeconds (Config.bucketSaleBucketInterval testMode)
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


buyFromBindingBuy : BucketSaleBindings.Buy -> Buy
buyFromBindingBuy bindingBuy =
    Buy
        (TokenValue.tokenValue bindingBuy.valueEntered)
        (BigInt.compare bindingBuy.buyerTokensExited (BigInt.fromInt 0) /= EQ)


type alias Buy =
    { valueEntered : TokenValue
    , hasExited : Bool
    }


numBucketsToSide =
    3


getClaimableTokens : TokenValue -> TokenValue -> Bool -> TokenValue
getClaimableTokens totalValueEntered daiIn testMode =
    let
        claimableRatio =
            TokenValue.toFloatWithWarning daiIn
                / TokenValue.toFloatWithWarning totalValueEntered
    in
    TokenValue.mulFloatWithWarning
        (Config.bucketSaleTokensPerBucket testMode)
        claimableRatio


getEffectivePricePerToken : TokenValue -> Bool -> TokenValue
getEffectivePricePerToken totalValueEntered testMode =
    TokenValue.toFloatWithWarning totalValueEntered
        / (TokenValue.toFloatWithWarning <| Config.bucketSaleTokensPerBucket testMode)
        |> TokenValue.fromFloatWithWarning
