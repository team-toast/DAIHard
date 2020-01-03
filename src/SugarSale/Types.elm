module SugarSale.Types exposing (AllowanceState(..), Bucket, BucketState(..), BucketView(..), Buy, Model, Msg(..), SugarSale, UpdateResult, activeBucketTimeLeft, bucketStartTime, buyFromBindingBuy, getActiveBucketId, getBucketInfo, getClaimableTokens, getEffectivePricePerToken, getFocusedBucketId, justModelUpdate, makeBlankBucket, numBucketsToSide, updateAllPastOrActiveBuckets, updatePastOrActiveBucketAt, visibleBucketIds)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.SugarSale.Generated.BucketSale as SugarSaleBindings
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
    , sugarSale : Maybe SugarSale
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
    | UserBuyFetched Address Int (Result Http.Error SugarSaleBindings.Buy)
    | BucketClicked Int
    | DaiInputChanged String
    | FirstDumbCheckboxClicked Bool
    | SecondDumbCheckboxClicked Bool
    | UnlockDaiButtonClicked
    | AllowanceFetched (Result Http.Error BigInt)
    | DaiUnlockSigned (Result String TxHash)
    | DaiUnlockMined (Result String TxReceipt)
    | EnterButtonClicked UserInfo TokenValue (Maybe Address)
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


getFocusedBucketId : SugarSale -> BucketView -> Time.Posix -> Bool -> Int
getFocusedBucketId sugarSale bucketView now testMode =
    case bucketView of
        ViewActive ->
            getActiveBucketId sugarSale now testMode

        ViewId id ->
            id


visibleBucketIds : SugarSale -> BucketView -> Time.Posix -> Bool -> List Int
visibleBucketIds sugarSale bucketView now testMode =
    let
        centerBucketId =
            getFocusedBucketId sugarSale bucketView now testMode
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


activeBucketTimeLeft : SugarSale -> Time.Posix -> Bool -> Time.Posix
activeBucketTimeLeft sugarSale now testMode =
    let
        nextBucketId =
            getActiveBucketId sugarSale now testMode + 1
    in
    TimeHelpers.sub
        (bucketStartTime sugarSale nextBucketId testMode)
        now


bucketStartTime : SugarSale -> Int -> Bool -> Time.Posix
bucketStartTime sugarSale bucketId testMode =
    TimeHelpers.add
        sugarSale.startTime
        (TimeHelpers.secondsToPosix <|
            TimeHelpers.posixToSeconds (Config.sugarSaleBucketInterval testMode)
                * bucketId
        )


type alias Bucket =
    { startTime : Time.Posix
    , totalValueEntered : Maybe TokenValue
    , userBuy : Maybe Buy
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


buyFromBindingBuy : SugarSaleBindings.Buy -> Buy
buyFromBindingBuy bindingBuy =
    Buy
        (TokenValue.tokenValue bindingBuy.valueEntered)
        (BigInt.compare bindingBuy.tokensExited (BigInt.fromInt 0) /= EQ)
        (not <| EthHelpers.addressIs0x0 bindingBuy.referralAddress)


type alias Buy =
    { valueEntered : TokenValue
    , hasExited : Bool
    , hasReferallBonus : Bool
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
        (Config.sugarSaleTokensPerBucket testMode)
        claimableRatio


getEffectivePricePerToken : TokenValue -> Bool -> TokenValue
getEffectivePricePerToken totalValueEntered testMode =
    TokenValue.toFloatWithWarning totalValueEntered
        / (TokenValue.toFloatWithWarning <| Config.sugarSaleTokensPerBucket testMode)
        |> TokenValue.fromFloatWithWarning
