module BucketSale.Types exposing (AllowanceState(..), BucketData, BucketSale, BucketState(..), BucketView(..), Buy, EnterUXModel, FetchedBucketInfo(..), Model, Msg(..), RelevantTimingInfo, UpdateResult, ValidBucketInfo, buyFromBindingBuy, calcClaimableTokens, calcEffectivePricePerToken, currentBucketTimeLeft, getBucketEndTime, getBucketInfo, getCurrentBucket, getCurrentBucketId, getFocusedBucketId, getRelevantTimingInfo, justModelUpdate, makeBlankBucket, updateAllBuckets, updateBucketAt)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.BucketSale.Generated.BucketSale as BucketSaleBindings
import Contracts.BucketSale.Wrappers as BucketSaleWrappers exposing (ExitInfo)
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
    , totalTokensExited : Maybe TokenValue
    , userFryBalance : Maybe TokenValue
    , bucketView : BucketView
    , enterUXModel : EnterUXModel
    , exitInfo : Maybe BucketSaleWrappers.ExitInfo
    }


type alias EnterUXModel =
    { daiInput : String
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
    | BucketValueEnteredFetched Int (Result Http.Error TokenValue)
    | UserBuyFetched Address Int (Result Http.Error BucketSaleBindings.Buy)
    | UserExitInfoFetched Address (Result Http.Error (Maybe BucketSaleWrappers.ExitInfo))
    | TotalTokensExitedFetched (Result Http.Error TokenValue)
    | UserFryBalanceFetched Address (Result Http.Error TokenValue)
    | ClaimClicked UserInfo ExitInfo
    | FocusToBucket Int
    | DaiInputChanged String
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


type BucketView
    = ViewCurrent
    | ViewId Int


type alias BucketSale =
    { startTime : Time.Posix
    , buckets : List BucketData
    }


type FetchedBucketInfo
    = InvalidBucket
    | ValidBucket ValidBucketInfo


type alias ValidBucketInfo =
    { id : Int
    , state : BucketState
    , bucketData : BucketData
    }


type alias BucketData =
    { startTime : Time.Posix
    , totalValueEntered : Maybe TokenValue
    , userBuy : Maybe Buy
    }


type BucketState
    = Closed
    | Current
    | Future


type alias Buy =
    { valueEntered : TokenValue
    , hasExited : Bool
    }


updateAllBuckets : (BucketData -> BucketData) -> BucketSale -> BucketSale
updateAllBuckets func bucketSale =
    { bucketSale
        | buckets =
            bucketSale.buckets
                |> List.map func
    }


updateBucketAt : Int -> (BucketData -> BucketData) -> BucketSale -> Maybe BucketSale
updateBucketAt id func bucketSale =
    if id < List.length bucketSale.buckets then
        Just
            { bucketSale
                | buckets =
                    bucketSale.buckets
                        |> List.Extra.updateAt id func
            }

    else
        Nothing


getBucketInfo : BucketSale -> Int -> Time.Posix -> Bool -> FetchedBucketInfo
getBucketInfo bucketSale bucketId now testMode =
    List.Extra.getAt bucketId bucketSale.buckets
        |> Maybe.map
            (\bucket ->
                ValidBucket <|
                    ValidBucketInfo
                        bucketId
                        (if TimeHelpers.compare bucket.startTime now == GT then
                            Future

                         else if TimeHelpers.compare (getBucketEndTime bucket testMode) now == GT then
                            Current

                         else
                            Closed
                        )
                        bucket
            )
        |> Maybe.withDefault InvalidBucket


getBucketEndTime : BucketData -> Bool -> Time.Posix
getBucketEndTime bucket testMode =
    TimeHelpers.add
        bucket.startTime
        (Config.bucketSaleBucketInterval testMode)


getFocusedBucketId : BucketSale -> BucketView -> Time.Posix -> Bool -> Int
getFocusedBucketId bucketSale bucketView now testMode =
    case bucketView of
        ViewCurrent ->
            getCurrentBucketId bucketSale now testMode

        ViewId id ->
            id


getCurrentBucketId : BucketSale -> Time.Posix -> Bool -> Int
getCurrentBucketId bucketSale now testMode =
    (TimeHelpers.sub now bucketSale.startTime
        |> TimeHelpers.posixToSeconds
    )
        // (Config.bucketSaleBucketInterval testMode
                |> TimeHelpers.posixToSeconds
           )


getCurrentBucket : BucketSale -> Time.Posix -> Bool -> FetchedBucketInfo
getCurrentBucket bucketSale now testMode =
    getBucketInfo
        bucketSale
        (getCurrentBucketId bucketSale now testMode)
        now
        testMode


currentBucketTimeLeft : BucketSale -> Time.Posix -> Bool -> Maybe Time.Posix
currentBucketTimeLeft bucketSale now testMode =
    case getCurrentBucket bucketSale now testMode of
        InvalidBucket ->
            Nothing

        ValidBucket validBucketInfo ->
            Just <|
                TimeHelpers.sub
                    (getBucketEndTime validBucketInfo.bucketData testMode)
                    now


makeBlankBucket : Bool -> Time.Posix -> Int -> BucketData
makeBlankBucket testMode bucketSaleStartTime bucketId =
    BucketData
        (TimeHelpers.posixToSeconds bucketSaleStartTime
            + (TimeHelpers.posixToSeconds (Config.bucketSaleBucketInterval testMode)
                * bucketId
              )
            |> TimeHelpers.secondsToPosix
        )
        Nothing
        Nothing


buyFromBindingBuy : BucketSaleBindings.Buy -> Buy
buyFromBindingBuy bindingBuy =
    Buy
        (TokenValue.tokenValue bindingBuy.valueEntered)
        (BigInt.compare bindingBuy.buyerTokensExited (BigInt.fromInt 0) /= EQ)


calcClaimableTokens : TokenValue -> TokenValue -> Bool -> TokenValue
calcClaimableTokens totalValueEntered daiIn testMode =
    let
        claimableRatio =
            TokenValue.toFloatWithWarning daiIn
                / TokenValue.toFloatWithWarning totalValueEntered
    in
    TokenValue.mulFloatWithWarning
        (Config.bucketSaleTokensPerBucket testMode)
        claimableRatio


calcEffectivePricePerToken : TokenValue -> Bool -> TokenValue
calcEffectivePricePerToken totalValueEntered testMode =
    TokenValue.toFloatWithWarning totalValueEntered
        / (TokenValue.toFloatWithWarning <| Config.bucketSaleTokensPerBucket testMode)
        |> TokenValue.fromFloatWithWarning


type alias RelevantTimingInfo =
    { state : BucketState
    , relevantTimeFromNow : Time.Posix
    }


getRelevantTimingInfo : ValidBucketInfo -> Time.Posix -> Bool -> RelevantTimingInfo
getRelevantTimingInfo bucketInfo now testMode =
    RelevantTimingInfo
        bucketInfo.state
        (case bucketInfo.state of
            Closed ->
                -- How long ago did the bucket end?
                TimeHelpers.sub
                    now
                    bucketInfo.bucketData.startTime

            Current ->
                -- How soon will the bucket end?
                TimeHelpers.sub
                    (getBucketEndTime bucketInfo.bucketData testMode)
                    now

            Future ->
                -- How soon will the bucket start?
                TimeHelpers.sub
                    bucketInfo.bucketData.startTime
                    now
        )
