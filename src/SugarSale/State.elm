module SugarSale.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.SugarSale.Wrappers as SugarSaleContract
import Eth
import Eth.Types exposing (HttpProvider)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import List.Extra
import Maybe.Extra
import SugarSale.Types exposing (..)
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Utils
import Wallet


init : Bool -> Wallet.State -> Time.Posix -> ( Model, Cmd Msg )
init testMode wallet now =
    if testMode then
        ( { wallet = wallet
          , testMode = testMode
          , now = now
          , saleStartTime = Nothing
          , sugarSale = Nothing
          , bucketView = ViewActive
          }
        , Cmd.batch
            [ fetchSaleStartTimestampCmd testMode
            ]
        )

    else
        Debug.todo "must use test mode"


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        CmdUp cmdUp ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ cmdUp ]

        Refresh newNow ->
            let
                cmd =
                    Cmd.batch <|
                        [ fetchInfoForVisibleNonFutureBucketsCmd prevModel
                        ]
            in
            UpdateResult
                { prevModel
                    | now = newNow
                    , sugarSale =
                        Maybe.map
                            (addNewActiveBucketIfNeeded newNow prevModel.testMode)
                            prevModel.sugarSale
                }
                cmd
                ChainCmd.none
                []

        SaleStartTimestampFetched fetchResult ->
            case fetchResult of
                Ok startTimestampBigInt ->
                    if BigInt.compare startTimestampBigInt (BigInt.fromInt 0) == EQ then
                        Debug.todo "Failed to init sugar sale; sale startTime == 0."

                    else
                        let
                            startTimestamp =
                                TimeHelpers.secondsBigIntToPosixWithWarning startTimestampBigInt

                            newMaybeSugarSale =
                                case prevModel.sugarSale of
                                    Nothing ->
                                        case initSugarSale prevModel.testMode startTimestamp prevModel.now of
                                            Just s ->
                                                Just s

                                            Nothing ->
                                                Debug.todo "Failed to init sugar sale. Is it started yet?"

                                    _ ->
                                        prevModel.sugarSale
                        in
                        justModelUpdate
                            { prevModel
                                | sugarSale = newMaybeSugarSale
                                , saleStartTime = Just startTimestamp
                            }

                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching sale startTime" httpErr
                    in
                    justModelUpdate prevModel

        BucketValueEnteredFetched bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching total bucket value entered" ( bucketId, fetchResult )
                    in
                    justModelUpdate prevModel

                Ok valueEnteredBigInt ->
                    case prevModel.sugarSale of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "Warning! Bucket value fetched but there is no sugarSale present!" ""
                            in
                            justModelUpdate prevModel

                        Just oldSugarSale ->
                            let
                                valueEntered =
                                    TokenValue.tokenValue valueEnteredBigInt

                                maybeNewSugarSale =
                                    oldSugarSale
                                        |> updatePastOrActiveBucketAt
                                            bucketId
                                            (\bucket ->
                                                { bucket | totalValueEntered = Just valueEntered }
                                            )
                            in
                            case maybeNewSugarSale of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "Warning! Somehow trying to update a bucket that doesn't exist or is in the future!" ""
                                    in
                                    justModelUpdate prevModel

                                Just newSugarSale ->
                                    justModelUpdate
                                        { prevModel
                                            | sugarSale =
                                                Just newSugarSale
                                        }

        UserBuyFetched userAddress bucketId fetchResult ->
            case fetchResult of
                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching buy for user" ( userAddress, bucketId, fetchResult )
                    in
                    justModelUpdate prevModel

                Ok userBuy ->
                    let
                        bucketExitInfo =
                            buyToBucketExitInfo userBuy
                    in
                    case prevModel.sugarSale of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "Warning! Bucket value fetched but there is no sugarSale present!" ""
                            in
                            justModelUpdate prevModel

                        Just oldSugarSale ->
                            let
                                maybeNewSugarSale =
                                    oldSugarSale
                                        |> updatePastOrActiveBucketAt
                                            bucketId
                                            (\bucket ->
                                                { bucket
                                                    | userExitInfo = Just bucketExitInfo
                                                }
                                            )
                            in
                            case maybeNewSugarSale of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "Warning! Somehow trying to update a bucket that does not exist or is in the future!" ""
                                    in
                                    justModelUpdate prevModel

                                Just newSugarSale ->
                                    justModelUpdate
                                        { prevModel | sugarSale = Just newSugarSale }

        BucketClicked bucketId ->
            case prevModel.sugarSale of
                Nothing ->
                    let
                        _ =
                            Debug.log "Bucket clicked, but sugarSale isn't loaded! What??" ""
                    in
                    justModelUpdate prevModel

                Just sugarSale ->
                    let
                        newBucketView =
                            if bucketId == getActiveBucketId sugarSale prevModel.now prevModel.testMode then
                                ViewActive

                            else
                                ViewId bucketId
                    in
                    justModelUpdate
                        { prevModel
                            | bucketView = newBucketView
                        }


initSugarSale : Bool -> Time.Posix -> Time.Posix -> Maybe SugarSale
initSugarSale testMode saleStartTimestamp now =
    let
        bucketInterval =
            Config.sugarSaleBucketInterval testMode

        allBuckets =
            List.Extra.iterate
                (\lastBucketAdded ->
                    let
                        nextBucketStartTime =
                            TimeHelpers.add
                                lastBucketAdded.startTime
                                bucketInterval
                    in
                    if TimeHelpers.compare nextBucketStartTime now == GT then
                        Nothing

                    else
                        Just <|
                            Bucket
                                nextBucketStartTime
                                Nothing
                                Nothing
                )
                (Bucket
                    saleStartTimestamp
                    Nothing
                    Nothing
                )
    in
    Maybe.map2
        (SugarSale saleStartTimestamp)
        (List.Extra.init allBuckets)
        (List.Extra.last allBuckets)


addNewActiveBucketIfNeeded : Time.Posix -> Bool -> SugarSale -> SugarSale
addNewActiveBucketIfNeeded now testMode prevSugarSale =
    let
        nextBucketStartTime =
            TimeHelpers.add
                prevSugarSale.activeBucket.startTime
                (Config.sugarSaleBucketInterval testMode)
    in
    if TimeHelpers.compare nextBucketStartTime now /= GT then
        { prevSugarSale
            | pastBuckets =
                List.append
                    prevSugarSale.pastBuckets
                    [ prevSugarSale.activeBucket ]
            , activeBucket =
                Bucket
                    nextBucketStartTime
                    Nothing
                    Nothing
        }

    else
        prevSugarSale


fetchInfoForVisibleNonFutureBucketsCmd : Model -> Cmd Msg
fetchInfoForVisibleNonFutureBucketsCmd model =
    case model.sugarSale of
        Just sugarSale ->
            visibleBucketIds sugarSale model.bucketView model.now model.testMode
                |> List.map
                    (\id ->
                        if id <= getActiveBucketId sugarSale model.now model.testMode then
                            Cmd.batch
                                [ SugarSaleContract.getTotalValueEnteredForBucket
                                    (httpProvider model.testMode)
                                    id
                                    (BucketValueEnteredFetched id)
                                , case Wallet.userInfo model.wallet of
                                    Just userInfo ->
                                        SugarSaleContract.getUserBuyForBucket
                                            (httpProvider model.testMode)
                                            userInfo.address
                                            id
                                            (UserBuyFetched userInfo.address id)

                                    Nothing ->
                                        Cmd.none
                                ]

                        else
                            Cmd.none
                     -- Don't try to fetch values for future buckets
                    )
                |> Cmd.batch

        _ ->
            Cmd.none


httpProvider : Bool -> HttpProvider
httpProvider test =
    if test then
        EthHelpers.httpProviderForFactory <| Token KovanDai

    else
        Debug.todo "non-test mode not yet defined for httpProvider"


fetchSaleStartTimestampCmd : Bool -> Cmd Msg
fetchSaleStartTimestampCmd test =
    SugarSaleContract.getSaleStartTimestampCmd
        (httpProvider test)
        SaleStartTimestampFetched


clearSugarSaleExitInfo : SugarSale -> SugarSale
clearSugarSaleExitInfo =
    updateAllPastOrActiveBuckets
        (\bucket ->
            { bucket | userExitInfo = Nothing }
        )


runCmdDown : CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            UpdateResult
                { prevModel
                    | wallet = wallet
                    , sugarSale =
                        Maybe.map
                            clearSugarSaleExitInfo
                            prevModel.sugarSale
                }
                Cmd.none
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Refresh
