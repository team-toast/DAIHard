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
            case prevModel.sugarSale of
                Nothing ->
                    let
                        _ =
                            Debug.log "Warning! Bucket value fetched but there is no sugarSale present!" ""
                    in
                    justModelUpdate prevModel

                Just oldSugarSale ->
                    case fetchResult of
                        Ok valueEnteredBigInt ->
                            let
                                valueEntered =
                                    TokenValue.tokenValue valueEnteredBigInt
                            in
                            justModelUpdate
                                { prevModel
                                    | sugarSale =
                                        Just <|
                                            if bucketId == List.length oldSugarSale.pastBuckets then
                                                let
                                                    oldActiveBucket =
                                                        oldSugarSale.activeBucket
                                                in
                                                { oldSugarSale
                                                    | activeBucket =
                                                        { oldActiveBucket
                                                            | totalValueEntered = Just valueEntered
                                                        }
                                                }

                                            else if bucketId < List.length oldSugarSale.pastBuckets then
                                                { oldSugarSale
                                                    | pastBuckets =
                                                        oldSugarSale.pastBuckets
                                                            |> List.Extra.updateAt bucketId
                                                                (\bucket ->
                                                                    { bucket | totalValueEntered = Just valueEntered }
                                                                )
                                                }

                                            else
                                                let
                                                    _ =
                                                        Debug.log "Warning! Somehow trying to update a bucket that doesn't exist!" ""
                                                in
                                                oldSugarSale
                                }

                        Err httpErr ->
                            let
                                _ =
                                    Debug.log "http error when fetching total bucket value entered" ( bucketId, fetchResult )
                            in
                            justModelUpdate prevModel


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
                            SugarSaleContract.getTotalValueEnteredForBucket
                                (httpProvider model.testMode)
                                id
                                (SugarSale.Types.BucketValueEnteredFetched id)

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


runCmdDown : CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            UpdateResult
                { prevModel | wallet = wallet }
                Cmd.none
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Refresh
