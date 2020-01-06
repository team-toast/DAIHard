module SugarSale.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.SugarSale.Wrappers as SugarSale
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address, HttpProvider)
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


init : Maybe Address -> Bool -> Wallet.State -> Time.Posix -> ( Model, Cmd Msg )
init maybeReferrer testMode wallet now =
    if testMode then
        ( { wallet = wallet
          , testMode = testMode
          , now = now
          , timezone = Nothing
          , saleStartTime = Nothing
          , sugarSale = Nothing
          , bucketView = ViewActive
          , daiInput = ""
          , dumbCheckboxesClicked = ( False, False )
          , daiAmount = Nothing
          , referrer = maybeReferrer
          , allowanceState = Loading
          }
        , Cmd.batch
            [ fetchSaleStartTimestampCmd testMode
            , Task.perform TimezoneGot Time.here
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

        TimezoneGot tz ->
            justModelUpdate
                { prevModel | timezone = Just tz }

        Refresh ->
            let
                cmd =
                    Cmd.batch <|
                        [ fetchInfoForVisibleNonFutureBucketsCmd prevModel
                        , Maybe.map
                            (\userInfo -> fetchUserAllowanceForSaleCmd userInfo prevModel.testMode)
                            (Wallet.userInfo prevModel.wallet)
                            |> Maybe.withDefault Cmd.none
                        ]
            in
            UpdateResult
                prevModel
                cmd
                ChainCmd.none
                []

        UpdateNow newNow ->
            justModelUpdate
                { prevModel
                    | now = newNow
                    , sugarSale =
                        Maybe.map
                            (addNewActiveBucketIfNeeded newNow prevModel.testMode)
                            prevModel.sugarSale
                }

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
                            Debug.log "http error when fetching buy for user" ( userAddress, bucketId, httpErr )
                    in
                    justModelUpdate prevModel

                Ok bindingBuy ->
                    let
                        buy =
                            buyFromBindingBuy bindingBuy
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
                                                    | userBuy = Just buy
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

        AllowanceFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching user allowance" httpErr
                    in
                    justModelUpdate prevModel

                Ok allowance ->
                    case prevModel.allowanceState of
                        UnlockMining ->
                            if allowance == EthHelpers.maxUintValue then
                                justModelUpdate
                                    { prevModel
                                        | allowanceState =
                                            Loaded <| TokenValue.tokenValue allowance
                                    }

                            else
                                justModelUpdate prevModel

                        _ ->
                            justModelUpdate
                                { prevModel
                                    | allowanceState =
                                        Loaded <|
                                            TokenValue.tokenValue allowance
                                }

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

        DaiInputChanged input ->
            justModelUpdate
                { prevModel
                    | daiInput = input
                    , daiAmount =
                        if input == "" then
                            Nothing

                        else
                            Just <| validateDaiInput input
                }

        FirstDumbCheckboxClicked flag ->
            justModelUpdate
                { prevModel
                    | dumbCheckboxesClicked =
                        ( flag, Tuple.second prevModel.dumbCheckboxesClicked )
                }

        SecondDumbCheckboxClicked flag ->
            justModelUpdate
                { prevModel
                    | dumbCheckboxesClicked =
                        ( Tuple.first prevModel.dumbCheckboxesClicked, flag )
                }

        UnlockDaiButtonClicked ->
            let
                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( DaiUnlockMined, Nothing )
                            , onSign = Just DaiUnlockSigned
                            , onBroadcast = Nothing
                            }

                        txParams =
                            SugarSale.unlockDai prevModel.testMode
                                |> Eth.toSend
                    in
                    ChainCmd.custom customSend txParams
            in
            UpdateResult
                prevModel
                Cmd.none
                chainCmd
                []

        DaiUnlockSigned txHashResult ->
            case txHashResult of
                Ok txHash ->
                    justModelUpdate
                        { prevModel
                            | allowanceState = UnlockMining
                        }

                Err errStr ->
                    let
                        _ =
                            Debug.log "Error signing unlock" errStr
                    in
                    justModelUpdate prevModel

        DaiUnlockMined txReceiptResult ->
            let
                _ =
                    Debug.log "txReceiptResult for daiUnlockMined" txReceiptResult
            in
            justModelUpdate
                { prevModel
                    | allowanceState = Loaded (TokenValue.tokenValue EthHelpers.maxUintValue)
                }

        EnterButtonClicked userInfo daiAmount maybeReferrer ->
            let
                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( EnterMined, Nothing )
                            , onSign = Just EnterSigned
                            , onBroadcast = Nothing
                            }

                        txParams =
                            SugarSale.enter
                                userInfo.address
                                daiAmount
                                maybeReferrer
                                prevModel.testMode
                                |> Eth.toSend
                    in
                    ChainCmd.custom customSend txParams
            in
            UpdateResult
                prevModel
                Cmd.none
                chainCmd
                []

        EnterSigned txHashResult ->
            let
                _ =
                    Debug.log "Signed enter tx!" ""
            in
            justModelUpdate
                { prevModel
                    | daiInput = ""
                    , daiAmount = Nothing
                }

        EnterMined txReceiptResult ->
            let
                _ =
                    Debug.log "Mined enter tx!" txReceiptResult
            in
            justModelUpdate prevModel

        ExitButtonClicked userInfo bucketId ->
            let
                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( ExitMined, Nothing )
                            , onSign = Just ExitSigned
                            , onBroadcast = Nothing
                            }

                        txParams =
                            SugarSale.exit
                                userInfo.address
                                bucketId
                                prevModel.testMode
                                |> Eth.toSend
                    in
                    ChainCmd.custom customSend txParams
            in
            UpdateResult
                prevModel
                Cmd.none
                chainCmd
                []

        ExitSigned txHashResult ->
            let
                _ =
                    Debug.log "ExitSigned" txHashResult
            in
            justModelUpdate prevModel

        ExitMined txReceiptResult ->
            let
                _ =
                    Debug.log "ExitMined" txReceiptResult
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
                            Cmd.batch
                                [ SugarSale.getTotalValueEnteredForBucket
                                    model.testMode
                                    id
                                    (BucketValueEnteredFetched id)
                                , case Wallet.userInfo model.wallet of
                                    Just userInfo ->
                                        SugarSale.getUserBuyForBucket
                                            model.testMode
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


fetchUserAllowanceForSaleCmd : UserInfo -> Bool -> Cmd Msg
fetchUserAllowanceForSaleCmd userInfo testMode =
    Contracts.Wrappers.getAllowanceCmd
        (if testMode then
            KovanDai

         else
            EthDai
        )
        userInfo.address
        (Config.sugarSaleAddress testMode)
        AllowanceFetched


fetchSaleStartTimestampCmd : Bool -> Cmd Msg
fetchSaleStartTimestampCmd testMode =
    SugarSale.getSaleStartTimestampCmd
        testMode
        SaleStartTimestampFetched


clearSugarSaleExitInfo : SugarSale -> SugarSale
clearSugarSaleExitInfo =
    updateAllPastOrActiveBuckets
        (\bucket ->
            { bucket | userBuy = Nothing }
        )


validateDaiInput : String -> Result String TokenValue
validateDaiInput input =
    case String.toFloat input of
        Just floatVal ->
            if floatVal <= 0 then
                Err "Value must be greater than 0"

            else
                Ok <| TokenValue.fromFloatWithWarning floatVal

        Nothing ->
            Err "Can't interpret that number"


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
                    , allowanceState = Loading
                }
                (Wallet.userInfo wallet
                    |> Maybe.map
                        (\userInfo ->
                            fetchUserAllowanceForSaleCmd
                                userInfo
                                prevModel.testMode
                        )
                    |> Maybe.withDefault Cmd.none
                )
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 <| always Refresh
        , Time.every 500 UpdateNow
        ]
