module BucketSale.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import BucketSale.Types exposing (..)
import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.BucketSale.Wrappers as BucketSaleWrappers
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address, HttpProvider)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import List.Extra
import Maybe.Extra
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
          , bucketSale = Nothing
          , totalTokensExited = Nothing
          , userFryBalance = Nothing
          , bucketView = ViewCurrent
          , daiInput = ""
          , dumbCheckboxesClicked = ( False, False )
          , daiAmount = Nothing
          , referrer = maybeReferrer
          , allowanceState = Loading
          , exitInfo = Nothing
          }
        , Cmd.batch
            ([ fetchSaleStartTimestampCmd testMode
             , fetchTotalTokensExitedCmd testMode
             , Task.perform TimezoneGot Time.here
             ]
                ++ (case Wallet.userInfo wallet of
                        Just userInfo ->
                            [ fetchUserExitInfoCmd userInfo testMode
                            , fetchUserAllowanceForSaleCmd userInfo testMode
                            , fetchUserFryBalance userInfo testMode
                            ]

                        Nothing ->
                            []
                   )
            )
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
                        [ fetchTotalTokensExitedCmd prevModel.testMode ]
                            ++ (Maybe.map
                                    (\userInfo ->
                                        [ fetchUserExitInfoCmd userInfo prevModel.testMode
                                        , fetchUserAllowanceForSaleCmd userInfo prevModel.testMode
                                        , fetchUserFryBalance userInfo prevModel.testMode
                                        ]
                                    )
                                    (Wallet.userInfo prevModel.wallet)
                                    |> Maybe.withDefault []
                               )
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
                }

        SaleStartTimestampFetched fetchResult ->
            case fetchResult of
                Ok startTimestampBigInt ->
                    if BigInt.compare startTimestampBigInt (BigInt.fromInt 0) == EQ then
                        Debug.todo "Failed to init bucket sale; sale startTime == 0."

                    else
                        let
                            startTimestamp =
                                TimeHelpers.secondsBigIntToPosixWithWarning startTimestampBigInt

                            newMaybeBucketSale =
                                case prevModel.bucketSale of
                                    Nothing ->
                                        case initBucketSale prevModel.testMode startTimestamp prevModel.now of
                                            Just s ->
                                                Just s

                                            Nothing ->
                                                Debug.todo "Failed to init bucket sale. Is it started yet?"

                                    _ ->
                                        prevModel.bucketSale
                        in
                        justModelUpdate
                            { prevModel
                                | bucketSale = newMaybeBucketSale
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
                    case prevModel.bucketSale of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "Warning! Bucket value fetched but there is no bucketSale present!" ""
                            in
                            justModelUpdate prevModel

                        Just oldBucketSale ->
                            let
                                valueEntered =
                                    TokenValue.tokenValue valueEnteredBigInt

                                maybeNewBucketSale =
                                    oldBucketSale
                                        |> updateBucketAt
                                            bucketId
                                            (\bucket ->
                                                { bucket | totalValueEntered = Just valueEntered }
                                            )
                            in
                            case maybeNewBucketSale of
                                Nothing ->
                                    let
                                        _ =
                                            Debug.log "Warning! Somehow trying to update a bucket that doesn't exist!" ""
                                    in
                                    justModelUpdate prevModel

                                Just newBucketSale ->
                                    justModelUpdate
                                        { prevModel
                                            | bucketSale =
                                                Just newBucketSale
                                        }

        UserBuyFetched userAddress bucketId fetchResult ->
            if (Wallet.userInfo prevModel.wallet |> Maybe.map .address) /= Just userAddress then
                justModelUpdate prevModel

            else
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
                        case prevModel.bucketSale of
                            Nothing ->
                                let
                                    _ =
                                        Debug.log "Warning! Bucket value fetched but there is no bucketSale present!" ""
                                in
                                justModelUpdate prevModel

                            Just oldBucketSale ->
                                let
                                    maybeNewBucketSale =
                                        oldBucketSale
                                            |> updateBucketAt
                                                bucketId
                                                (\bucket ->
                                                    { bucket
                                                        | userBuy = Just buy
                                                    }
                                                )
                                in
                                case maybeNewBucketSale of
                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "Warning! Somehow trying to update a bucket that does not exist or is in the future!" ""
                                        in
                                        justModelUpdate prevModel

                                    Just newBucketSale ->
                                        justModelUpdate
                                            { prevModel | bucketSale = Just newBucketSale }

        UserExitInfoFetched userAddress fetchResult ->
            if (Wallet.userInfo prevModel.wallet |> Maybe.map .address) /= Just userAddress then
                justModelUpdate prevModel

            else
                case fetchResult of
                    Err httpErr ->
                        let
                            _ =
                                Debug.log "http error when fetching userExitInfo" ( userAddress, httpErr )
                        in
                        justModelUpdate prevModel

                    Ok Nothing ->
                        let
                            _ =
                                Debug.log "Query contract returned an invalid result" userAddress
                        in
                        justModelUpdate prevModel

                    Ok (Just exitInfo) ->
                        justModelUpdate
                            { prevModel
                                | exitInfo = Just exitInfo
                            }

        UserFryBalanceFetched userAddress fetchResult ->
            if (Wallet.userInfo prevModel.wallet |> Maybe.map .address) /= Just userAddress then
                justModelUpdate prevModel

            else
                case fetchResult of
                    Err httpErr ->
                        let
                            _ =
                                Debug.log "http error when fetching userFryBalance" ( userAddress, httpErr )
                        in
                        justModelUpdate prevModel

                    Ok userFryBalance ->
                        justModelUpdate
                            { prevModel
                                | userFryBalance = Just userFryBalance
                            }

        TotalTokensExitedFetched fetchResult ->
            case fetchResult of
                Err httpErr ->
                    let
                        _ =
                            Debug.log "http error when fetching totalTokensExited" httpErr
                    in
                    justModelUpdate prevModel

                Ok totalTokensExited ->
                    justModelUpdate
                        { prevModel
                            | totalTokensExited = Just totalTokensExited
                        }

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

        ClaimClicked userInfo exitInfo ->
            let
                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( ExitMined, Nothing )
                            , onSign = Nothing
                            , onBroadcast = Nothing
                            }

                        txParams =
                            BucketSaleWrappers.exitMany
                                userInfo.address
                                exitInfo.exitableBuckets
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

        BucketClicked bucketId ->
            case prevModel.bucketSale of
                Nothing ->
                    let
                        _ =
                            Debug.log "Bucket clicked, but bucketSale isn't loaded! What??" ""
                    in
                    justModelUpdate prevModel

                Just bucketSale ->
                    let
                        newBucketView =
                            if bucketId == getCurrentBucketId bucketSale prevModel.now prevModel.testMode then
                                ViewCurrent

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
                            BucketSaleWrappers.unlockDai prevModel.testMode
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

        EnterButtonClicked userInfo bucketId daiAmount maybeReferrer ->
            let
                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( EnterMined, Nothing )
                            , onSign = Just EnterSigned
                            , onBroadcast = Nothing
                            }

                        txParams =
                            BucketSaleWrappers.enter
                                userInfo.address
                                bucketId
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
                            BucketSaleWrappers.exit
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


initBucketSale : Bool -> Time.Posix -> Time.Posix -> Maybe BucketSale
initBucketSale testMode saleStartTime now =
    let
        numBuckets =
            TimeHelpers.sub
                now
                saleStartTime
                |> TimeHelpers.posixToSeconds
                |> (\seconds ->
                        (seconds // (Config.bucketSaleBucketInterval testMode |> TimeHelpers.posixToSeconds))
                            + 1
                   )
    in
    if numBuckets <= 0 then
        Nothing

    else
        Just <|
            BucketSale
                saleStartTime
                (List.range 0 (numBuckets - 1)
                    |> List.map
                        (\id ->
                            Bucket
                                (TimeHelpers.add
                                    saleStartTime
                                    (TimeHelpers.mul
                                        (Config.bucketSaleBucketInterval testMode)
                                        id
                                    )
                                )
                                Nothing
                                Nothing
                        )
                )


fetchUserExitInfoCmd : UserInfo -> Bool -> Cmd Msg
fetchUserExitInfoCmd userInfo testMode =
    BucketSaleWrappers.getUserExitInfo
        testMode
        userInfo.address
        (UserExitInfoFetched userInfo.address)


fetchUserAllowanceForSaleCmd : UserInfo -> Bool -> Cmd Msg
fetchUserAllowanceForSaleCmd userInfo testMode =
    Contracts.Wrappers.getAllowanceCmd
        (if testMode then
            KovanDai

         else
            EthDai
        )
        userInfo.address
        (Config.bucketSaleAddress testMode)
        AllowanceFetched


fetchSaleStartTimestampCmd : Bool -> Cmd Msg
fetchSaleStartTimestampCmd testMode =
    BucketSaleWrappers.getSaleStartTimestampCmd
        testMode
        SaleStartTimestampFetched


fetchTotalTokensExitedCmd : Bool -> Cmd Msg
fetchTotalTokensExitedCmd testMode =
    BucketSaleWrappers.getTotalExitedTokens
        testMode
        TotalTokensExitedFetched


fetchUserFryBalance : UserInfo -> Bool -> Cmd Msg
fetchUserFryBalance userInfo testMode =
    BucketSaleWrappers.getFryBalance
        testMode
        userInfo.address
        (UserFryBalanceFetched userInfo.address)


clearBucketSaleExitInfo : BucketSale -> BucketSale
clearBucketSaleExitInfo =
    updateAllBuckets
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
                    , bucketSale =
                        Maybe.map
                            clearBucketSaleExitInfo
                            prevModel.bucketSale
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
