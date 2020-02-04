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
import Dict exposing (Dict)
import Eth
import Eth.Types exposing (Address, HttpProvider, Tx, TxHash, TxReceipt)
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
          , enterUXModel = initEnterUXModel maybeReferrer
          , userExitInfo = Nothing
          , trackedTxs = []
          , confirmModal = Nothing
          , showReferralModal = False
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
                            , fetchUserFryBalanceCmd userInfo testMode
                            ]

                        Nothing ->
                            []
                   )
            )
        )

    else
        Debug.todo "must use test mode"


initEnterUXModel : Maybe Address -> EnterUXModel
initEnterUXModel maybeReferrer =
    { daiInput = ""
    , daiAmount = Nothing
    , referrer = maybeReferrer
    , allowanceState = Loading
    }


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
                fetchUserInfoCmds =
                    Cmd.batch <|
                        (Maybe.map
                            (\userInfo ->
                                [ fetchUserExitInfoCmd userInfo prevModel.testMode
                                , fetchUserAllowanceForSaleCmd userInfo prevModel.testMode
                                , fetchUserFryBalanceCmd userInfo prevModel.testMode
                                ]
                            )
                            (Wallet.userInfo prevModel.wallet)
                            |> Maybe.withDefault []
                        )
            in
            UpdateResult
                prevModel
                (Cmd.batch
                    [ fetchTotalTokensExitedCmd prevModel.testMode
                    , fetchUserInfoCmds
                    ]
                )
                ChainCmd.none
                []

        UpdateNow newNow ->
            let
                cmd =
                    case ( prevModel.bucketSale, prevModel.bucketView ) of
                        ( Nothing, _ ) ->
                            Cmd.none

                        ( Just (Err _), _ ) ->
                            Cmd.none

                        ( _, ViewId _ ) ->
                            Cmd.none

                        ( Just (Ok bucketSale), ViewCurrent ) ->
                            let
                                newFocusedId =
                                    getCurrentBucketId bucketSale newNow prevModel.testMode
                            in
                            if newFocusedId /= getCurrentBucketId bucketSale prevModel.now prevModel.testMode then
                                let
                                    _ =
                                        Debug.log "fetching" ""
                                in
                                fetchBucketDataCmd
                                    newFocusedId
                                    (Wallet.userInfo prevModel.wallet)
                                    prevModel.testMode

                            else
                                Cmd.none
            in
            UpdateResult
                { prevModel
                    | now = newNow
                }
                cmd
                ChainCmd.none
                []

        SaleStartTimestampFetched fetchResult ->
            case fetchResult of
                Ok startTimestampBigInt ->
                    if BigInt.compare startTimestampBigInt (BigInt.fromInt 0) == EQ then
                        justModelUpdate
                            { prevModel
                                | bucketSale = Just <| Err "The sale has not been initialized yet."
                            }

                    else
                        let
                            startTimestamp =
                                TimeHelpers.secondsBigIntToPosixWithWarning startTimestampBigInt

                            ( newMaybeResultBucketSale, cmd ) =
                                case prevModel.bucketSale of
                                    Nothing ->
                                        case initBucketSale prevModel.testMode startTimestamp prevModel.now of
                                            Ok sale ->
                                                ( Just <| Ok sale
                                                , fetchBucketDataCmd
                                                    (getCurrentBucketId
                                                        sale
                                                        prevModel.now
                                                        prevModel.testMode
                                                    )
                                                    (Wallet.userInfo prevModel.wallet)
                                                    prevModel.testMode
                                                )

                                            Err errStr ->
                                                ( Just <| Err errStr
                                                , Cmd.none
                                                )

                                    _ ->
                                        ( prevModel.bucketSale
                                        , Cmd.none
                                        )
                        in
                        UpdateResult
                            { prevModel
                                | bucketSale = newMaybeResultBucketSale
                                , saleStartTime = Just startTimestamp
                            }
                            cmd
                            ChainCmd.none
                            []

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

                Ok valueEntered ->
                    case prevModel.bucketSale of
                        Just (Ok oldBucketSale) ->
                            let
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
                                                Just (Ok newBucketSale)
                                        }

                        somethingElse ->
                            let
                                _ =
                                    Debug.log "Warning! Bucket value fetched but there is no bucketSale present!" somethingElse
                            in
                            justModelUpdate prevModel

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
                            Just (Ok oldBucketSale) ->
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
                                            { prevModel | bucketSale = Just <| Ok newBucketSale }

                            somethingElse ->
                                let
                                    _ =
                                        Debug.log "Warning! Bucket value fetched but there is no bucketSale present!" somethingElse
                                in
                                justModelUpdate prevModel

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
                                | userExitInfo = Just exitInfo
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
                    case prevModel.enterUXModel.allowanceState of
                        UnlockMining ->
                            if allowance == EthHelpers.maxUintValue then
                                justModelUpdate
                                    { prevModel
                                        | enterUXModel =
                                            let
                                                oldEnterUXModel =
                                                    prevModel.enterUXModel
                                            in
                                            { oldEnterUXModel
                                                | allowanceState =
                                                    Loaded <| TokenValue.tokenValue allowance
                                            }
                                    }

                            else
                                justModelUpdate prevModel

                        _ ->
                            justModelUpdate
                                { prevModel
                                    | enterUXModel =
                                        let
                                            oldEnterUXModel =
                                                prevModel.enterUXModel
                                        in
                                        { oldEnterUXModel
                                            | allowanceState =
                                                Loaded <|
                                                    TokenValue.tokenValue allowance
                                        }
                                }

        FocusToBucket bucketId ->
            case prevModel.bucketSale of
                Just (Ok bucketSale) ->
                    let
                        newBucketView =
                            if bucketId == getCurrentBucketId bucketSale prevModel.now prevModel.testMode then
                                ViewCurrent

                            else
                                ViewId
                                    (bucketId
                                        |> min Config.bucketSaleNumBuckets
                                        |> max
                                            (getCurrentBucketId
                                                bucketSale
                                                prevModel.now
                                                prevModel.testMode
                                            )
                                    )

                        maybeBucketData =
                            getBucketInfo
                                bucketSale
                                (getFocusedBucketId
                                    bucketSale
                                    newBucketView
                                    prevModel.now
                                    prevModel.testMode
                                )
                                prevModel.now
                                prevModel.testMode
                                |> (\fetchedBucketInfo ->
                                        case fetchedBucketInfo of
                                            ValidBucket bucketInfo ->
                                                Just bucketInfo.bucketData

                                            _ ->
                                                Nothing
                                   )

                        cmd =
                            maybeBucketData
                                |> Maybe.map
                                    (\bucketData ->
                                        case ( bucketData.totalValueEntered, bucketData.userBuy ) of
                                            ( Just _, Just _ ) ->
                                                Cmd.none

                                            ( Nothing, _ ) ->
                                                fetchBucketDataCmd
                                                    bucketId
                                                    (Wallet.userInfo prevModel.wallet)
                                                    prevModel.testMode

                                            ( Just _, Nothing ) ->
                                                case Wallet.userInfo prevModel.wallet of
                                                    Just userInfo ->
                                                        fetchBucketUserBuyCmd
                                                            bucketId
                                                            userInfo
                                                            prevModel.testMode

                                                    Nothing ->
                                                        Cmd.none
                                    )
                                |> Maybe.withDefault Cmd.none
                    in
                    UpdateResult
                        { prevModel
                            | bucketView = newBucketView
                        }
                        cmd
                        ChainCmd.none
                        []

                somethingElse ->
                    let
                        _ =
                            Debug.log "Bucket clicked, but bucketSale isn't loaded! What??" somethingElse
                    in
                    justModelUpdate prevModel

        DaiInputChanged input ->
            justModelUpdate
                { prevModel
                    | enterUXModel =
                        let
                            oldEnterUXModel =
                                prevModel.enterUXModel
                        in
                        { oldEnterUXModel
                            | daiInput = input
                            , daiAmount =
                                if input == "" then
                                    Nothing

                                else
                                    Just <| validateDaiInput input
                        }
                }

        ReferralIndicatorClicked ->
            justModelUpdate
                { prevModel
                    | showReferralModal = True
                }

        CloseReferralModal ->
            justModelUpdate
                { prevModel
                    | showReferralModal = False
                }

        UnlockDaiButtonClicked ->
            let
                ( trackedTxId, newTrackedTxs ) =
                    prevModel.trackedTxs
                        |> trackNewTx
                            (TrackedTx
                                Nothing
                                "Unlock DAI"
                                Signing
                            )

                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( TxMined trackedTxId Unlock, Nothing )
                            , onSign = Just <| TxSigned trackedTxId Unlock
                            , onBroadcast = Just <| TxBroadcast trackedTxId Unlock
                            }

                        txParams =
                            BucketSaleWrappers.unlockDai prevModel.testMode
                                |> Eth.toSend
                    in
                    ChainCmd.custom customSend txParams
            in
            UpdateResult
                { prevModel
                    | trackedTxs = newTrackedTxs
                }
                Cmd.none
                chainCmd
                []

        EnterButtonClicked enterInfo ->
            justModelUpdate
                { prevModel
                    | confirmModal = Just enterInfo
                }

        CancelClicked ->
            justModelUpdate
                { prevModel
                    | confirmModal = Nothing
                }

        ConfirmClicked enterInfo ->
            let
                ( trackedTxId, newTrackedTxs ) =
                    prevModel.trackedTxs
                        |> trackNewTx
                            (TrackedTx
                                Nothing
                                ("Bid on bucket "
                                    ++ String.fromInt enterInfo.bucketId
                                    ++ " with "
                                    ++ TokenValue.toConciseString enterInfo.amount
                                    ++ " DAI"
                                )
                                Signing
                            )

                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( TxMined trackedTxId Enter, Nothing )
                            , onSign = Just <| TxSigned trackedTxId Enter
                            , onBroadcast = Just <| TxBroadcast trackedTxId Enter
                            }

                        txParams =
                            BucketSaleWrappers.enter
                                enterInfo.userInfo.address
                                enterInfo.bucketId
                                enterInfo.amount
                                enterInfo.maybeReferrer
                                prevModel.testMode
                                |> Eth.toSend
                    in
                    ChainCmd.custom customSend txParams
            in
            UpdateResult
                { prevModel
                    | trackedTxs = newTrackedTxs
                    , confirmModal = Nothing
                }
                Cmd.none
                chainCmd
                []

        ClaimClicked userInfo exitInfo ->
            let
                ( trackedTxId, newTrackedTxs ) =
                    prevModel.trackedTxs
                        |> trackNewTx
                            (TrackedTx
                                Nothing
                                "Claim FRY"
                                Signing
                            )

                chainCmd =
                    let
                        customSend =
                            { onMined = Just ( TxMined trackedTxId Exit, Nothing )
                            , onSign = Just <| TxSigned trackedTxId Exit
                            , onBroadcast = Just <| TxBroadcast trackedTxId Exit
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
                { prevModel
                    | trackedTxs = newTrackedTxs
                }
                Cmd.none
                chainCmd
                []

        TxSigned trackedTxId txType txHashResult ->
            case txHashResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "Error signing tx" ( txType, errStr )
                    in
                    justModelUpdate
                        { prevModel
                            | trackedTxs =
                                prevModel.trackedTxs
                                    |> updateTrackedTxStatus trackedTxId Rejected
                        }

                Ok txHash ->
                    let
                        newTrackedTxs =
                            prevModel.trackedTxs
                                |> updateTrackedTxStatus trackedTxId Mining

                        newEnterUXModel =
                            case txType of
                                Unlock ->
                                    let
                                        oldEnterUXModel =
                                            prevModel.enterUXModel
                                    in
                                    { oldEnterUXModel
                                        | allowanceState = UnlockMining
                                    }

                                Enter ->
                                    let
                                        oldEnterUXModel =
                                            prevModel.enterUXModel
                                    in
                                    { oldEnterUXModel
                                        | daiInput = ""
                                        , daiAmount = Nothing
                                    }

                                _ ->
                                    prevModel.enterUXModel
                    in
                    justModelUpdate
                        { prevModel
                            | trackedTxs = newTrackedTxs
                            , enterUXModel = newEnterUXModel
                        }

        TxBroadcast trackedTxId txType txResult ->
            case txResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "Error broadcasting tx" ( txType, errStr )
                    in
                    justModelUpdate
                        { prevModel
                            | trackedTxs =
                                prevModel.trackedTxs
                                    |> updateTrackedTxStatus trackedTxId Failed
                        }

                Ok tx ->
                    let
                        newTrackedTxs =
                            prevModel.trackedTxs
                                |> updateTrackedTxStatus trackedTxId Mining

                        newEnterUXModel =
                            case txType of
                                Unlock ->
                                    let
                                        oldEnterUXModel =
                                            prevModel.enterUXModel
                                    in
                                    { oldEnterUXModel
                                        | allowanceState = Loaded (TokenValue.tokenValue EthHelpers.maxUintValue)
                                    }

                                _ ->
                                    prevModel.enterUXModel
                    in
                    justModelUpdate
                        { prevModel
                            | trackedTxs = newTrackedTxs
                            , enterUXModel = newEnterUXModel
                        }

        TxMined trackedTxId txType txReceiptResult ->
            case txReceiptResult of
                Err errStr ->
                    let
                        _ =
                            Debug.log "Error mining tx" ( txType, errStr )
                    in
                    justModelUpdate
                        { prevModel
                            | trackedTxs =
                                prevModel.trackedTxs
                                    |> updateTrackedTxStatus trackedTxId Failed
                        }

                Ok txReceipt ->
                    let
                        newTrackedTxs =
                            prevModel.trackedTxs
                                |> updateTrackedTxStatus trackedTxId Mined

                        cmd =
                            case ( txType, Wallet.userInfo prevModel.wallet ) of
                                ( Exit, Just userInfo ) ->
                                    Cmd.batch
                                        [ fetchUserExitInfoCmd
                                            userInfo
                                            prevModel.testMode
                                        , fetchUserFryBalanceCmd
                                            userInfo
                                            prevModel.testMode
                                        ]

                                _ ->
                                    Cmd.none
                    in
                    UpdateResult
                        { prevModel
                            | trackedTxs = newTrackedTxs
                        }
                        cmd
                        ChainCmd.none
                        []


initBucketSale : Bool -> Time.Posix -> Time.Posix -> Result String BucketSale
initBucketSale testMode saleStartTime now =
    if TimeHelpers.compare saleStartTime now == GT then
        Err <|
            "Sale hasn't started yet. You are "
                ++ (TimeHelpers.sub
                        saleStartTime
                        now
                        |> TimeHelpers.toConciseIntervalString
                   )
                ++ " too early!"

    else
        Ok <|
            BucketSale
                saleStartTime
                (List.range 0 (Config.bucketSaleNumBuckets - 1)
                    |> List.map
                        (\id ->
                            BucketData
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


fetchBucketDataCmd : Int -> Maybe UserInfo -> Bool -> Cmd Msg
fetchBucketDataCmd id maybeUserInfo testMode =
    Cmd.batch
        [ fetchTotalValueEnteredCmd id testMode
        , case maybeUserInfo of
            Just userInfo ->
                fetchBucketUserBuyCmd id userInfo testMode

            Nothing ->
                Cmd.none
        ]


fetchTotalValueEnteredCmd : Int -> Bool -> Cmd Msg
fetchTotalValueEnteredCmd id testMode =
    BucketSaleWrappers.getTotalValueEnteredForBucket
        testMode
        id
        (BucketValueEnteredFetched id)


fetchBucketUserBuyCmd : Int -> UserInfo -> Bool -> Cmd Msg
fetchBucketUserBuyCmd id userInfo testMode =
    BucketSaleWrappers.getUserBuyForBucket
        testMode
        userInfo.address
        id
        (UserBuyFetched userInfo.address id)


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


fetchUserFryBalanceCmd : UserInfo -> Bool -> Cmd Msg
fetchUserFryBalanceCmd userInfo testMode =
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


trackNewTx : TrackedTx -> List TrackedTx -> ( Int, List TrackedTx )
trackNewTx newTrackedTx prevTrackedTxs =
    ( List.length prevTrackedTxs
    , List.append
        prevTrackedTxs
        [ newTrackedTx ]
    )


updateTrackedTxStatus : Int -> TxStatus -> List TrackedTx -> List TrackedTx
updateTrackedTxStatus id newStatus =
    List.Extra.updateAt id
        (\trackedTx ->
            { trackedTx | status = newStatus }
        )


runCmdDown : CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet newWallet ->
            let
                newBucketSale =
                    (Maybe.map << Result.map)
                        clearBucketSaleExitInfo
                        prevModel.bucketSale
            in
            UpdateResult
                { prevModel
                    | wallet = newWallet
                    , bucketSale = newBucketSale
                    , userFryBalance = Nothing
                    , userExitInfo = Nothing
                    , enterUXModel =
                        let
                            oldEnterUXModel =
                                prevModel.enterUXModel
                        in
                        { oldEnterUXModel
                            | allowanceState = Loading
                        }
                }
                (case ( Wallet.userInfo newWallet, newBucketSale ) of
                    ( Just userInfo, Just (Ok bucketSale) ) ->
                        Cmd.batch
                            [ fetchUserAllowanceForSaleCmd
                                userInfo
                                prevModel.testMode
                            , fetchUserFryBalanceCmd
                                userInfo
                                prevModel.testMode
                            , fetchBucketDataCmd
                                (getFocusedBucketId
                                    bucketSale
                                    prevModel.bucketView
                                    prevModel.now
                                    prevModel.testMode
                                )
                                (Just userInfo)
                                prevModel.testMode
                            ]

                    _ ->
                        Cmd.none
                )
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            justModelUpdate prevModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 3000 <| always Refresh
        , Time.every 500 UpdateNow
        ]
