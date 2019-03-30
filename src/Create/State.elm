module Create.State exposing (init, subscriptions, update, updateUserInfo)

import BigInt
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Constants exposing (..)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Create.PMWizard.State as PMWizard
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Margin
import Maybe.Extra
import Routing
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


init : EthHelpers.EthNode -> Maybe UserInfo -> ( Model, Cmd Msg, ChainCmd Msg )
init node userInfo =
    let
        model =
            { node = node
            , userInfo = userInfo
            , inputs = initialInputs
            , showFiatTypeDropdown = False
            , addPMModal = Nothing
            , createParameters = Nothing
            , txChainStatus = NoTx
            }
    in
    ( model |> updateInputs initialInputs
    , Cmd.none
    , ChainCmd.none
    )


initialInputs =
    { openMode = CTypes.SellerOpened
    , daiAmount = ""
    , fiatType = "USD"
    , fiatAmount = ""
    , margin = "0"
    , paymentMethods = []
    , autorecallInterval = Time.millisToPosix <| 1000 * 60 * 60 * 2
    , autoabortInterval = Time.millisToPosix <| 1000 * 60 * 30
    , autoreleaseInterval = Time.millisToPosix <| 1000 * 60 * 60 * 12
    }


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }
        |> updateInputs model.inputs


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        ChangeType openMode ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel | inputs = { oldInputs | openMode = openMode } }

        TradeAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs

                newFiatAmountString =
                    recalculateFiatAmountString newAmountStr oldInputs.margin oldInputs.fiatType
                        |> Maybe.withDefault oldInputs.fiatAmount
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | daiAmount = newAmountStr
                            , fiatAmount = newFiatAmountString
                        }
                )

        FiatAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs

                newMarginString =
                    recalculateMarginString oldInputs.daiAmount newAmountStr oldInputs.fiatType
                        |> Maybe.withDefault oldInputs.margin
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | fiatAmount = newAmountStr
                            , margin = newMarginString
                        }
                )

        FiatTypeChanged newTypeStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | fiatType = newTypeStr
                            , margin =
                                recalculateMarginString oldInputs.daiAmount oldInputs.fiatAmount newTypeStr
                                    |> Maybe.withDefault oldInputs.margin
                        }
                )

        FiatTypeLostFocus ->
            justModelUpdate
                { prevModel
                    | showFiatTypeDropdown = False
                }

        MarginStringChanged newString ->
            let
                oldInputs =
                    prevModel.inputs

                newFiatAmount =
                    recalculateFiatAmountString oldInputs.daiAmount newString oldInputs.fiatType
                        |> Maybe.withDefault oldInputs.fiatAmount
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | margin = newString
                            , fiatAmount = newFiatAmount
                        }
                )

        AutorecallIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autorecallInterval = newTime })

        AutoabortIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autoabortInterval = newTime })

        AutoreleaseIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autoreleaseInterval = newTime })

        ShowCurrencyDropdown flag ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                ({ prevModel
                    | showFiatTypeDropdown = flag
                 }
                    |> (if flag then
                            updateInputs { oldInputs | fiatType = "" }

                        else
                            identity
                       )
                )

        OpenPMWizard ->
            justModelUpdate { prevModel | addPMModal = Just PMWizard.init }

        ClearDraft ->
            justModelUpdate { prevModel | inputs = initialInputs }

        BeginCreateProcess ->
            case prevModel.createParameters of
                Just parameters ->
                    { model = { prevModel | txChainStatus = FetchingFees }
                    , cmd =
                        Contracts.Wrappers.getExtraFeesCmd
                            prevModel.node
                            (TokenValue.getBigInt parameters.tradeAmount)
                            ExtraFeesFetched
                    , chainCmd = ChainCmd.none
                    , newRoute = Nothing
                    }

                Nothing ->
                    let
                        _ =
                            Debug.log "Trying to form fetchDevFee cmd, but can't find the contract parameters!" ""
                    in
                    justModelUpdate prevModel

        ExtraFeesFetched fetchResult ->
            case ( fetchResult, prevModel.userInfo, prevModel.createParameters ) of
                ( Ok fees, Just _, Just parameters ) ->
                    let
                        mainDepositAmount =
                            case parameters.openMode of
                                CTypes.BuyerOpened ->
                                    fees.buyerDeposit

                                CTypes.SellerOpened ->
                                    TokenValue.getBigInt parameters.tradeAmount

                        fullDepositAmount =
                            mainDepositAmount
                                |> BigInt.add fees.devFee
                                |> BigInt.add (TokenValue.getBigInt parameters.pokeReward)

                        txParams =
                            TokenContract.approve
                                daiAddress
                                factoryAddress
                                fullDepositAmount
                                |> Eth.toSend

                        customSend =
                            { onMined = Just ( ApproveMined, Nothing )
                            , onSign = Just ApproveSigned
                            , onBroadcast = Nothing
                            }

                        newModel =
                            { prevModel | txChainStatus = ApproveNeedsSig }
                    in
                    { model = newModel
                    , cmd = Cmd.none
                    , chainCmd = ChainCmd.custom customSend txParams
                    , newRoute = Nothing
                    }

                ( Err fetchErrStr, _, _ ) ->
                    let
                        _ =
                            Debug.log "Error fetching devFee: " fetchErrStr
                    in
                    justModelUpdate prevModel

                ( _, Nothing, _ ) ->
                    let
                        _ =
                            Debug.log "Metamask seems to be locked! I can't find the user address." ""
                    in
                    justModelUpdate prevModel

                ( _, _, Nothing ) ->
                    let
                        _ =
                            Debug.log "Can't create without a valid contract!" ""
                    in
                    justModelUpdate prevModel

        ApproveSigned result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = ApproveMining txHash }

                Err e ->
                    let
                        _ =
                            Debug.log "Error encountered when getting sig from user" e
                    in
                    justModelUpdate { prevModel | txChainStatus = TxError e }

        ApproveMined (Err errstr) ->
            let
                _ =
                    Debug.log "'approve' call mining error" errstr
            in
            justModelUpdate prevModel

        ApproveMined (Ok txReceipt) ->
            case prevModel.createParameters of
                Nothing ->
                    let
                        _ =
                            Debug.log "Can't find valid contract parameters. What the heck?????" ""
                    in
                    justModelUpdate prevModel

                Just createParameters ->
                    let
                        txParams =
                            Contracts.Wrappers.openTrade
                                createParameters
                                |> Eth.toSend

                        customSend =
                            { onMined = Just ( CreateMined, Nothing )
                            , onSign = Just CreateSigned
                            , onBroadcast = Nothing
                            }

                        newModel =
                            { prevModel | txChainStatus = CreateNeedsSig }
                    in
                    { model = newModel
                    , cmd = Cmd.none
                    , chainCmd = ChainCmd.custom customSend txParams
                    , newRoute = Nothing
                    }

        CreateSigned result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = CreateMining txHash }

                Err e ->
                    let
                        _ =
                            Debug.log "Error encountered when getting sig from user" e
                    in
                    justModelUpdate { prevModel | txChainStatus = TxError e }

        CreateMined (Err errstr) ->
            let
                _ =
                    Debug.log "error mining create contract tx" errstr
            in
            justModelUpdate prevModel

        CreateMined (Ok txReceipt) ->
            let
                maybeId =
                    CTypes.txReceiptToCreatedToastytradeSellId txReceipt
                        |> Result.toMaybe
                        |> Maybe.andThen BigIntHelpers.toInt
            in
            case maybeId of
                Just id ->
                    { model = prevModel
                    , cmd = Cmd.none
                    , chainCmd = ChainCmd.none
                    , newRoute = Just (Routing.Trade (Just id))
                    }

                Nothing ->
                    let
                        _ =
                            Debug.log "Error getting the ID of the created contract. Here's the txReceipt" txReceipt
                    in
                    justModelUpdate prevModel

        PMWizardMsg pmMsg ->
            case prevModel.addPMModal of
                Just pmModel ->
                    let
                        updateResult =
                            PMWizard.update pmMsg pmModel

                        cmd =
                            Cmd.map PMWizardMsg updateResult.cmd

                        newPaymentMethods =
                            List.append
                                prevModel.inputs.paymentMethods
                                (Maybe.Extra.values [ updateResult.newPaymentMethod ])

                        model =
                            let
                                prevInputs =
                                    prevModel.inputs
                            in
                            { prevModel
                                | addPMModal = updateResult.model -- If nothing, will close modal
                            }
                                |> updateInputs
                                    { prevInputs | paymentMethods = newPaymentMethods }
                    in
                    UpdateResult
                        model
                        cmd
                        ChainCmd.none
                        Nothing

                Nothing ->
                    let
                        _ =
                            Debug.log "Got a PMWizard message, but the modal is closed! That doesn't make sense! AHHHHH" ""
                    in
                    justModelUpdate prevModel

        NoOp ->
            justModelUpdate prevModel


updateInputs : Inputs -> Model -> Model
updateInputs newInputs model =
    { model | inputs = newInputs }
        |> updateParameters


updateParameters : Model -> Model
updateParameters model =
    { model
        | createParameters =
            Maybe.map2
                CTypes.buildCreateParameters
                model.userInfo
                (validateInputs tokenDecimals model.inputs)
    }


validateInputs : Int -> Inputs -> Maybe CTypes.UserParameters
validateInputs numDecimals inputs =
    Maybe.map2
        (\daiAmount fiatAmount ->
            { openMode = inputs.openMode
            , tradeAmount = daiAmount
            , fiatPrice = { fiatType = inputs.fiatType, amount = fiatAmount }
            , autorecallInterval = inputs.autorecallInterval
            , autoabortInterval = inputs.autoabortInterval
            , autoreleaseInterval = inputs.autoreleaseInterval
            , paymentMethods = inputs.paymentMethods
            }
        )
        (TokenValue.fromString numDecimals inputs.daiAmount)
        (BigInt.fromString inputs.fiatAmount)


recalculateFiatAmountString : String -> String -> String -> Maybe String
recalculateFiatAmountString daiAmountStr marginString fiatType =
    case fiatType of
        "USD" ->
            if String.isEmpty daiAmountStr then
                Just ""

            else
                case ( String.toFloat daiAmountStr, Margin.stringToMarginFloat marginString ) of
                    ( Just daiAmount, Just marginFloat ) ->
                        Just
                            (daiAmount
                                + (daiAmount * marginFloat)
                                |> round
                                |> String.fromInt
                            )

                    _ ->
                        Nothing

        _ ->
            Nothing


recalculateMarginString : String -> String -> String -> Maybe String
recalculateMarginString daiAmountString fiatAmountString fiatType =
    case fiatType of
        "USD" ->
            Maybe.map2
                Margin.marginFromFloats
                (String.toFloat daiAmountString)
                (String.toFloat fiatAmountString)
                |> Maybe.map Margin.marginToString

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
