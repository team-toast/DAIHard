module Create.State exposing (init, subscriptions, update, updateUserInfo)

import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
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
import Network exposing (..)
import PaymentMethods exposing (PaymentMethod)
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
            , errors = noErrors
            , showFiatTypeDropdown = False
            , addPMModal = Nothing
            , createParameters = Nothing
            , txChainStatus = Nothing
            , depositAmount = Nothing
            , allowance = Nothing
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
    , autorecallInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    , autoabortInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    , autoreleaseInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    }


updateUserInfo : Maybe UserInfo -> Model -> ( Model, Cmd Msg )
updateUserInfo userInfo model =
    ( { model | userInfo = userInfo }
        |> updateInputs model.inputs
    , case userInfo of
        Just uInfo ->
            Contracts.Wrappers.getAllowanceCmd
                model.node
                uInfo.address
                (factoryAddress model.node.network)
                AllowanceFetched

        Nothing ->
            Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh time ->
            case prevModel.userInfo of
                Just userInfo ->
                    let
                        cmd =
                            Contracts.Wrappers.getAllowanceCmd
                                prevModel.node
                                userInfo.address
                                (factoryAddress prevModel.node.network)
                                AllowanceFetched
                    in
                    UpdateResult
                        prevModel
                        cmd
                        ChainCmd.none
                        Nothing

                _ ->
                    justModelUpdate prevModel

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

        CreateClicked userInfo ->
            case validateInputs prevModel.inputs of
                Ok userParameters ->
                    let
                        createParameters =
                            CTypes.buildCreateParameters userInfo userParameters
                    in
                    UpdateResult
                        { prevModel
                            | txChainStatus = Just <| Confirm createParameters
                            , depositAmount = Nothing
                        }
                        (Contracts.Wrappers.getExtraFeesCmd
                            prevModel.node
                            (TokenValue.getBigInt createParameters.tradeAmount)
                            (ExtraFeesFetched createParameters)
                        )
                        ChainCmd.none
                        Nothing

                Err inputErrors ->
                    justModelUpdate { prevModel | errors = inputErrors }

        ExtraFeesFetched createParameters fetchResult ->
            case fetchResult of
                Ok fees ->
                    let
                        mainDepositAmount =
                            case createParameters.openMode of
                                CTypes.BuyerOpened ->
                                    fees.buyerDeposit

                                CTypes.SellerOpened ->
                                    TokenValue.getBigInt createParameters.tradeAmount

                        fullDepositAmount =
                            mainDepositAmount
                                |> BigInt.add fees.devFee
                                |> BigInt.add (TokenValue.getBigInt createParameters.pokeReward)
                    in
                    justModelUpdate { prevModel | depositAmount = Just fullDepositAmount }

                Err fetchErrStr ->
                    let
                        _ =
                            Debug.log "Error fetching devFee: " fetchErrStr
                    in
                    justModelUpdate prevModel

        AbortCreate ->
            justModelUpdate { prevModel | txChainStatus = Nothing }

        ConfirmCreate createParameters fullDepositAmount ->
            let
                approveChainCmd =
                    let
                        txParams =
                            TokenContract.approve
                                (daiAddress prevModel.node.network)
                                (factoryAddress prevModel.node.network)
                                fullDepositAmount
                                |> Eth.toSend

                        customSend =
                            { onMined = Nothing
                            , onSign = Just (ApproveSigned createParameters)
                            , onBroadcast = Nothing
                            }
                    in
                    ChainCmd.custom customSend txParams

                ( txChainStatus, chainCmd ) =
                    case prevModel.allowance of
                        Just allowance ->
                            if BigInt.compare allowance fullDepositAmount /= LT then
                                initiateCreateCall prevModel.node.network createParameters

                            else
                                ( Just ApproveNeedsSig, approveChainCmd )

                        Nothing ->
                            ( Just ApproveNeedsSig, approveChainCmd )
            in
            { model = { prevModel | txChainStatus = txChainStatus }
            , cmd = Cmd.none
            , chainCmd = chainCmd
            , newRoute = Nothing
            }

        ApproveSigned createParameters result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| ApproveMining createParameters txHash }

                Err e ->
                    let
                        _ =
                            Debug.log "Error encountered when getting sig from user" e
                    in
                    justModelUpdate { prevModel | txChainStatus = Nothing }

        AllowanceFetched fetchResult ->
            case fetchResult of
                Ok allowance ->
                    let
                        newModel =
                            { prevModel
                                | allowance = Just allowance
                            }
                    in
                    case ( newModel.txChainStatus, newModel.depositAmount ) of
                        ( Just (ApproveMining createParameters _), Just depositAmount ) ->
                            if BigInt.compare allowance depositAmount /= LT then
                                let
                                    ( txChainStatus, chainCmd ) =
                                        initiateCreateCall newModel.node.network createParameters
                                in
                                UpdateResult
                                    { newModel | txChainStatus = txChainStatus }
                                    Cmd.none
                                    chainCmd
                                    Nothing

                            else
                                justModelUpdate newModel

                        _ ->
                            justModelUpdate newModel

                Err e ->
                    let
                        _ =
                            Debug.log "Error fecthing allowance" e
                    in
                    justModelUpdate prevModel

        CreateSigned result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| CreateMining txHash }

                Err e ->
                    let
                        _ =
                            Debug.log "Error encountered when getting sig from user" e
                    in
                    justModelUpdate { prevModel | txChainStatus = Nothing }

        CreateMined (Err errstr) ->
            let
                _ =
                    Debug.log "error mining create contract tx" errstr
            in
            justModelUpdate prevModel

        CreateMined (Ok txReceipt) ->
            let
                maybeId =
                    CTypes.txReceiptToCreatedTradeSellId prevModel.node.network txReceipt
                        |> Result.toMaybe
                        |> Maybe.andThen BigIntHelpers.toInt
            in
            case maybeId of
                Just id ->
                    { model = prevModel
                    , cmd = Cmd.none
                    , chainCmd = ChainCmd.none
                    , newRoute = Just (Routing.Trade id)
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


initiateCreateCall : Network -> CTypes.CreateParameters -> ( Maybe TxChainStatus, ChainCmd Msg )
initiateCreateCall network parameters =
    let
        txParams =
            Contracts.Wrappers.openTrade
                network
                parameters
                |> Eth.toSend

        customSend =
            { onMined = Just ( CreateMined, Nothing )
            , onSign = Just CreateSigned
            , onBroadcast = Nothing
            }
    in
    ( Just CreateNeedsSig
    , ChainCmd.custom customSend txParams
    )


updateInputs : Inputs -> Model -> Model
updateInputs newInputs model =
    { model | inputs = newInputs }
        |> updateParameters


updateParameters : Model -> Model
updateParameters model =
    let
        validateResult =
            validateInputs model.inputs

        -- Don't log errors right away (wait until the user tries to submit)
        -- But if there are already errors displaying, update them accordingly
        newErrors =
            if model.errors == noErrors then
                noErrors

            else
                case validateResult of
                    Ok _ ->
                        noErrors

                    Err errors ->
                        errors
    in
    { model
        | createParameters =
            Maybe.map2
                CTypes.buildCreateParameters
                model.userInfo
                (Result.toMaybe validateResult)
        , errors = newErrors
    }


validateInputs : Inputs -> Result Errors CTypes.UserParameters
validateInputs inputs =
    Result.map3
        (\daiAmount fiatAmount paymentMethods ->
            { openMode = inputs.openMode
            , tradeAmount = daiAmount
            , fiatPrice = { fiatType = inputs.fiatType, amount = fiatAmount }
            , autorecallInterval = inputs.autorecallInterval
            , autoabortInterval = inputs.autoabortInterval
            , autoreleaseInterval = inputs.autoreleaseInterval
            , paymentMethods = paymentMethods
            }
        )
        (interpretDaiAmount inputs.daiAmount
            |> Result.mapError (\e -> { noErrors | daiAmount = Just e })
        )
        (interpretFiatAmount inputs.fiatAmount
            |> Result.mapError (\e -> { noErrors | fiat = Just e })
        )
        (interpretPaymentMethods inputs.paymentMethods
            |> Result.mapError (\e -> { noErrors | paymentMethods = Just e })
        )


interpretDaiAmount : String -> Result String TokenValue
interpretDaiAmount input =
    if input == "" then
        Err "You must specify a trade amount."

    else
        case TokenValue.fromString tokenDecimals input of
            Nothing ->
                Err "I don't understand this number."

            Just value ->
                if TokenValue.getFloatValueWithWarning value < 1 then
                    Err "Trade amount must be a least 1 DAI."

                else
                    Ok value


interpretFiatAmount : String -> Result String BigInt
interpretFiatAmount input =
    if input == "" then
        Err "You must specify a fiat price."

    else
        case BigInt.fromString input of
            Nothing ->
                case String.toFloat input of
                    Just _ ->
                        Err "Fractional fiat amounts (i.e. $1.20) are not supported. Use a whole number."

                    _ ->
                        Err "I don't understand this number."

            Just value ->
                Ok value


interpretPaymentMethods : List PaymentMethod -> Result String (List PaymentMethod)
interpretPaymentMethods paymentMethods =
    if paymentMethods == [] then
        Err "Must include at least one payment method."

    else
        Ok paymentMethods


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
    Time.every 2000 Refresh
