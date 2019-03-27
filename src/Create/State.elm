module Create.State exposing (init, subscriptions, update, updateUserInfo)

import BigInt
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Constants exposing (..)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Routing
import TimeHelpers
import TokenValue exposing (TokenValue)


init : EthHelpers.EthNode -> Maybe UserInfo -> ( Model, Cmd Msg, ChainCmd Msg )
init node userInfo =
    let
        initialInputs =
            { openMode = CTypes.SellerOpened
            , daiAmount = "100"
            , fiatType = "USD"
            , fiatAmount = "100"
            , margin = "0"
            , paymentMethods = []
            , autorecallInterval = "3"
            , autoabortInterval = "3"
            , autoreleaseInterval = "3"
            }

        model =
            { node = node
            , userInfo = userInfo
            , inputs = initialInputs
            , showFiatTypeDropdown = False
            , createParameters = Nothing
            , busyWithTxChain = False
            }
    in
    ( model |> updateInputs initialInputs
    , Cmd.none
    , ChainCmd.none
    )


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
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | daiAmount = newAmountStr })

        FiatAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | fiatAmount = newAmountStr })

        FiatTypeChanged newTypeStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | fiatType = newTypeStr })

        FiatTypeLostFocus ->
            justModelUpdate
                { prevModel
                    | showFiatTypeDropdown = False
                }

        OpenCurrencySelector ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                ({ prevModel
                    | showFiatTypeDropdown = True
                 }
                    |> updateInputs { oldInputs | fiatType = "" }
                )

        FiatTypeArrowClicked ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                ({ prevModel
                    | showFiatTypeDropdown = True
                 }
                    |> updateInputs { oldInputs | fiatType = "" }
                )

        MarginStringChanged newString ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | margin = newString })

        -- AutorecallIntervalChanged newTimeStr ->
        --     let
        --         oldInputs =
        --             prevModel.inputs
        --     in
        --     justModelUpdate (prevModel |> updateInputs { oldInputs | autorecallInterval = newTimeStr })
        -- AutoabortIntervalChanged newTimeStr ->
        --     let
        --         oldInputs =
        --             prevModel.inputs
        --     in
        --     justModelUpdate (prevModel |> updateInputs { oldInputs | autoabortInterval = newTimeStr })
        -- AutoreleaseIntervalChanged newTimeStr ->
        --     let
        --         oldInputs =
        --             prevModel.inputs
        --     in
        --     justModelUpdate (prevModel |> updateInputs { oldInputs | autoreleaseInterval = newTimeStr })
        AddPaymentMethod paymentMethod ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs | paymentMethods = List.append oldInputs.paymentMethods [ paymentMethod ] }
                )

        ShowCurrencyDropdown flag ->
            justModelUpdate
                { prevModel | showFiatTypeDropdown = flag }

        BeginCreateProcess ->
            case prevModel.createParameters of
                Just parameters ->
                    { model = prevModel
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
                            , onSign = Nothing
                            , onBroadcast = Nothing
                            }

                        newModel =
                            { prevModel | busyWithTxChain = True }
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

        ApproveMined (Err errstr) ->
            let
                _ =
                    Debug.log "'approve' call mining error" errstr
            in
            justModelUpdate prevModel

        ApproveMined (Ok txReceipt) ->
            if not prevModel.busyWithTxChain then
                let
                    _ =
                        Debug.log "Not ready to catch this mined tx. Did you somehow cancel the tx chain?" ""
                in
                justModelUpdate prevModel

            else
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
                                , onSign = Nothing
                                , onBroadcast = Nothing
                                }
                        in
                        { model = prevModel
                        , cmd = Cmd.none
                        , chainCmd = ChainCmd.custom customSend txParams
                        , newRoute = Nothing
                        }

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
                    , newRoute = Nothing --Just (Routing.Interact (Just id))
                    }

                Nothing ->
                    let
                        _ =
                            Debug.log "Error getting the ID of the created contract. Here's the txReceipt" txReceipt
                    in
                    justModelUpdate prevModel

        NoOp ->
            justModelUpdate prevModel


updateInputs : Inputs -> Model -> Model
updateInputs newParameters model =
    { model | inputs = newParameters }
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
    Maybe.map5
        (\daiAmount fiatAmount autorecallInterval autoabortInterval autoreleaseInterval ->
            { openMode = inputs.openMode
            , tradeAmount = daiAmount
            , fiatPrice = { fiatType = inputs.fiatType, amount = fiatAmount }
            , autorecallInterval = autorecallInterval
            , autoabortInterval = autoabortInterval
            , autoreleaseInterval = autoreleaseInterval
            , paymentMethods = inputs.paymentMethods
            }
        )
        (TokenValue.fromString numDecimals inputs.daiAmount)
        (BigInt.fromString inputs.fiatAmount)
        (TimeHelpers.daysStrToMaybePosix inputs.autorecallInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoabortInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoreleaseInterval)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
