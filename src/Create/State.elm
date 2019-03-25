module Create.State exposing (init, subscriptions, udpateParameterInputs, update, updateUserInfo, validateInputs)

import BigInt
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types
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


init : EthHelpers.EthNode -> Address -> Int -> Address -> Maybe Contracts.Types.OpenMode -> Maybe UserInfo -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode tokenAddress tokenDecimals factoryAddress maybeOpenMode userInfo =
    let
        openMode =
            maybeOpenMode |> Maybe.withDefault Contracts.Types.SellerOpened

        initialInputs =
            { openMode = openMode
            , tradeAmount = "100"
            , fiatType = "USD"
            , fiatAmount = "100"
            , paymentMethods = []
            , autorecallInterval = "3"
            , autoabortInterval = "3"
            , autoreleaseInterval = "3"
            }

        model =
            { ethNode = ethNode
            , tokenAddress = tokenAddress
            , tokenDecimals = tokenDecimals
            , factoryAddress = factoryAddress
            , userInfo = userInfo
            , openMode = openMode
            , parameterInputs = initialInputs
            , showCurrencyDropdown = False
            , contractParameters = Nothing
            , busyWithTxChain = False
            }
    in
    ( model |> udpateParameterInputs initialInputs
    , Cmd.none
    , ChainCmd.none
    )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }
        |> udpateParameterInputs model.parameterInputs


update : Msg -> Model -> UpdateResult
update msg model =
    case msg of
        TradeAmountChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | tradeAmount = newAmountStr })

        FiatAmountChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | fiatAmount = newAmountStr })

        FiatTypeChanged newTypeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | fiatType = newTypeStr })

        AutorecallIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | autorecallInterval = newTimeStr })

        AutoabortIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | autoabortInterval = newTimeStr })

        AutoreleaseIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | autoreleaseInterval = newTimeStr })

        AddPaymentMethod paymentMethod ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate
                (model
                    |> udpateParameterInputs
                        { oldInputs | paymentMethods = List.append oldInputs.paymentMethods [ paymentMethod ] }
                )

        ShowCurrencyDropdown flag ->
            justModelUpdate
                { model | showCurrencyDropdown = flag }

        BeginCreateProcess ->
            case model.contractParameters of
                Just parameters ->
                    { model = model
                    , cmd =
                        Contracts.Wrappers.getDevFeeCmd
                            model.ethNode
                            model.factoryAddress
                            (TokenValue.getBigInt parameters.tradeParameters.tradeAmount)
                            DevFeeFetched
                    , chainCmd = ChainCmd.none
                    , newRoute = Nothing
                    }

                Nothing ->
                    let
                        _ =
                            Debug.log "Trying to form fetchDevFee cmd, but can't find the contract parameters!" ""
                    in
                    justModelUpdate model

        DevFeeFetched fetchResult ->
            case ( fetchResult, model.userInfo, model.contractParameters ) of
                ( Ok devFee, Just _, Just parameters ) ->
                    let
                        mainDepositAmount =
                            TokenValue.getBigInt <|
                                case parameters.tradeParameters.openMode of
                                    Contracts.Types.BuyerOpened ->
                                        parameters.tradeParameters.buyerDeposit

                                    Contracts.Types.SellerOpened ->
                                        parameters.tradeParameters.tradeAmount

                        fullDepositAmount =
                            mainDepositAmount
                                |> BigInt.add devFee
                                |> BigInt.add (TokenValue.getBigInt parameters.tradeParameters.pokeReward)

                        txParams =
                            TokenContract.approve
                                model.tokenAddress
                                model.factoryAddress
                                fullDepositAmount
                                |> Eth.toSend

                        customSend =
                            { onMined = Just ( ApproveMined, Nothing )
                            , onSign = Nothing
                            , onBroadcast = Nothing
                            }

                        newModel =
                            { model | busyWithTxChain = True }
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
                    justModelUpdate model

                ( _, Nothing, _ ) ->
                    let
                        _ =
                            Debug.log "Metamask seems to be locked! I can't find the user address." ""
                    in
                    justModelUpdate model

                ( _, _, Nothing ) ->
                    let
                        _ =
                            Debug.log "Can't create without a valid contract!" ""
                    in
                    justModelUpdate model

        ApproveMined (Err errstr) ->
            let
                _ =
                    Debug.log "'approve' call mining error" errstr
            in
            justModelUpdate model

        ApproveMined (Ok txReceipt) ->
            if not model.busyWithTxChain then
                let
                    _ =
                        Debug.log "Not ready to catch this mined tx. Did you somehow cancel the tx chain?" ""
                in
                justModelUpdate model

            else
                case model.contractParameters of
                    Nothing ->
                        let
                            _ =
                                Debug.log "Can't find valid contract parameters. What the heck?????" ""
                        in
                        justModelUpdate model

                    Just contractParameters ->
                        let
                            txParams =
                                Contracts.Wrappers.createSell
                                    model.factoryAddress
                                    contractParameters
                                    |> Eth.toSend

                            customSend =
                                { onMined = Just ( CreateMined, Nothing )
                                , onSign = Nothing
                                , onBroadcast = Nothing
                                }
                        in
                        { model = model
                        , cmd = Cmd.none
                        , chainCmd = ChainCmd.custom customSend txParams
                        , newRoute = Nothing
                        }

        CreateMined (Err errstr) ->
            let
                _ =
                    Debug.log "error mining create contract tx" errstr
            in
            justModelUpdate model

        CreateMined (Ok txReceipt) ->
            let
                maybeId =
                    Contracts.Types.txReceiptToCreatedToastytradeSellId model.factoryAddress txReceipt
                        |> Result.toMaybe
                        |> Maybe.andThen BigIntHelpers.toInt
            in
            case maybeId of
                Just id ->
                    { model = model
                    , cmd = Cmd.none
                    , chainCmd = ChainCmd.none
                    , newRoute = Just (Routing.Interact (Just id))
                    }

                Nothing ->
                    let
                        _ =
                            Debug.log "Error getting the ID of the created contract. Here's the txReceipt" txReceipt
                    in
                    justModelUpdate model

        NoOp ->
            justModelUpdate model


udpateParameterInputs : ContractParameterInputs -> Model -> Model
udpateParameterInputs newParameters model =
    { model | parameterInputs = newParameters }
        |> updateParameters


updateParameters : Model -> Model
updateParameters model =
    { model
        | contractParameters =
            Maybe.map2
                Contracts.Types.buildCreateParameters
                model.userInfo
                (validateInputs model.tokenDecimals model.parameterInputs)
    }


validateInputs : Int -> ContractParameterInputs -> Maybe Contracts.Types.UserParameters
validateInputs numDecimals inputs =
    Maybe.map5
        (\tradeAmount fiatAmount autorecallInterval autoabortInterval autoreleaseInterval ->
            { openMode = inputs.openMode
            , tradeAmount = tradeAmount
            , fiatPrice = { fiatType = inputs.fiatType, amount = fiatAmount }
            , autorecallInterval = autorecallInterval
            , autoabortInterval = autoabortInterval
            , autoreleaseInterval = autoreleaseInterval
            , paymentMethods = inputs.paymentMethods
            }
        )
        (TokenValue.fromString numDecimals inputs.tradeAmount)
        (BigInt.fromString inputs.fiatAmount)
        (TimeHelpers.daysStrToMaybePosix inputs.autorecallInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoabortInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoreleaseInterval)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
