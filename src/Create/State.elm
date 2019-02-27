module Create.State exposing (init, subscriptions, udpateParameterInputs, update, updateUserInfo, validateInputs)

import BigInt
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types
import Contracts.Wrappers
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import Flip exposing (flip)
import Routing
import TimeHelpers
import TokenValue exposing (TokenValue)


init : Address -> Int -> Address -> Maybe UserInfo -> ( Model, Cmd Msg, ChainCmd Msg )
init tokenAddress tokenDecimals factoryAddress userInfo =
    let
        initialInputs =
            { uncoiningAmount = "100"
            , price = "100"
            , transferMethods = ""
            , autorecallInterval = "3"
            , depositDeadlineInterval = "3"
            , autoreleaseInterval = "3"
            }

        model =
            { tokenAddress = tokenAddress
            , tokenDecimals = tokenDecimals
            , factoryAddress = factoryAddress
            , userInfo = userInfo
            , parameterInputs = initialInputs
            , devFee = TokenValue.zero tokenDecimals
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
        UncoiningAmountChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | uncoiningAmount = newAmountStr })

        PriceChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | price = newAmountStr })

        TransferMethodsChanged newStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | transferMethods = newStr })

        AutorecallIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | autorecallInterval = newTimeStr })

        DepositDeadlineIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | depositDeadlineInterval = newTimeStr })

        AutoreleaseIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | autoreleaseInterval = newTimeStr })

        BeginCreateProcess ->
            case ( model.userInfo, model.contractParameters ) of
                ( Just _, Just parameters ) ->
                    let
                        fullSendAmount =
                            TokenValue.add parameters.uncoiningAmount model.devFee

                        txParams =
                            TokenContract.approve
                                model.tokenAddress
                                model.factoryAddress
                                (TokenValue.getBigInt fullSendAmount)
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

                ( Nothing, _ ) ->
                    let
                        _ =
                            Debug.log "Metamask seems to be locked! I can't find the user address."
                    in
                    justModelUpdate model

                ( _, Nothing ) ->
                    let
                        _ =
                            Debug.log "Can't create without a valid contract!" ""
                    in
                    justModelUpdate model

        ApproveMined (Err errstr) ->
            let
                _ =
                    Debug.log "'approve' call mining error"
                        errstr
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
                Contracts.Types.buildFullParameters
                model.userInfo
                (validateInputs model.tokenDecimals model.parameterInputs)
    }


validateInputs : Int -> ContractParameterInputs -> Maybe Contracts.Types.UserParameters
validateInputs numDecimals inputs =
    Maybe.map5
        (\uncoiningAmount price autorecallInterval depositDeadlineInterval autoreleaseInterval ->
            { uncoiningAmount = uncoiningAmount
            , price = price
            , autorecallInterval = autorecallInterval
            , depositDeadlineInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , transferMethods = inputs.transferMethods
            }
        )
        (TokenValue.fromString numDecimals inputs.uncoiningAmount)
        (TokenValue.fromString numDecimals inputs.price)
        (TimeHelpers.daysStrToMaybePosix inputs.autorecallInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.depositDeadlineInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoreleaseInterval)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
