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
import EthHelpers
import Flip exposing (flip)
import Routing
import TimeHelpers
import TokenValue exposing (TokenValue)


init : EthHelpers.EthNode -> Address -> Int -> Address -> Maybe UserInfo -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode tokenAddress tokenDecimals factoryAddress userInfo =
    let
        initialInputs =
            { openMode = Contracts.Types.SellerOpened
            , tradeAmount = "100"
            , totalPrice = "100"
            , transferMethods = ""
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
            , parameterInputs = initialInputs
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

        PriceChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            justModelUpdate (model |> udpateParameterInputs { oldInputs | totalPrice = newAmountStr })

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

        BeginCreateProcess ->
            case model.contractParameters of
                Just parameters ->
                    { model = model
                    , cmd =
                        Contracts.Wrappers.getDevFeeCmd
                            model.ethNode
                            model.factoryAddress
                            (TokenValue.getBigInt parameters.tradeAmount)
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
                        fullSendAmount =
                            TokenValue.getBigInt parameters.tradeAmount
                                |> BigInt.add devFee
                                |> BigInt.add (TokenValue.getBigInt parameters.pokeReward)

                        txParams =
                            TokenContract.approve
                                model.tokenAddress
                                model.factoryAddress
                                fullSendAmount
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
        (\tradeAmount totalPriceString autorecallInterval autoabortInterval autoreleaseInterval ->
            { openMode = inputs.openMode
            , tradeAmount = tradeAmount
            , totalPriceCurrency = "??"
            , totalPriceValue = totalPriceString
            , autorecallInterval = autorecallInterval
            , autoabortInterval = autoabortInterval
            , autoreleaseInterval = autoreleaseInterval
            , transferMethods = inputs.transferMethods
            }
        )
        (TokenValue.fromString numDecimals inputs.tradeAmount)
        (TokenValue.fromString numDecimals inputs.totalPrice)
        (TimeHelpers.daysStrToMaybePosix inputs.autorecallInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoabortInterval)
        (TimeHelpers.daysStrToMaybePosix inputs.autoreleaseInterval)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
