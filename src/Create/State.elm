module Create.State exposing (init, update, updateParameters, updateWithUserAddress, validateInputs)

import ChainCmd exposing (ChainCmd)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types
import Contracts.Wrappers
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import Flip exposing (flip)
import TimeHelpers
import TokenValue exposing (TokenValue)


init : Address -> Int -> Address -> Maybe Address -> ( Model, Cmd Msg, ChainCmd Msg )
init tokenAddress tokenDecimals factoryAddress userAddress =
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
            , userAddress = userAddress
            , parameterInputs = initialInputs
            , devFee = TokenValue.zero tokenDecimals
            , contractParameters = Nothing
            , busyWithTxChain = False
            }
    in
    ( updateParameters model initialInputs
    , Cmd.none
    , ChainCmd.none
    )


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }
        |> flip updateParameters model.parameterInputs


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        UncoiningAmountChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | uncoiningAmount = newAmountStr }
            , Cmd.none
            , ChainCmd.none
            )

        PriceChanged newAmountStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | price = newAmountStr }
            , Cmd.none
            , ChainCmd.none
            )

        TransferMethodsChanged newStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | transferMethods = newStr }
            , Cmd.none
            , ChainCmd.none
            )

        AutorecallIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | autorecallInterval = newTimeStr }
            , Cmd.none
            , ChainCmd.none
            )

        DepositDeadlineIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | depositDeadlineInterval = newTimeStr }
            , Cmd.none
            , ChainCmd.none
            )

        AutoreleaseIntervalChanged newTimeStr ->
            let
                oldInputs =
                    model.parameterInputs
            in
            ( updateParameters model { oldInputs | autoreleaseInterval = newTimeStr }
            , Cmd.none
            , ChainCmd.none
            )

        BeginCreateProcess ->
            case ( model.userAddress, model.contractParameters ) of
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
                    ( newModel, Cmd.none, ChainCmd.custom customSend txParams )

                ( Nothing, _ ) ->
                    let
                        _ =
                            Debug.log "Metamask seems to be locked! I can't find the user address."
                    in
                    ( model, Cmd.none, ChainCmd.none )

                ( _, Nothing ) ->
                    let
                        _ =
                            Debug.log "Can't create without a valid contract!" ""
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ApproveMined (Err errstr) ->
            let
                _ =
                    Debug.log "'approve' call mining error"
                        errstr
            in
            ( model, Cmd.none, ChainCmd.none )

        ApproveMined (Ok txReceipt) ->
            if not model.busyWithTxChain then
                let
                    _ =
                        Debug.log "Not ready to catch this mined tx. Did you somehow cancel the tx chain?" ""
                in
                ( model, Cmd.none, ChainCmd.none )

            else
                case model.contractParameters of
                    Nothing ->
                        let
                            _ =
                                Debug.log "Can't find valid contract parameters. What the heck?????" ""
                        in
                        ( model, Cmd.none, ChainCmd.none )

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
                        ( model, Cmd.none, ChainCmd.custom customSend txParams )

        CreateMined (Err errstr) ->
            let
                _ =
                    Debug.log "error mining create contract tx" errstr
            in
            ( model, Cmd.none, ChainCmd.none )

        CreateMined (Ok txReceipt) ->
            -- let
            --     _ =
            --         Debug.log "id" <| Result.map BigInt.toString (Contracts.Types.txReceiptToCreatedToastytradeSellId model.factoryAddress txReceipt)
            -- in
            ( model, Cmd.none, ChainCmd.none )

        NoOp ->
            ( model, Cmd.none, ChainCmd.none )


updateParameters : Model -> ContractParameterInputs -> Model
updateParameters model newParameters =
    { model
        | parameterInputs = newParameters
        , contractParameters =
            Maybe.map2
                Contracts.Types.buildFullParameters
                model.userAddress
                (validateInputs model.tokenDecimals newParameters)
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
