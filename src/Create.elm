module Create exposing
    ( Model
    , Msg
    , init
    , update
    , updateWithUserAddress
    , view
    )

import BigInt
import ChainCmd exposing (ChainCmdOrder)
import ContractRender
import Contracts.ERC20Token as TokenContract
import Contracts.ToastytradeExtras as TTExtras
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth
import Eth.Types exposing (Address, TxReceipt)
import Eth.Utils
import Flip exposing (flip)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type alias Model =
    { tokenAddress : Address
    , tokenDecimals : Int
    , factoryAddress : Address
    , userAddress : Maybe Address
    , parameterInputs : ContractParameterInputs
    , devFee : TokenValue
    , contractParameters : Maybe TTExtras.FullParameters
    , busyWithTxChain : Bool
    }


type alias ContractParameterInputs =
    { uncoiningAmount : String
    , price : String
    , transferMethods : String
    , autorecallInterval : String
    , depositDeadlineInterval : String
    , autoreleaseInterval : String
    }


type Msg
    = UncoiningAmountChanged String
    | PriceChanged String
    | AutorecallIntervalChanged String
    | DepositDeadlineIntervalChanged String
    | AutoreleaseIntervalChanged String
    | TransferMethodsChanged String
    | BeginCreateProcess
    | ApproveMined (Result String TxReceipt)
    | CreateMined (Result String TxReceipt)
    | NoOp


init : Address -> Int -> Address -> Maybe Address -> ( Model, Cmd Msg, ChainCmdOrder Msg )
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


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmdOrder Msg )
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
                                TTExtras.createSell
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
            let
                _ =
                    Debug.log "addresssss" (TTExtras.txReceiptToCreatedToastytradeSellAddress model.factoryAddress txReceipt)

                _ =
                    Debug.log "status" txReceipt.status
            in
            ( model, Cmd.none, ChainCmd.none )

        NoOp ->
            ( model, Cmd.none, ChainCmd.none )


updateParameters : Model -> ContractParameterInputs -> Model
updateParameters model newParameters =
    { model
        | parameterInputs = newParameters
        , contractParameters =
            Maybe.map2
                TTExtras.buildFullParameters
                model.userAddress
                (validateInputs model.tokenDecimals newParameters)
    }


validateInputs : Int -> ContractParameterInputs -> Maybe TTExtras.UserParameters
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


view : Model -> Element.Element Msg
view model =
    let
        contractRendered =
            case model.contractParameters of
                Nothing ->
                    Element.text "no contract?????"

                Just parameters ->
                    ContractRender.render ContractRender.Draft parameters
                        |> Element.map (\msg -> NoOp)

        createButton =
            Element.Input.button [ Element.centerX ]
                { onPress = Just BeginCreateProcess
                , label = Element.text "Create!"
                }
    in
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ contractParametersFormElement model
        , createButton
        , contractRendered
        ]


contractParametersFormElement : Model -> Element.Element Msg
contractParametersFormElement model =
    EH.fillWidthBlock "Contract parameters" (contractParametersForm model)


contractParametersForm : Model -> Element.Element Msg
contractParametersForm model =
    let
        columnHeader title =
            Element.el [ Element.Font.size 24, Element.Font.bold, Element.centerX ] (Element.text title)

        daiAmountInputs =
            let
                nameAndElementToRow tuple =
                    Element.row [ Element.spacing 8 ]
                        [ Element.el [ Element.width (Element.px 200) ]
                            (Element.paragraph [ Element.width Element.shrink, Element.alignLeft ] [ Element.text (Tuple.first tuple) ])
                        , Tuple.second tuple
                        ]
            in
            Element.column [ Element.width (Element.fillPortion 1), Element.spacing 8, Element.alignTop ]
                (columnHeader "Dai Amounts"
                    :: ([ ( "Uncoining Amount", EH.smallInput "uncoiningAmount" model.parameterInputs.uncoiningAmount UncoiningAmountChanged )
                        , ( "Total Fiat Price", EH.smallInput "summonfee" model.parameterInputs.price PriceChanged )
                        ]
                            |> List.map nameAndElementToRow
                       )
                )

        transferMethodsInput =
            Element.column [ Element.width (Element.fillPortion 3), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Fiat Transfer Methods"
                , Element.Input.multiline [ Element.width Element.fill, Element.height (Element.px 150) ]
                    { onChange = TransferMethodsChanged
                    , text = model.parameterInputs.transferMethods
                    , placeholder = Just (Element.Input.placeholder [] (Element.paragraph [] [ Element.text "Be specific, and consider listing multiple options. Keep in mind that many Responders find offers via keyword searches." ]))
                    , label = Element.Input.labelHidden "transferMethods"
                    , spellcheck = False
                    }
                ]

        intervalInputs =
            let
                nameAndElementToReversedRow tuple =
                    Element.row [ Element.spacing 8 ]
                        [ Tuple.second tuple
                        , Element.el [ Element.width (Element.px 170) ]
                            (Element.paragraph [ Element.width Element.shrink, Element.alignLeft ] [ Element.text (Tuple.first tuple) ])
                        ]
            in
            Element.column [ Element.width (Element.fillPortion 1), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Phase Time Limits"
                , Element.column [ Element.spacing 8 ]
                    ([ ( "Autorecall", EH.timeInput "autorecall interval" model.parameterInputs.autorecallInterval AutorecallIntervalChanged )
                     , ( "Deposit Deadline", EH.timeInput "deposit deadline interval" model.parameterInputs.depositDeadlineInterval DepositDeadlineIntervalChanged )
                     , ( "Autorelease", EH.timeInput "autorelease interval" model.parameterInputs.autoreleaseInterval AutoreleaseIntervalChanged )
                     ]
                        |> List.map nameAndElementToReversedRow
                    )
                ]
    in
    Element.row [ Element.width Element.fill, Element.spacing 20 ]
        [ daiAmountInputs
        , transferMethodsInput
        , intervalInputs
        ]
