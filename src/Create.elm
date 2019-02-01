module Create exposing
    ( Model
    , Msg
    , init
    , update
    , updateWithUserAddress
    , viewElement
    )

import BigInt
import ChainCmd exposing (ChainCmdOrder)
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
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type alias Model =
    { tokenAddress : Address
    , factoryAddress : Address
    , userAddress : Maybe Address
    , uncoiningAmount : TokenValue
    , summonFee : TokenValue
    , devFee : TokenValue
    , initialDeposit : TokenValue
    , responderDeposit : TokenValue
    , preCommitBalance : TokenValue
    , postCommitBalance : TokenValue
    , claimFailBurnAmount : TokenValue
    , transferMethods : String
    , auotrecallIntervalInput : String
    , depositDeadlineIntervalInput : String
    , autoreleaseIntervalInput : String
    , autorecallInterval : Maybe Time.Posix
    , depositDeadlineInterval : Maybe Time.Posix
    , autoreleaseInterval : Maybe Time.Posix
    }


type Msg
    = UncoiningAmountChanged String
    | SummonFeeChanged String
    | AutorecallIntervalChanged String
    | DepositDeadlineIntervalChanged String
    | AutoreleaseIntervalChanged String
    | TransferMethodsChanged String
    | BeginCreateProcess
    | ApproveMined (Result String TxReceipt)
    | CreateMined (Result String TxReceipt)
    | NoOp


init : Address -> Address -> Maybe Address -> Int -> ( Model, Cmd Msg, ChainCmdOrder Msg )
init tokenAddress factoryAddress userAddress tokenDecimals =
    let
        model =
            { tokenAddress = tokenAddress
            , factoryAddress = factoryAddress
            , userAddress = userAddress
            , uncoiningAmount = TokenValue.tokenValue tokenDecimals "100"
            , summonFee = TokenValue.empty tokenDecimals
            , devFee = TokenValue.empty tokenDecimals
            , initialDeposit = TokenValue.empty tokenDecimals
            , responderDeposit = TokenValue.empty tokenDecimals
            , preCommitBalance = TokenValue.empty tokenDecimals
            , postCommitBalance = TokenValue.empty tokenDecimals
            , claimFailBurnAmount = TokenValue.empty tokenDecimals
            , transferMethods = ""
            , auotrecallIntervalInput = "3"
            , depositDeadlineIntervalInput = "3"
            , autoreleaseIntervalInput = "3"
            , autorecallInterval = TimeHelpers.daysStrToMaybePosix "3"
            , depositDeadlineInterval = TimeHelpers.daysStrToMaybePosix "3"
            , autoreleaseInterval = TimeHelpers.daysStrToMaybePosix "3"
            }
    in
    ( propogateUncoiningAmountChange model
    , Cmd.none
    , ChainCmd.none
    )


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmdOrder Msg )
update msg model =
    case msg of
        UncoiningAmountChanged newAmtStr ->
            let
                newModel =
                    { model | uncoiningAmount = TokenValue.updateViaString model.uncoiningAmount newAmtStr }
            in
            ( propogateUncoiningAmountChange newModel, Cmd.none, ChainCmd.none )

        SummonFeeChanged newAmtStr ->
            ( { model | summonFee = TokenValue.updateViaString model.summonFee newAmtStr }, Cmd.none, ChainCmd.none )

        TransferMethodsChanged newStr ->
            ( { model | transferMethods = newStr }, Cmd.none, ChainCmd.none )

        AutorecallIntervalChanged input ->
            ( { model
                | auotrecallIntervalInput = input
                , autorecallInterval = Debug.log "posix" (TimeHelpers.daysStrToMaybePosix input)
              }
            , Cmd.none
            , ChainCmd.none
            )

        DepositDeadlineIntervalChanged input ->
            ( { model
                | depositDeadlineIntervalInput = input
                , depositDeadlineInterval = TimeHelpers.daysStrToMaybePosix input
              }
            , Cmd.none
            , ChainCmd.none
            )

        AutoreleaseIntervalChanged input ->
            ( { model
                | autoreleaseIntervalInput = input
                , autoreleaseInterval = TimeHelpers.daysStrToMaybePosix input
              }
            , Cmd.none
            , ChainCmd.none
            )

        BeginCreateProcess ->
            case ( model.userAddress, TokenValue.toBigInt model.uncoiningAmount ) of
                ( Just userAddress, Just uncoiningAmount ) ->
                    let
                        txParams =
                            TokenContract.approve
                                model.tokenAddress
                                model.factoryAddress
                                uncoiningAmount
                                |> Eth.toSend

                        customSend =
                            { onMined = Just ( ApproveMined, Nothing )
                            , onSign = Nothing
                            , onBroadcast = Nothing
                            }
                    in
                    ( model, Cmd.none, ChainCmd.custom customSend txParams )

                ( Nothing, _ ) ->
                    let
                        _ =
                            Debug.log "Metamask seems to be locked! I can't find the user address."
                    in
                    ( model, Cmd.none, ChainCmd.none )

                ( _, Nothing ) ->
                    let
                        _ =
                            Debug.log "Invalid Uncoining amount" (TokenValue.getString model.uncoiningAmount)
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ApproveMined (Err errstr) ->
            ( model, Cmd.none, ChainCmd.none )

        ApproveMined (Ok txReceipt) ->
            case model.userAddress of
                Just userAddress ->
                    let
                        maybeTxParams =
                            Maybe.map5
                                (\uncoiningAmount responderDeposit autorecallInterval depositDeadlineInterval autoreleaseInterval ->
                                    TTExtras.createSell
                                        model.factoryAddress
                                        userAddress
                                        uncoiningAmount
                                        responderDeposit
                                        autorecallInterval
                                        depositDeadlineInterval
                                        autoreleaseInterval
                                        model.transferMethods
                                )
                                (TokenValue.toBigInt model.uncoiningAmount)
                                (TokenValue.toBigInt model.responderDeposit)
                                model.autorecallInterval
                                model.depositDeadlineInterval
                                model.autoreleaseInterval
                                |> Maybe.map Eth.toSend

                        customSend =
                            { onMined = Just ( CreateMined, Nothing )
                            , onSign = Nothing
                            , onBroadcast = Nothing
                            }
                    in
                    case maybeTxParams of
                        Just txParams ->
                            ( model, Cmd.none, ChainCmd.custom customSend txParams )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "Error: uncoiningAmount or responderDeposit is not set"
                            in
                            ( model, Cmd.none, ChainCmd.none )

                Nothing ->
                    let
                        _ =
                            Debug.log "No user account found!"
                    in
                    ( model, Cmd.none, ChainCmd.none )

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


logToEvent : a -> Eth.Types.Log -> Eth.Types.Event a
logToEvent eventType log =
    { address = log.address
    , data = log.data
    , topics = log.topics
    , removed = log.removed
    , logIndex = log.logIndex
    , transactionIndex = log.transactionIndex
    , transactionHash = log.transactionHash
    , blockHash = log.blockHash
    , blockNumber = log.blockNumber
    , returnData = eventType
    }


propogateUncoiningAmountChange : Model -> Model
propogateUncoiningAmountChange model =
    case TokenValue.toBigInt model.uncoiningAmount of
        Nothing ->
            model

        Just uncoiningAmountBigInt ->
            let
                summonFeeBigInt =
                    BigInt.div
                        uncoiningAmountBigInt
                        (BigInt.fromInt 10)

                devFeeBigInt =
                    BigInt.div uncoiningAmountBigInt (BigInt.fromInt 100)

                preCommitBalanceBigInt =
                    BigInt.add uncoiningAmountBigInt summonFeeBigInt

                initialDepositBigInt =
                    BigInt.add preCommitBalanceBigInt devFeeBigInt

                responderDepositBigInt =
                    BigInt.div uncoiningAmountBigInt (BigInt.fromInt 2)

                postCommitBalanceBigInt =
                    BigInt.add initialDepositBigInt responderDepositBigInt

                claimFailBurnAmountBigInt =
                    BigInt.div responderDepositBigInt (BigInt.fromInt 2)
            in
            { model
                | summonFee = TokenValue.updateViaBigInt model.summonFee summonFeeBigInt
                , devFee = TokenValue.updateViaBigInt model.devFee devFeeBigInt
                , initialDeposit = TokenValue.updateViaBigInt model.preCommitBalance initialDepositBigInt
                , preCommitBalance = TokenValue.updateViaBigInt model.initialDeposit preCommitBalanceBigInt
                , postCommitBalance = TokenValue.updateViaBigInt model.responderDeposit postCommitBalanceBigInt
                , responderDeposit = TokenValue.updateViaBigInt model.postCommitBalance responderDepositBigInt
                , claimFailBurnAmount = TokenValue.updateViaBigInt model.claimFailBurnAmount claimFailBurnAmountBigInt
            }


viewElement : Model -> Element.Element Msg
viewElement model =
    let
        header =
            EH.pageTitle "Create Uncoining Contract"

        bodyStyles =
            [ Element.Border.rounded 15
            , Element.Background.color EH.subpageBackgroundColor
            , Element.padding 20
            , Element.width Element.fill
            ]

        body =
            Element.column bodyStyles
                [ parametersSection

                --, openerSection
                --, transferMethodsSection
                --, openPhaseSection
                --, commitedPhaseSection
                --, claimedPhaseSection
                --, createButton
                ]

        parametersSection =
            contractParametersFormElement model

        openerSection =
            Element.column []
                [ Element.paragraph []
                    [ Element.text "Uncoining "
                    , EH.smallInput "uncoiningAmount" (TokenValue.getString model.uncoiningAmount) UncoiningAmountChanged
                    , Element.text " Dai, with a summon fee of "
                    , EH.smallInput "summonfee" (TokenValue.getString model.summonFee) SummonFeeChanged
                    , Element.text " Dai."
                    ]
                , Element.paragraph []
                    [ Element.text "Clicking 'Inititalize Contract' at the bottom of this page will cause CoinerTool to request "
                    , EH.tokenValue model.initialDeposit
                    , Element.text " from your web3 provider. Once you accept the transaction:"
                    ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "The contract shown below will be created with YOU ("
                        , outputMaybeUserAddress model.userAddress
                        , Element.text ") as the "
                        , EH.initiator []
                        , Element.text ";"
                        ]
                    , Element.paragraph []
                        [ EH.tokenValue model.preCommitBalance
                        , Element.text " will be deposited into the contract;"
                        ]
                    , Element.paragraph []
                        [ EH.tokenValue model.devFee
                        , Element.text " will be forwarded to the developers of CoinerTool;"
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.sectionReference "Fiat Transfer Methods"
                        , Element.text " section you fill out below will be publicly posted onto the Ethereum blockchain."
                        ]
                    ]
                , Element.paragraph []
                    [ Element.text "The contract begins in the "
                    , EH.sectionReference "Open Phase"
                    , Element.text ", and will be discoverable by other users via "
                    , EH.fakeLink "CoinerTool Browse"
                    , Element.text " (and potentially third-party tools)."
                    ]
                ]

        transferMethodsSection =
            Element.textColumn [ Element.width Element.fill ]
                [ EH.sectionHeading "Fiat Transfer Methods"
                , Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Committed phase"
                    , Element.text ", the "
                    , EH.responder []
                    , Element.text " is expected to transfer "
                    , EH.usdValue model.uncoiningAmount
                    , Element.text " to the "
                    , EH.initiator []
                    , Element.text " via one of the following methods:"
                    ]
                , Element.Input.multiline []
                    { onChange = TransferMethodsChanged
                    , text = model.transferMethods
                    , placeholder = Just (Element.Input.placeholder [] (Element.text "Be SPECIFIC. One method per line.\nFor example:\n\nCash drop within 50 km of NY Times Square\nBank deposit at Western Union\nHand off cash at 4th and Main (I'm available most weekdays after 6)"))
                    , label = Element.Input.labelHidden "transfer methods"
                    , spellcheck = False
                    }
                ]

        openPhaseSection =
            Element.textColumn [ Element.width Element.fill ]
                [ EH.sectionHeading "Open Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Open Phase"
                    , Element.text ", the "
                    , EH.initiator []
                    , Element.text " waits for a "
                    , EH.responder []
                    , Element.text " to appear (which would move the contract to the "
                    , EH.sectionReference "Committed Phase"
                    , Element.text "). The "
                    , EH.initiator []
                    , Element.text " can cancel anytime with a refund anytime before this happens. "
                    , Element.el [ Element.Font.bold ] (Element.text "This is the ONLY phase in which a refund is a guaranteed option to the ")
                    , EH.initiator [ Element.Font.bold ]
                    , Element.el [ Element.Font.bold ] (Element.text ".")
                    ]
                , Element.paragraph [] [ Element.text "Specifically: " ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "Another Ethereum user may become the "
                        , EH.responder []
                        , Element.text " by executing the  "
                        , EH.methodName "commit"
                        , Element.text " method, which requires a deposit of "
                        , EH.tokenValue model.responderDeposit
                        , Element.text ". This immediately moves the contract to the "
                        , EH.sectionReference "Committed Phase"
                        , Element.text ". Thenceforth, the identity of the "
                        , EH.responder []
                        , Element.text " will never change."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.initiator []
                        , Element.text " may execute the "
                        , EH.methodName "recall"
                        , Element.text " method. This immediately "
                        , EH.sectionReference "closes the contract"
                        , Element.text " and refunds the "
                        , EH.initiator []
                        , Element.text "'s "
                        , EH.tokenValue model.preCommitBalance
                        , Element.text ". "
                        ]
                    , Element.paragraph []
                        [ Element.text "If no Ethereum user "
                        , EH.methodName "commit"
                        , Element.text "s within "
                        , EH.timeInput "autorecall interval" model.auotrecallIntervalInput AutorecallIntervalChanged
                        , Element.text ", "
                        , EH.methodName "recall"
                        , Element.text " will be automatically triggered."
                        ]
                    ]
                ]

        commitedPhaseSection =
            Element.textColumn [ Element.width Element.fill ]
                [ EH.sectionHeading "Commited Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Committed Phase"
                    , Element.text ", the "
                    , EH.responder []
                    , Element.text " is expected to transfer "
                    , EH.usdValue model.uncoiningAmount
                    , Element.text " to the "
                    , EH.initiator []
                    , Element.text " via one of the "
                    , EH.sectionReference "Fiat Transfer Methods"
                    , Element.text " and mark the deposit as complete, moving the contract to the "
                    , EH.sectionReference "Claimed Phase"
                    , Element.text "."
                    ]
                , Element.paragraph [] [ Element.text " Specifically:" ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "The contract has two parties (the "
                        , EH.initiator []
                        , Element.text " and the "
                        , EH.responder []
                        , Element.text "), and has a total balance of "
                        , EH.tokenValue model.postCommitBalance
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.responder []
                        , Element.text " is expected to transfer "
                        , EH.usdValue model.uncoiningAmount
                        , Element.text " to the "
                        , EH.initiator []
                        , Element.text " as described in "
                        , EH.sectionReference "Fiat Transfer Methods"
                        , Element.text " (see below). Once the "
                        , EH.responder []
                        , Element.text " has done so, he is expected to execute the "
                        , EH.methodName "claim"
                        , Element.text " method. This will move the contract to the "
                        , EH.sectionReference "Claimed Phase"
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , EH.initiator []
                        , Element.text " is expected to communicate any additional information the "
                        , EH.responder []
                        , Element.text " needs to complete the transfer (such as bank account numbers, contact info, etc.), either via CoinerTool's secure messaging or some other medium."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.responder []
                        , Element.text " does not claim within "
                        , EH.timeInput "deposit deadline interval" model.depositDeadlineIntervalInput DepositDeadlineIntervalChanged
                        , Element.text ", "
                        , EH.sectionReference "the contract is closed"
                        , Element.text ", and the balance of "
                        , EH.tokenValue model.postCommitBalance
                        , Element.text " is handled as follows:"
                        , EH.clauseList
                            [ Element.paragraph []
                                [ Element.text "Half of the "
                                , EH.responder []
                                , Element.text "’s deposit ("
                                , EH.tokenValue model.claimFailBurnAmount
                                , Element.text ") is burned and half ("
                                , EH.tokenValue model.claimFailBurnAmount
                                , Element.text ") is returned to the "
                                , EH.responder []
                                , Element.text "."
                                ]
                            , Element.paragraph []
                                [ Element.text "The "
                                , EH.initiator []
                                , Element.text "’s original investment of "
                                , EH.tokenValue model.preCommitBalance
                                , Element.text " is refunded."
                                ]
                            ]
                        ]
                    ]
                ]

        claimedPhaseSection =
            Element.textColumn [ Element.width Element.fill ]
                [ EH.sectionHeading "Claimed Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , EH.sectionReference "Claimed Phase"
                    , Element.text ", the "
                    , EH.initiator []
                    , Element.text " is expected to "
                    , EH.methodName "release"
                    , Element.text " the contract's balance ("
                    , EH.tokenValue model.postCommitBalance
                    , Element.text ") to the "
                    , EH.responder []
                    , Element.text ", or if not, burn it. "
                    , Element.el [ Element.Font.bold ] (Element.text "At this point, in no case does the ")
                    , EH.initiator [ Element.Font.bold ]
                    , Element.el [ Element.Font.bold ] (Element.text " get a refund of his original investment. ")
                    , EH.fakeLink "why?"
                    ]
                , Element.paragraph [] [ Element.text "Specifically:" ]
                , EH.clauseList
                    [ Element.paragraph []
                        [ Element.text "The "
                        , EH.initiator []
                        , Element.text " is expected to verify with certainty whether the "
                        , EH.responder []
                        , Element.text " has irreversibly transferred at least "
                        , EH.usdValue model.uncoiningAmount
                        , Element.text " to the "
                        , EH.initiator []
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has taken place for at least "
                        , EH.usdValue model.uncoiningAmount
                        , Element.text ", the "
                        , EH.initiator []
                        , Element.text " is expected to execute the "
                        , EH.methodName "release"
                        , Element.text " method. This releases the entire balance of the contract ("
                        , EH.tokenValue model.postCommitBalance
                        , Element.text ") to the "
                        , EH.responder []
                        , Element.text " and closes the contract. Thus, the "
                        , EH.responder []
                        , Element.text " will have made a profit of "
                        , EH.tokenValue model.summonFee
                        , Element.text " by servicing this Uncoining contract."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has not taken place, or if the transfer amount was less than "
                        , EH.usdValue model.uncoiningAmount
                        , Element.text ", the "
                        , EH.initiator []
                        , Element.text " is expected to execute the "
                        , EH.methodName "burn"
                        , Element.text " method. This burns the entire balance of the contract ("
                        , EH.tokenValue model.postCommitBalance
                        , Element.text ") and "
                        , EH.sectionReference "closes the contract"
                        , Element.text ". Thus, the "
                        , EH.responder []
                        , Element.text " will have suffered a loss of "
                        , EH.tokenValue model.responderDeposit
                        , Element.text " for failing to make the deposit."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.initiator []
                        , Element.text " is not sure whether the transfer has taken place, the "
                        , EH.initiator []
                        , Element.text " and the "
                        , EH.responder []
                        , Element.text " are expected to communicate to resolve any ambiguity."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , EH.initiator []
                        , Element.text " has not executed a "
                        , EH.methodName "burn"
                        , Element.text " or "
                        , EH.methodName "release"
                        , Element.text " within "
                        , EH.timeInput "autorelease interval" model.autoreleaseIntervalInput AutoreleaseIntervalChanged
                        , Element.text ", "
                        , EH.methodName "release"
                        , Element.text " is triggered automatically."
                        ]
                    ]
                ]

        createButton =
            Element.Input.button []
                { onPress = Just BeginCreateProcess
                , label = Element.text "Create!"
                }
    in
    Element.column [ Element.spacing 20, Element.width Element.fill ]
        [ header
        , body
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
                    :: ([ ( "Uncoining Amount", EH.smallInput "uncoiningAmount" (TokenValue.getString model.uncoiningAmount) UncoiningAmountChanged )
                        , ( "Summon Fee", EH.smallInput "summonfee" (TokenValue.getString model.summonFee) SummonFeeChanged )
                        ]
                            |> List.map nameAndElementToRow
                       )
                )

        transferMethodsInput =
            Element.column [ Element.width (Element.fillPortion 3), Element.spacing 8, Element.alignTop ]
                [ columnHeader "Fiat Transfer Methods"
                , Element.Input.multiline [ Element.width Element.fill, Element.height (Element.px 150) ]
                    { onChange = TransferMethodsChanged
                    , text = model.transferMethods
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
                    ([ ( "Autorecall", EH.timeInput "autorecall interval" model.auotrecallIntervalInput AutorecallIntervalChanged )
                     , ( "Deposit Deadline", EH.timeInput "deposit deadline interval" model.depositDeadlineIntervalInput DepositDeadlineIntervalChanged )
                     , ( "Autorelease", EH.timeInput "autorelease interval" model.autoreleaseIntervalInput AutoreleaseIntervalChanged )
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


outputMaybeUserAddress : Maybe Address -> Element.Element msg
outputMaybeUserAddress maybeUserAddress =
    case maybeUserAddress of
        Just userAddress ->
            Element.text (Eth.Utils.addressToString userAddress)

        Nothing ->
            Element.el [ Element.Font.color (Element.rgb 1 0 0) ] (Element.text "error: no account found. Unlock Metamask?")
