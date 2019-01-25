module Create exposing
    ( Model
    , Msg
    , init
    , update
    , viewElement
    )

import BigInt
import Element
import Element.Font as Font
import Element.Input as Input
import ElementHelpers
import TokenValue exposing (TokenValue)


type alias Model =
    { uncoiningAmount : TokenValue
    , summonFee : TokenValue
    , devFee : TokenValue
    , initialDeposit : TokenValue
    , responderDeposit : TokenValue
    , preCommitBalance : TokenValue
    , postCommitBalance : TokenValue
    , claimFailBurnAmount : TokenValue
    }


type OrderType
    = Sell
    | Buy


type Msg
    = UncoiningAmountChanged String
    | SummonFeeChanged String
    | CreateContract
    | NoOp


init : Int -> ( Model, Cmd Msg )
init tokenDecimals =
    let
        model =
            { uncoiningAmount = TokenValue.tokenValue tokenDecimals "100"
            , summonFee = TokenValue.empty tokenDecimals
            , devFee = TokenValue.empty tokenDecimals
            , initialDeposit = TokenValue.empty tokenDecimals
            , responderDeposit = TokenValue.empty tokenDecimals
            , preCommitBalance = TokenValue.empty tokenDecimals
            , postCommitBalance = TokenValue.empty tokenDecimals
            , claimFailBurnAmount = TokenValue.empty tokenDecimals
            }
    in
    ( propogateUncoiningAmountChange model
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UncoiningAmountChanged newAmtStr ->
            let
                newModel =
                    { model | uncoiningAmount = TokenValue.updateViaString model.uncoiningAmount newAmtStr }
            in
            ( propogateUncoiningAmountChange newModel, Cmd.none )

        SummonFeeChanged newAmtStr ->
            ( { model | summonFee = TokenValue.updateViaString model.summonFee newAmtStr }, Cmd.none )

        CreateContract ->
            let
                _ =
                    Debug.log "" "totes gonna create a contract"
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
                , initialDeposit = TokenValue.updateViaBigInt model.preCommitBalance preCommitBalanceBigInt
                , preCommitBalance = TokenValue.updateViaBigInt model.initialDeposit initialDepositBigInt
                , postCommitBalance = TokenValue.updateViaBigInt model.responderDeposit postCommitBalanceBigInt
                , responderDeposit = TokenValue.updateViaBigInt model.postCommitBalance responderDepositBigInt
                , claimFailBurnAmount = TokenValue.updateViaBigInt model.claimFailBurnAmount claimFailBurnAmountBigInt
            }


viewElement : Model -> Element.Element Msg
viewElement model =
    let
        header =
            Element.el [ Font.size 36 ] (Element.text "Uncoining Contract - draft ")

        openerSection =
            Element.textColumn []
                [ Element.paragraph []
                    [ Element.text "Uncoining "
                    , ElementHelpers.smallInput "uncoiningAmount" (TokenValue.getString model.uncoiningAmount) UncoiningAmountChanged
                    , Element.text " Dai, with a summon fee of "
                    , ElementHelpers.smallInput "summonfee" (TokenValue.getString model.summonFee) SummonFeeChanged
                    , Element.text " Dai."
                    ]
                , Element.paragraph []
                    [ Element.text "Clicking 'Inititalize Contract' at the bottom of this page will cause CoinerTool to request "
                    , ElementHelpers.tokenValue model.initialDeposit
                    , Element.text " from your web3 provider. Once you accept the transaction:"
                    ]
                , ElementHelpers.clauseList
                    [ Element.paragraph []
                        [ Element.text "The contract shown below will be created with YOU (addr) as the "
                        , ElementHelpers.initiator []
                        , Element.text ";"
                        ]
                    , Element.paragraph []
                        [ ElementHelpers.tokenValue model.preCommitBalance
                        , Element.text " will be deposited into the contract;"
                        ]
                    , Element.paragraph []
                        [ ElementHelpers.tokenValue model.devFee
                        , Element.text " will be forwarded to the developers of CoinerTool;"
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , ElementHelpers.sectionReference "Fiat Transfer Methods"
                        , Element.text " section you fill out below will be publicly posted onto the Ethereum blockchain."
                        ]
                    ]
                , Element.paragraph []
                    [ Element.text "The contract begins in the "
                    , ElementHelpers.sectionReference "Open Phase"
                    , Element.text ", and will be discoverable by other users via "
                    , ElementHelpers.fakeLink "CoinerTool Browse"
                    , Element.text " (and potentially third-party tools)."
                    ]
                ]

        openPhaseSection =
            Element.textColumn []
                [ ElementHelpers.sectionHeading "Open Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , ElementHelpers.sectionReference "Open Phase"
                    , Element.text ", the "
                    , ElementHelpers.initiator []
                    , Element.text " waits for a "
                    , ElementHelpers.responder []
                    , Element.text " to appear (which would move the contract to the "
                    , ElementHelpers.sectionReference "Committed Phase"
                    , Element.text "). The "
                    , ElementHelpers.initiator []
                    , Element.text " can cancel anytime with a refund anytime before this happens. "
                    , Element.el [ Font.bold ] (Element.text "This is the ONLY phase in which a refund is a guaranteed option to the ")
                    , ElementHelpers.initiator [ Font.bold ]
                    , Element.el [ Font.bold ] (Element.text ".")
                    ]
                , Element.paragraph [] [ Element.text "Specifically: " ]
                , ElementHelpers.clauseList
                    [ Element.paragraph []
                        [ Element.text "Another Ethereum user may become the "
                        , ElementHelpers.responder []
                        , Element.text " by executing the  "
                        , ElementHelpers.methodName "commit"
                        , Element.text " method, which requires a deposit of "
                        , ElementHelpers.tokenValue model.responderDeposit
                        , Element.text ". This immediately moves the contract to the "
                        , ElementHelpers.sectionReference "Committed Phase"
                        , Element.text ". Thenceforth, the identity of the "
                        , ElementHelpers.responder []
                        , Element.text " will never change."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , ElementHelpers.initiator []
                        , Element.text " may execute the "
                        , ElementHelpers.methodName "recall"
                        , Element.text " method. This immediately "
                        , ElementHelpers.sectionReference "closes the contract"
                        , Element.text " and refunds the "
                        , ElementHelpers.initiator []
                        , Element.text "'s "
                        , ElementHelpers.tokenValue model.preCommitBalance
                        , Element.text ". "
                        ]
                    , Element.paragraph []
                        [ Element.text "If no Ethereum user "
                        , ElementHelpers.methodName "commit"
                        , Element.text "s within "
                        , ElementHelpers.timeInput "autorecall interval" (\t -> NoOp)
                        , Element.text ", "
                        , ElementHelpers.methodName "recall"
                        , Element.text " will be automatically triggered."
                        ]
                    ]
                ]

        commitedPhaseSection =
            Element.textColumn []
                [ ElementHelpers.sectionHeading "Commited Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , ElementHelpers.sectionReference "Committed Phase"
                    , Element.text ", the "
                    , ElementHelpers.responder []
                    , Element.text " is expected to transfer "
                    , ElementHelpers.usdValue model.uncoiningAmount
                    , Element.text " to the "
                    , ElementHelpers.initiator []
                    , Element.text " via one of the "
                    , ElementHelpers.sectionReference "Fiat Transfer Methods"
                    , Element.text " and mark the deposit as complete, moving the contract to the "
                    , ElementHelpers.sectionReference "Claimed Phase"
                    , Element.text "."
                    ]
                , Element.paragraph [] [ Element.text " Specifically:" ]
                , ElementHelpers.clauseList
                    [ Element.paragraph []
                        [ Element.text "The contract has two parties (the "
                        , ElementHelpers.initiator []
                        , Element.text " and the "
                        , ElementHelpers.responder []
                        , Element.text "), and has a total balance of "
                        , ElementHelpers.tokenValue model.postCommitBalance
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , ElementHelpers.responder []
                        , Element.text " is expected to transfer "
                        , ElementHelpers.usdValue model.uncoiningAmount
                        , Element.text " to the "
                        , ElementHelpers.initiator []
                        , Element.text " as described in "
                        , ElementHelpers.sectionReference "Fiat Transfer Methods"
                        , Element.text " (see below). Once the "
                        , ElementHelpers.responder []
                        , Element.text " has done so, he is expected to execute the "
                        , ElementHelpers.methodName "claim"
                        , Element.text " method. This will move the contract to the "
                        , ElementHelpers.sectionReference "Claimed Phase"
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "The "
                        , ElementHelpers.initiator []
                        , Element.text " is expected to communicate any additional information the "
                        , ElementHelpers.responder []
                        , Element.text " needs to complete the transfer (such as bank account numbers, contact info, etc.), either via CoinerTool's secure messaging or some other medium."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , ElementHelpers.responder []
                        , Element.text " does not claim within "
                        , ElementHelpers.timeInput "deposit deadline interval" (\t -> NoOp)
                        , Element.text ", "
                        , ElementHelpers.sectionReference "the contract is closed"
                        , Element.text ", and the balance of "
                        , ElementHelpers.tokenValue model.postCommitBalance
                        , Element.text " is handled as follows:"
                        , ElementHelpers.clauseList
                            [ Element.paragraph []
                                [ Element.text "Half of the "
                                , ElementHelpers.responder []
                                , Element.text "’s deposit ("
                                , ElementHelpers.tokenValue model.claimFailBurnAmount
                                , Element.text ") is burned and half ("
                                , ElementHelpers.tokenValue model.claimFailBurnAmount
                                , Element.text ") is returned to the "
                                , ElementHelpers.responder []
                                , Element.text "."
                                ]
                            , Element.paragraph []
                                [ Element.text "The "
                                , ElementHelpers.initiator []
                                , Element.text "’s original investment of "
                                , ElementHelpers.tokenValue model.preCommitBalance
                                , Element.text " is refunded."
                                ]
                            ]
                        ]
                    ]
                ]

        claimedPhaseSection =
            Element.textColumn []
                [ ElementHelpers.sectionHeading "Claimed Phase"
                , Element.paragraph []
                    [ Element.text "During the "
                    , ElementHelpers.sectionReference "Claimed Phase"
                    , Element.text ", the "
                    , ElementHelpers.initiator []
                    , Element.text " is expected to "
                    , ElementHelpers.methodName "release"
                    , Element.text " the contract's balance ("
                    , ElementHelpers.tokenValue model.postCommitBalance
                    , Element.text ") to the "
                    , ElementHelpers.responder []
                    , Element.text ", or if not, burn it. "
                    , Element.el [ Font.bold ] (Element.text "At this point, in no case does the ")
                    , ElementHelpers.initiator [ Font.bold ]
                    , Element.el [ Font.bold ] (Element.text " get a refund of his original investment. ")
                    , ElementHelpers.fakeLink "why?"
                    ]
                , Element.paragraph [] [ Element.text "Specifically:" ]
                , ElementHelpers.clauseList
                    [ Element.paragraph []
                        [ Element.text "The "
                        , ElementHelpers.initiator []
                        , Element.text " is expected to verify with certainty whether the "
                        , ElementHelpers.responder []
                        , Element.text " has irreversibly transferred at least "
                        , ElementHelpers.usdValue model.uncoiningAmount
                        , Element.text " to the "
                        , ElementHelpers.initiator []
                        , Element.text "."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has taken place for at least "
                        , ElementHelpers.usdValue model.uncoiningAmount
                        , Element.text ", the "
                        , ElementHelpers.initiator []
                        , Element.text " is expected to execute the "
                        , ElementHelpers.methodName "release"
                        , Element.text " method. This releases the entire balance of the contract ("
                        , ElementHelpers.tokenValue model.postCommitBalance
                        , Element.text ") to the "
                        , ElementHelpers.responder []
                        , Element.text " and closes the contract. Thus, the "
                        , ElementHelpers.responder []
                        , Element.text " will have made a profit of "
                        , ElementHelpers.tokenValue model.summonFee
                        , Element.text " by servicing this Uncoining contract."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the transfer has not taken place, or if the transfer amount was less than "
                        , ElementHelpers.usdValue model.uncoiningAmount
                        , Element.text ", the "
                        , ElementHelpers.initiator []
                        , Element.text " is expected to execute the "
                        , ElementHelpers.methodName "burn"
                        , Element.text " method. This burns the entire balance of the contract ("
                        , ElementHelpers.tokenValue model.postCommitBalance
                        , Element.text ") and "
                        , ElementHelpers.sectionReference "closes the contract"
                        , Element.text ". Thus, the "
                        , ElementHelpers.responder []
                        , Element.text " will have suffered a loss of "
                        , ElementHelpers.tokenValue model.responderDeposit
                        , Element.text " for failing to make the deposit."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , ElementHelpers.initiator []
                        , Element.text " is not sure whether the transfer has taken place, the "
                        , ElementHelpers.initiator []
                        , Element.text " and the "
                        , ElementHelpers.responder []
                        , Element.text " are expected to communicate to resolve any ambiguity."
                        ]
                    , Element.paragraph []
                        [ Element.text "If the "
                        , ElementHelpers.initiator []
                        , Element.text " has not executed a "
                        , ElementHelpers.methodName "burn"
                        , Element.text " or "
                        , ElementHelpers.methodName "release"
                        , Element.text " within "
                        , ElementHelpers.timeInput "autorelease interval" (\t -> NoOp)
                        , Element.text ", "
                        , ElementHelpers.methodName "release"
                        , Element.text " is triggered automatically."
                        ]
                    ]
                ]

        createButton =
            Input.button []
                { onPress = Just CreateContract
                , label = Element.text "Create!"
                }
    in
    Element.column [ Element.spacing 50 ]
        [ header
        , openerSection
        , openPhaseSection
        , commitedPhaseSection
        , claimedPhaseSection
        , createButton
        ]
