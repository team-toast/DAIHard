module Create exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import BigInt
import Html exposing (..)
import Html.Events exposing (onClick)
import HtmlElements
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

                summonFee =
                    TokenValue.updateViaBigInt model.summonFee summonFeeBigInt

                devFeeBigInt =
                    BigInt.div uncoiningAmountBigInt (BigInt.fromInt 100)

                devFee =
                    TokenValue.updateViaBigInt model.devFee devFeeBigInt

                preCommitBalanceBigInt =
                    BigInt.add uncoiningAmountBigInt summonFeeBigInt

                preCommitBalance =
                    TokenValue.updateViaBigInt model.preCommitBalance preCommitBalanceBigInt

                initialDepositBigInt =
                    BigInt.add preCommitBalanceBigInt devFeeBigInt

                initialDeposit =
                    TokenValue.updateViaBigInt model.initialDeposit initialDepositBigInt

                responderDepositBigInt =
                    BigInt.div uncoiningAmountBigInt (BigInt.fromInt 2)

                responderDeposit =
                    TokenValue.updateViaBigInt model.responderDeposit responderDepositBigInt

                postCommitBalanceBigInt =
                    BigInt.add initialDepositBigInt responderDepositBigInt

                postCommitBalance =
                    TokenValue.updateViaBigInt model.postCommitBalance postCommitBalanceBigInt

                claimFailBurnAmountBigInt =
                    BigInt.div responderDepositBigInt (BigInt.fromInt 2)

                claimFailBurnAmount =
                    TokenValue.updateViaBigInt model.claimFailBurnAmount claimFailBurnAmountBigInt
            in
            { model
                | summonFee = summonFee
                , devFee = devFee
                , initialDeposit = initialDeposit
                , preCommitBalance = preCommitBalance
                , postCommitBalance = postCommitBalance
                , responderDeposit = responderDeposit
                , claimFailBurnAmount = claimFailBurnAmount
            }


view : Model -> Html Msg
view model =
    let
        header =
            h1 [] [ text "Uncoining Contract - draft" ]

        openerSection =
            div []
                [ p []
                    [ text "Uncoining "
                    , HtmlElements.smallInput (TokenValue.getString model.uncoiningAmount) UncoiningAmountChanged
                    , text " Dai, with a summon fee of "
                    , HtmlElements.smallInput (TokenValue.getString model.summonFee) SummonFeeChanged
                    , text " Dai."
                    ]
                , p []
                    [ text "Clicking 'Inititalize Contract' at the bottom of this page will cause CoinerTool to request "
                    , HtmlElements.tokenValue model.initialDeposit
                    , text " from your web3 provider. Once you accept the transaction:"
                    , ul []
                        [ li []
                            [ text "The contract shown below will be created with YOU (addr) as the "
                            , HtmlElements.initiator
                            , text ";"
                            ]
                        , li []
                            [ HtmlElements.tokenValue model.preCommitBalance
                            , text " will be deposited into the contract;"
                            ]
                        , li []
                            [ HtmlElements.tokenValue model.devFee
                            , text " will be forwarded to the developers of CoinerTool;"
                            ]
                        , li []
                            [ text "The "
                            , HtmlElements.sectionReference "Fiat Transfer Methods"
                            , text " section you fill out below will be publicly posted onto the Ethereum blockchain."
                            ]
                        ]
                    ]
                , p []
                    [ text "The contract begins in the "
                    , HtmlElements.sectionReference "Open Phase"
                    , text ", and will be discoverable by other users via "
                    , HtmlElements.fakeLink "CoinerTool Browse"
                    , text " (and potentially third-party tools)."
                    ]
                ]

        openPhaseSection =
            div []
                [ h2 [] [ text "Open Phase" ]
                , p []
                    [ text "During the "
                    , HtmlElements.sectionReference "Open Phase"
                    , text ", the "
                    , HtmlElements.initiator
                    , text " waits for a "
                    , HtmlElements.responder
                    , text " to appear (which would move the contract to the "
                    , HtmlElements.sectionReference "Committed Phase"
                    , text "). The "
                    , HtmlElements.initiator
                    , text " can cancel anytime with a refund anytime before this happens. "
                    , b []
                        [ text "This is the ONLY phase in which a refund is a guaranteed option to the "
                        , HtmlElements.initiator
                        , text "."
                        ]
                    ]
                , p [] [ text "Specifically: " ]
                , ul []
                    [ li []
                        [ text "Another Ethereum user may become the "
                        , HtmlElements.responder
                        , text " by executing the  "
                        , HtmlElements.methodName "commit"
                        , text " method, which requires a deposit of "
                        , HtmlElements.tokenValue model.responderDeposit
                        , text ". This immediately moves the contract to the "
                        , HtmlElements.sectionReference "Committed Phase"
                        , text ". Thenceforth, the identity of the "
                        , HtmlElements.responder
                        , text " will never change."
                        ]
                    , li []
                        [ text "The "
                        , HtmlElements.initiator
                        , text " may execute the "
                        , HtmlElements.methodName "recall"
                        , text " method. This immediately "
                        , HtmlElements.sectionReference "closes the contract"
                        , text " and refunds the "
                        , HtmlElements.initiator
                        , text "'s "
                        , HtmlElements.tokenValue model.preCommitBalance
                        , text ". "
                        ]
                    , li []
                        [ text "If no Ethereum user "
                        , HtmlElements.methodName "commit"
                        , text "s within "
                        , HtmlElements.timeInput
                        , text ", "
                        , HtmlElements.methodName "recall"
                        , text " will be automatically triggered."
                        ]
                    ]
                ]

        commitedPhaseSection =
            div []
                [ h2 [] [ text "Commited Phase" ]
                , p []
                    [ text "During the "
                    , HtmlElements.sectionReference "Committed Phase"
                    , text ", the "
                    , HtmlElements.responder
                    , text " is expected to transfer "
                    , HtmlElements.usdValue model.uncoiningAmount
                    , text " to the "
                    , HtmlElements.initiator
                    , text " via one of the "
                    , HtmlElements.sectionReference "Fiat Transfer Methods"
                    , text " and mark the deposit as complete, moving the contract to the "
                    , HtmlElements.sectionReference "Claimed Phase"
                    , text "."
                    ]
                , p [] [ text " Specifically:" ]
                , ul []
                    [ li []
                        [ text "The contract has two parties (the "
                        , HtmlElements.initiator
                        , text " and the "
                        , HtmlElements.responder
                        , text "), and has a total balance of "
                        , HtmlElements.tokenValue model.postCommitBalance
                        , text "."
                        ]
                    , li []
                        [ text "The "
                        , HtmlElements.responder
                        , text " is expected to transfer "
                        , HtmlElements.usdValue model.uncoiningAmount
                        , text " to the "
                        , HtmlElements.initiator
                        , text " as described in "
                        , HtmlElements.sectionReference "Fiat Transfer Methods"
                        , text " (see below). Once the "
                        , HtmlElements.responder
                        , text " has done so, he is expected to execute the "
                        , HtmlElements.methodName "claim"
                        , text " method. This will move the contract to the "
                        , HtmlElements.sectionReference "Claimed Phase"
                        , text "."
                        ]
                    , li []
                        [ text "The "
                        , HtmlElements.initiator
                        , text " is expected to communicate any additional information the "
                        , HtmlElements.responder
                        , text " needs to complete the transfer (such as bank account numbers, contact info, etc.), either via CoinerTool's secure messaging or some other medium."
                        ]
                    , li []
                        [ text "If the "
                        , HtmlElements.responder
                        , text " does not claim within "
                        , HtmlElements.timeInput
                        , text ", "
                        , HtmlElements.sectionReference "the contract is closed"
                        , text ", and the balance of "
                        , HtmlElements.tokenValue model.postCommitBalance
                        , text " is handled as follows:"
                        , ul []
                            [ li []
                                [ text "Half of the "
                                , HtmlElements.responder
                                , text "’s deposit ("
                                , HtmlElements.tokenValue model.claimFailBurnAmount
                                , text ") is burned and half ("
                                , HtmlElements.tokenValue model.claimFailBurnAmount
                                , text ") is returned to the "
                                , HtmlElements.responder
                                , text "."
                                ]
                            , li []
                                [ text "The "
                                , HtmlElements.initiator
                                , text "’s original investment of "
                                , HtmlElements.tokenValue model.preCommitBalance
                                , text " is refunded."
                                ]
                            ]
                        ]
                    ]
                ]

        claimedPhaseSection =
            div []
                [ h2 [] [ text "Claimed Phase" ]
                , p []
                    [ text "During the "
                    , HtmlElements.sectionReference "Claimed Phase"
                    , text ", the "
                    , HtmlElements.initiator
                    , text " is expected to "
                    , HtmlElements.methodName "release"
                    , text " the contract's balance ("
                    , HtmlElements.tokenValue model.postCommitBalance
                    , text ") to the "
                    , HtmlElements.responder
                    , text ", or if not, burn it. "
                    , b []
                        [ text "At this point, in no case does the "
                        , HtmlElements.initiator
                        , text " get a refund of his original investment. "
                        , HtmlElements.fakeLink "why?"
                        ]
                    ]
                , p [] [ text "Specifically:" ]
                , ul []
                    [ li []
                        [ text "The "
                        , HtmlElements.initiator
                        , text " is expected to verify with certainty whether the "
                        , HtmlElements.responder
                        , text " has irreversibly transferred at least "
                        , HtmlElements.usdValue model.uncoiningAmount
                        , text " to the "
                        , HtmlElements.initiator
                        , text "."
                        ]
                    , li []
                        [ text "If the transfer has taken place for at least "
                        , HtmlElements.usdValue model.uncoiningAmount
                        , text ", the "
                        , HtmlElements.initiator
                        , text " is expected to execute the "
                        , HtmlElements.methodName "release"
                        , text " method. This releases the entire balance of the contract ("
                        , HtmlElements.tokenValue model.postCommitBalance
                        , text ") to the "
                        , HtmlElements.responder
                        , text " and closes the contract. Thus, the "
                        , HtmlElements.responder
                        , text " will have made a profit of "
                        , HtmlElements.tokenValue model.summonFee
                        , text " by servicing this Uncoining contract."
                        ]
                    , li []
                        [ text "If the transfer has not taken place, or if the transfer amount was less than "
                        , HtmlElements.usdValue model.uncoiningAmount
                        , text ", the "
                        , HtmlElements.initiator
                        , text " is expected to execute the "
                        , HtmlElements.methodName "burn"
                        , text " method. This burns the entire balance of the contract ("
                        , HtmlElements.tokenValue model.postCommitBalance
                        , text ") and "
                        , HtmlElements.sectionReference "closes the contract"
                        , text ". Thus, the "
                        , HtmlElements.responder
                        , text " will have suffered a loss of "
                        , HtmlElements.tokenValue model.responderDeposit
                        , text " for failing to make the deposit."
                        ]
                    , li []
                        [ text "If the "
                        , HtmlElements.initiator
                        , text " is not sure whether the transfer has taken place, the "
                        , HtmlElements.initiator
                        , text " and the "
                        , HtmlElements.responder
                        , text " are expected to communicate to resolve any ambiguity."
                        ]
                    , li []
                        [ text "If the "
                        , HtmlElements.initiator
                        , text " has not executed a "
                        , HtmlElements.methodName "burn"
                        , text " or "
                        , HtmlElements.methodName "release"
                        , text " within "
                        , HtmlElements.timeInput
                        , text ", "
                        , HtmlElements.methodName "release"
                        , text " is triggered automatically."
                        ]
                    ]
                ]

        createButton =
            div []
                [ button [ onClick CreateContract ] [ text "Create!" ] ]
    in
    div []
        [ header
        , openerSection
        , openPhaseSection
        , commitedPhaseSection
        , claimedPhaseSection
        , createButton
        ]
