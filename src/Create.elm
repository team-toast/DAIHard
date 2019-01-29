module Create exposing
    ( Model
    , Msg
    , init
    , update
    , updateWithUserAddress
    , viewElement
    )

import Abi.Decode
import BigInt
import ChainCmd exposing (ChainCmdOrder)
import Contracts.ERC20Token as TokenContract
import Contracts.ToastytradeExtras as TTExtras
import Contracts.ToastytradeFactory as TTFactoryContract
import Element
import Element.Font as Font
import Element.Input as Input
import ElementHelpers
import Eth
import Eth.Decode
import Eth.Types exposing (Address, TxReceipt)
import Eth.Utils as EthUtils
import EthHelpers
import Json.Decode
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
    , auotrecallIntervalInput : String
    , depositDeadlineIntervalInput : String
    , autoreleaseIntervalInput : String
    , autorecallInterval : Maybe Time.Posix
    , depositDeadlineInterval : Maybe Time.Posix
    , autoreleaseInterval : Maybe Time.Posix
    }


type OrderType
    = Sell
    | Buy


type Msg
    = UncoiningAmountChanged String
    | SummonFeeChanged String
    | AutorecallIntervalChanged String
    | DepositDeadlineIntervalChanged String
    | AutoreleaseIntervalChanged String
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
                                        "test stringggg"
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
                        [ Element.text "The contract shown below will be created with YOU ("
                        , outputMaybeUserAddress model.userAddress
                        , Element.text ") as the "
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
                        , ElementHelpers.timeInput "autorecall interval" model.auotrecallIntervalInput AutorecallIntervalChanged
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
                        , ElementHelpers.timeInput "deposit deadline interval" model.depositDeadlineIntervalInput DepositDeadlineIntervalChanged
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
                        , ElementHelpers.timeInput "autorelease interval" model.autoreleaseIntervalInput AutoreleaseIntervalChanged
                        , Element.text ", "
                        , ElementHelpers.methodName "release"
                        , Element.text " is triggered automatically."
                        ]
                    ]
                ]

        createButton =
            Input.button []
                { onPress = Just BeginCreateProcess
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


outputMaybeUserAddress : Maybe Address -> Element.Element msg
outputMaybeUserAddress maybeUserAddress =
    case maybeUserAddress of
        Just userAddress ->
            Element.text (EthUtils.addressToString userAddress)

        Nothing ->
            Element.el [ Font.color (Element.rgb255 255 0 0) ] (Element.text "error: no account found. Unlock Metamask?")
