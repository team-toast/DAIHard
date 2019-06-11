module Create.View exposing (root)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Create.PMWizard.View as PMWizard
import Create.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FiatValue
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Images exposing (Image)
import List.Extra
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Time
import TokenValue exposing (TokenValue)


root : Model -> Element Msg
root model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 40
        , Element.Events.onClick <| ShowCurrencyDropdown False
        , Element.inFront <|
            getModalOrNone model
        ]
        [ mainInputElement model
        , devFeeNotifyElement model
        , phasesElement model
        , Element.el
            [ Element.width Element.fill
            , Element.above <|
                EH.maybeErrorElement
                    [ Element.moveDown 5
                    , Element.padding 10
                    , Element.Font.size 20
                    , Element.width <| Element.px 400
                    ]
                    model.errors.paymentMethods
            ]
            (PaymentMethods.viewList
                model.inputs.paymentMethods
                (if List.length model.inputs.paymentMethods == 0 then
                    Just OpenPMWizard

                 else
                    Nothing
                )
            )
        ]


mainInputElement : Model -> Element Msg
mainInputElement model =
    EH.niceFloatingRow
        [ tradeTypeElement model
        , daiElement model
        , fiatElement model
        , marginElement model
        , buttonsElement model
        ]


tradeTypeElement : Model -> Element Msg
tradeTypeElement model =
    EH.withHeader
        "Trade Type"
        (roleToggleElement model.inputs.userRole)


roleToggleElement : BuyerOrSeller -> Element Msg
roleToggleElement userRole =
    let
        baseStyles =
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.pointer
            ]

        ( buyDaiStyles, sellDaiStyles ) =
            case userRole of
                Buyer ->
                    ( baseStyles
                    , baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    )

                Seller ->
                    ( baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    , baseStyles
                    )
    in
    Element.row [ Element.spacing 20 ]
        [ Element.el
            ([ Element.Events.onClick <| ChangeRole Seller ] ++ sellDaiStyles)
            (Element.text "Sell DAI")
        , Element.el
            ([ Element.Events.onClick <| ChangeRole Buyer ] ++ buyDaiStyles)
            (Element.text "Buy DAI")
        ]


daiElement : Model -> Element Msg
daiElement model =
    EH.niceBottomBorderEl <|
        EH.withHeader
            (case model.inputs.userRole of
                Buyer ->
                    "You're buying"

                Seller ->
                    "You're selling"
            )
            (daiInputElement model.inputs.daiAmount model.errors.daiAmount)


daiInputElement : String -> Maybe String -> Element Msg
daiInputElement amountString maybeError =
    EH.fancyInput
        [ Element.width <| Element.px 250
        , Element.Font.medium
        , Element.Font.size 24
        , Element.below <|
            EH.maybeErrorElement
                [ Element.moveDown 5 ]
                maybeError
        ]
        ( Nothing, Just <| EH.daiSymbolAndLabel )
        "dai input"
        Nothing
        amountString
        TradeAmountChanged


fiatElement : Model -> Element Msg
fiatElement model =
    EH.niceBottomBorderEl <|
        EH.withHeader
            "For fiat"
            (fiatInputElement
                model.inputs.fiatType
                model.inputs.fiatAmount
                model.showFiatTypeDropdown
                model.errors.fiat
            )


fiatInputElement : String -> String -> Bool -> Maybe String -> Element Msg
fiatInputElement typeString amountString showFiatTypeDropdown maybeError =
    let
        fiatCharElement =
            Element.text <| FiatValue.typeStringToCharStringDefaultEmpty typeString
    in
    EH.fancyInput
        [ Element.width <| Element.px 250
        , Element.Font.medium
        , Element.Font.size 24
        , Element.below <|
            EH.maybeErrorElement
                [ Element.moveDown 5 ]
                maybeError
        ]
        ( Just fiatCharElement, Just <| EH.currencySelector showFiatTypeDropdown typeString (ShowCurrencyDropdown True) FiatTypeChanged )
        "fiat input"
        Nothing
        amountString
        FiatAmountChanged


marginElement : Model -> Element Msg
marginElement model =
    case model.inputs.fiatType of
        "USD" ->
            EH.niceBottomBorderEl <|
                EH.withHeader
                    "At margin"
                    (marginInputElement
                        model.inputs.margin
                        (model.inputs.userRole == Seller)
                        model.errors.margin
                    )

        _ ->
            EH.comingSoonMsg [ Element.width <| Element.px 150 ] "Margin for non-USD currencies coming soon!"


marginInputElement : String -> Bool -> Maybe String -> Element Msg
marginInputElement marginString upIsGreen maybeError =
    let
        ( color, arrowImage ) =
            case interpretMarginString marginString of
                Just margin ->
                    if margin == 0 then
                        ( EH.black, Images.none )

                    else if xor (margin > 0) upIsGreen then
                        ( EH.red, Images.marginSymbol (margin > 0) (Just False) )

                    else
                        ( EH.green, Images.marginSymbol (margin > 0) (Just True) )

                Nothing ->
                    ( EH.black, Images.qmarkCircle )

        percentAndArrowElement =
            Element.row [ Element.spacing 8 ]
                [ Element.text "%"
                , Images.toElement [] arrowImage
                ]
    in
    EH.fancyInput
        [ Element.width <| Element.px 150
        , Element.Font.medium
        , Element.Font.size 24
        , Element.Font.color color
        , Element.below <|
            EH.maybeErrorElement
                [ Element.moveDown 5 ]
                maybeError
        ]
        ( Nothing, Just percentAndArrowElement )
        "margin input"
        Nothing
        marginString
        MarginStringChanged


buttonsElement : Model -> Element Msg
buttonsElement model =
    Element.row
        [ Element.spacing 10 ]
        [ EH.blueButton "Clear Draft" ClearDraft
        , case model.userInfo of
            Just userInfo ->
                EH.redButton "Open Trade" (CreateClicked userInfo)

            Nothing ->
                EH.disabledButton "Open Trade" (Just "No account detected. Is Metamask unlocked?")
        ]


devFeeNotifyElement : Model -> Element Msg
devFeeNotifyElement model =
    let
        topText =
            case TokenValue.fromString model.inputs.daiAmount of
                Just daiAmount ->
                    "There is a 1% fee of "
                        ++ TokenValue.toConciseString
                            (TokenValue.div daiAmount 100)
                        ++ " DAI."

                Nothing ->
                    "There is a 1% fee."

        bottomText =
            "This dev fee is only collected when trades resolve successfully. If the trade is burned or aborted, we all lose out."
    in
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 40 0
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.paddingXY 30 20
            , Element.Background.color <| Element.rgb255 10 33 108
            , Element.Border.rounded 8
            , Element.spacing 3
            ]
            [ Element.el
                [ Element.Font.size 18
                , Element.Font.color <| Element.rgb255 0 226 255
                , Element.Font.semiBold
                ]
                (Element.text topText)
            , Element.el
                [ Element.Font.size 17
                , Element.Font.color EH.white
                , Element.Font.medium
                ]
                (Element.text bottomText)
            ]


phasesElement : Model -> Element Msg
phasesElement model =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY 40 0
        , Element.spacing 20
        ]
        [ Element.el [ Element.width <| Element.fillPortion 1 ] <|
            openPhaseElement
                model.inputs.autorecallInterval
                model.inputs.userRole
        , Element.el [ Element.width <| Element.fillPortion 1 ] <|
            committedPhaseElement
                model.inputs.autoabortInterval
                model.inputs.userRole
        , Element.el [ Element.width <| Element.fillPortion 1 ] <|
            judgmentPhaseElement
                model.inputs.autoreleaseInterval
                model.inputs.userRole
        ]


openPhaseElement : Time.Posix -> BuyerOrSeller -> Element Msg
openPhaseElement interval userRole =
    phaseElement
        Images.openWindowIcon
        "Open Window"
        (openWindowSummary userRole)
        interval
        Nothing
        AutorecallIntervalChanged


committedPhaseElement : Time.Posix -> BuyerOrSeller -> Element Msg
committedPhaseElement interval userRole =
    phaseElement
        Images.paymentWindowIcon
        "Payment Window"
        (paymentWindowSummary userRole)
        interval
        (Just EH.red)
        AutoabortIntervalChanged


judgmentPhaseElement : Time.Posix -> BuyerOrSeller -> Element Msg
judgmentPhaseElement interval userRole =
    phaseElement
        Images.releaseWindowIcon
        "Release Window"
        (releaseWindowSummary userRole)
        interval
        (Just EH.red)
        AutoreleaseIntervalChanged


openWindowSummary : BuyerOrSeller -> String
openWindowSummary userRole =
    let
        committingParty =
            case userRole of
                Buyer ->
                    "Seller"

                Seller ->
                    "Buyer"
    in
    "The offer will expire by this time window if a "
        ++ committingParty
        ++ " does not commit to the trade, returning the balance and the devFee to your wallet. This can also be manually triggered anytime before a "
        ++ committingParty
        ++ " commits."


paymentWindowSummary : BuyerOrSeller -> String
paymentWindowSummary userRole =
    case userRole of
        Buyer ->
            "After committing, you have this time window to send the fiat funds to the Seller using one of your payment methods indicated below. If you fail to confirm payment within this window, 1/4 of your deposit is burned from both parties and the rest is refunded."

        Seller ->
            "After committing, the Buyer has this time window to send the fiat funds to you using one of your payment methods indicated below. If the Buyer aborts or fails to confirm within this window, 1/12 of the trade amount is burned from both parties and the rest is refunded."


releaseWindowSummary : BuyerOrSeller -> String
releaseWindowSummary userRole =
    case userRole of
        Buyer ->
            "Once you confirm payment, the Seller has this time window to decide whether to release the funds to you or burn everything. If he doesn't decide before the time is up, funds are released to you by default."

        Seller ->
            "Once the Buyer confirms payment, you have this time window to decide whether to release the funds to the Buyer or burn everything. If you don't decide before the time is up, funds are released to the Buyer by default."


phaseElement : Image -> String -> String -> Time.Posix -> Maybe Element.Color -> (Time.Posix -> Msg) -> Element Msg
phaseElement icon title summary interval lowIntervalColor newIntervalMsg =
    let
        descriptionElement =
            Element.column
                [ Element.spacing 15
                , Element.width Element.fill
                , Element.height <| Element.px 220
                ]
                [ Images.toElement [] icon
                , Element.column
                    [ Element.spacing 6 ]
                    [ Element.el
                        [ Element.Font.size 22
                        , Element.Font.semiBold
                        ]
                        (Element.text title)
                    , Element.paragraph
                        [ Element.Font.size 17
                        , Element.Font.medium
                        , Element.Font.color EH.permanentTextColor
                        ]
                        [ Element.text summary ]
                    ]
                ]

        intervalElement =
            Element.el
                [ Element.paddingXY 100 0
                , Element.width Element.fill
                ]
                (EH.intervalInput lowIntervalColor interval newIntervalMsg)
    in
    Element.column
        [ Element.width Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundTopCorners 8
            , Element.Border.color EH.lightGray
            , Element.Border.widthEach
                { bottom = 2
                , top = 0
                , right = 0
                , left = 0
                }
            , Element.paddingXY 62 42
            ]
            descriptionElement
        , Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundBottomCorners 8
            , Element.paddingXY 0 42
            ]
            (Element.el
                [ Element.centerX
                , Element.width Element.fill
                ]
                intervalElement
            )
        ]


getModalOrNone : Model -> Element Msg
getModalOrNone model =
    case model.txChainStatus of
        Nothing ->
            case model.addPMModal of
                Nothing ->
                    Element.none

                Just pmModal ->
                    EH.modal (Element.rgba 0 0 0 0.6) <|
                        Element.map
                            PMWizardMsg
                            (PMWizard.root pmModal model.inputs.userRole)

        Just txChainStatus ->
            txChainStatusModal txChainStatus model


txChainStatusModal : TxChainStatus -> Model -> Element Msg
txChainStatusModal txChainStatus model =
    case txChainStatus of
        Confirm createParameters ->
            let
                ( depositAmountEl, confirmButton ) =
                    case model.depositAmount of
                        Just depositAmount ->
                            ( TokenValue.tokenValue depositAmount
                                |> TokenValue.toConciseString
                                |> Element.text
                            , EH.redButton "Yes, I definitely want to open this trade." (ConfirmCreate createParameters depositAmount)
                            )

                        Nothing ->
                            ( Element.text "??"
                            , EH.disabledButton "(loading exact fees...)" Nothing
                            )
            in
            EH.closeableModal
                (Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.height Element.fill
                    , Element.Font.center
                    ]
                    [ Element.el
                        [ Element.Font.size 26
                        , Element.Font.semiBold
                        , Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Just to Confirm...")
                    , Element.column
                        [ Element.spacing 20
                        , Element.centerX
                        , Element.centerY
                        ]
                        (List.map
                            (Element.paragraph
                                [ Element.width <| Element.px 500
                                , Element.centerX
                                , Element.Font.size 18
                                , Element.Font.medium
                                , Element.Font.color EH.permanentTextColor
                                ]
                            )
                            (getWarningParagraphs createParameters
                                ++ [ [ Element.text <| "You will deposit "
                                     , depositAmountEl
                                     , Element.text " DAI (including the 1% dev fee) to open this trade."
                                     ]
                                   ]
                                ++ (case model.node.network of
                                        Eth _ ->
                                            [ [ Element.text <| "This ususally requires two Metamask signatures. Your DAI will not be deposited until the final transaction has been mined." ] ]

                                        XDai ->
                                            []
                                   )
                            )
                        )
                    , Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        ]
                        confirmButton
                    ]
                )
                AbortCreate

        ApproveNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the approve call."
                , Element.text "(check Metamask!)"
                , Element.text "Note that there will be a second transaction to sign after this."
                ]

        ApproveMining createParameters txHash ->
            EH.txProcessModal
                [ Element.text "Mining the initial approve transaction..."
                , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                    { url = EthHelpers.makeViewTxUrl model.node.network txHash
                    , label = Element.text "See the transaction on Etherscan"
                    }
                , Element.text "Funds will not be sent until you sign the next transaction."
                ]

        CreateNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the create call."
                , Element.text "(check Metamask!)"
                ]

        CreateMining txHash ->
            EH.txProcessModal
                [ Element.text "Mining the final create call..."
                , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                    { url = EthHelpers.makeViewTxUrl model.node.network txHash
                    , label = Element.text "See the transaction on Etherscan"
                    }
                , Element.text "You will be redirected when it's mined."
                ]


getWarningParagraphs : CTypes.CreateParameters -> List (List (Element Msg))
getWarningParagraphs createParameters =
    [ if TimeHelpers.compare createParameters.autoreleaseInterval (Time.millisToPosix (1000 * 60 * 20)) == LT then
        Just <|
            case createParameters.initiatingParty of
                Buyer ->
                    "That Release Window time is quite small! It might take a while to find a committed Seller."

                Seller ->
                    "That Release Window time is quite small! This may attract scammers. Only create this trade if you know what you're doing."

      else
        Nothing
    , if TimeHelpers.compare createParameters.autoabortInterval (Time.millisToPosix (1000 * 60 * 60)) == LT then
        Just <|
            case createParameters.initiatingParty of
                Buyer ->
                    "That Payment Window time is quite small! If you fail to to 1. make the payment and 2. click \"confirm\" before this time is up, the trade will automatically abort, incurring the abort punishments on both parties."

                Seller ->
                    "That Payment Window time is quite small! If the Buyer fails to to 1. make the payment and 2. click \"confirm\" before this time is up, the trade will automatically abort, incurring the abort punishments on both parties."

      else
        Nothing
    ]
        |> Maybe.Extra.values
        |> List.map
            (\message ->
                [ Element.el [ Element.Font.color EH.red ] <| Element.text "Caution! "
                , Element.text message
                ]
            )
