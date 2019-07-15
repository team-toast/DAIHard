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
import Html.Attributes
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
        , Element.spacing 20
        , Element.paddingEach
            { top = 0
            , right = 40
            , left = 40
            , bottom = 30
            }
        , Element.Events.onClick <| ShowCurrencyDropdown False
        , Element.inFront <|
            getModalOrNone model
        ]
        [ mainInputElement model
        , phasesElement model
        , openButtonElement model.userInfo
        ]


mainInputElement : Model -> Element Msg
mainInputElement model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.Background.color EH.white
        , Element.Border.rounded 5
        , Element.padding 20
        , EH.subtleShadow
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ tradeTypeElement model
            , daiElement model
            , fiatElement model
            ]
        , feeNotifyElement model
        ]


tradeTypeElement : Model -> Element Msg
tradeTypeElement model =
    EH.withHeader
        "Trade Type"
        (roleToggleElement model.web3Context.factoryType model.inputs.userRole)


roleToggleElement : FactoryType -> BuyerOrSeller -> Element Msg
roleToggleElement factoryType userRole =
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
            (Element.text <| "Sell " ++ Config.tokenUnitName factoryType)
        , Element.el
            ([ Element.Events.onClick <| ChangeRole Buyer ] ++ buyDaiStyles)
            (Element.text <| "Buy " ++ Config.tokenUnitName factoryType)
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
            (daiInputElement model.web3Context.factoryType model.inputs.daiAmount model.errors.daiAmount)


daiInputElement : FactoryType -> String -> Maybe String -> Element Msg
daiInputElement factoryType amountString maybeError =
    EH.fancyInput
        [ Element.width <| Element.px 150
        , Element.Font.medium
        , Element.Font.size 24
        , Element.below <|
            EH.maybeErrorElement
                [ inputErrorTag
                , Element.moveDown 5
                ]
                maybeError
        ]
        ( Nothing, Just <| EH.daiSymbolAndLabel factoryType )
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
                [ inputErrorTag
                , Element.moveDown 5
                ]
                maybeError
        ]
        ( Just fiatCharElement, Just <| EH.currencySelector showFiatTypeDropdown typeString (ShowCurrencyDropdown True) FiatTypeChanged )
        "fiat input"
        Nothing
        amountString
        FiatAmountChanged


openButtonElement : Maybe UserInfo -> Element Msg
openButtonElement maybeUserInfo =
    Element.el [ Element.centerX ] <|
        case maybeUserInfo of
            Just userInfo ->
                EH.redButton "Open Trade" (CreateClicked userInfo)

            Nothing ->
                EH.redButton "Connect to Wallet" Web3Connect


feeNotifyElement : Model -> Element Msg
feeNotifyElement model =
    let
        blueText =
            case TokenValue.fromString model.inputs.daiAmount of
                Just daiAmount ->
                    "There is a 1% fee of "
                        ++ TokenValue.toConciseString
                            (TokenValue.div daiAmount 100)
                        ++ " "
                        ++ Config.tokenUnitName model.web3Context.factoryType
                        ++ "."

                Nothing ->
                    "There is a 1% fee."

        regularText =
            "We only collect this fee when trades resolve successfully."
    in
    Element.row
        [ Element.centerX
        , Element.paddingXY 20 10
        , Element.Background.color <| Element.rgb255 10 33 108
        , Element.Border.rounded 8
        , Element.spacing 5
        ]
        [ Element.el
            [ Element.Font.size 18
            , Element.Font.color <| Element.rgb255 0 226 255
            , Element.Font.semiBold
            ]
            (Element.text blueText)
        , Element.el
            [ Element.Font.size 17
            , Element.Font.color EH.white
            , Element.Font.medium
            ]
            (Element.text regularText)
        ]


phasesElement : Model -> Element Msg
phasesElement model =
    Element.column
        [ Element.width Element.fill
        , Element.paddingXY 10 0
        , Element.spacing 20
        ]
        [ openPhaseElement
            model.inputs.autorecallInterval
            model.inputs.userRole
        , committedPhaseElement
            model.inputs.paymentMethod
            model.errors.paymentMethod
            model.inputs.autoabortInterval
            model.inputs.userRole
        , judgmentPhaseElement
            model.inputs.autoreleaseInterval
            model.inputs.userRole
        ]


openPhaseElement : Time.Posix -> BuyerOrSeller -> Element Msg
openPhaseElement interval userRole =
    Element.el
        [ Element.padding 8
        , Element.Border.rounded 8
        , Element.Background.color EH.lightBlue
        , Element.Border.shadow
            { offset = ( -3, 3 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.3
            }
        ]
    <|
        phaseElement
            Images.openWindowIcon
            "Open Window"
            (openWindowSummary userRole)
            interval
            Nothing
            AutorecallIntervalChanged


committedPhaseElement : String -> Maybe String -> Time.Posix -> BuyerOrSeller -> Element Msg
committedPhaseElement paymentMethodText maybeError interval userRole =
    Element.column
        [ Element.padding 8
        , Element.spacing 15
        , Element.Border.rounded 8
        , Element.Background.color EH.lightBlue
        , Element.Border.shadow
            { offset = ( -3, 3 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.3
            }
        ]
        [ phaseElement
            Images.paymentWindowIcon
            "Payment Window"
            (paymentWindowSummary userRole)
            interval
            (Just EH.red)
            AutoabortIntervalChanged
        , paymentMethodsElement maybeError userRole paymentMethodText
        ]


judgmentPhaseElement : Time.Posix -> BuyerOrSeller -> Element Msg
judgmentPhaseElement interval userRole =
    Element.el
        [ Element.padding 8
        , Element.Border.rounded 8
        , Element.Background.color EH.lightBlue
        , Element.Border.shadow
            { offset = ( -3, 3 )
            , size = 0
            , blur = 5
            , color = Element.rgba 0 0 0 0.3
            }
        ]
    <|
        phaseElement
            Images.releaseWindowIcon
            "Judgment Window"
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
            "After committing, you and the Seller have this long to complete the fiat payment, using one of your payment methods indicated below. If you fail to confirm payment within this window, 1/4 of your deposit is burned from both parties and the rest is refunded."

        Seller ->
            "After committing, you and the Buyer have this long complete the fiat payment, using one of your payment methods indicated below. If the Buyer aborts or fails to confirm within this window, 1/12 of the trade amount is burned from both parties and the rest is refunded."


releaseWindowSummary : BuyerOrSeller -> String
releaseWindowSummary userRole =
    case userRole of
        Buyer ->
            "Once you confirm payment, the Seller has this time window to decide whether to release the funds to you or burn everything. If he doesn't decide before the time is up, funds are released to you by default."

        Seller ->
            "Once the Buyer confirms payment, you have this long to decide whether to release the funds to the Buyer or, in the case of an attempted scam, burn everything. If you don't decide before the time is up, funds are released to the Buyer by default."


paymentMethodsElement : Maybe String -> BuyerOrSeller -> String -> Element Msg
paymentMethodsElement maybeError initiatorRole inputText =
    let
        titleElement =
            Element.el
                [ Element.Font.size 22
                , Element.Font.semiBold
                ]
                (Element.text "Payment Method")

        inputElement =
            Element.Input.multiline
                [ Element.width Element.fill
                , Element.height <| Element.px 150
                , Element.Background.color <| Element.rgba 1 1 1 0.5
                ]
                { onChange = ChangePaymentMethodText
                , text = inputText
                , placeholder =
                    if inputText == "" then
                        Just <| inputPlaceholder initiatorRole

                    else
                        Nothing
                , label = Element.Input.labelHidden "payment method"
                , spellcheck = True
                }
    in
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        , Element.above <|
            EH.maybeErrorElement
                [ inputErrorTag
                , Element.moveDown 30
                , Element.padding 10
                , Element.Font.size 20
                , Element.width <| Element.px 400
                ]
                maybeError
        ]
        [ titleElement
        , inputElement
        ]


phaseElement : Image -> String -> String -> Time.Posix -> Maybe Element.Color -> (Time.Posix -> Msg) -> Element Msg
phaseElement icon title summary interval lowIntervalColor newIntervalMsg =
    let
        iconElement =
            Element.el
                [ Element.width <| Element.px 80 ]
                (Images.toElement [ Element.centerX ] icon)

        descriptionElement =
            Element.paragraph
                [ Element.Font.size 17
                , Element.Font.medium
                , Element.Font.color EH.permanentTextColor
                ]
                [ Element.text summary ]

        intervalAndTitleElement =
            Element.column
                [ Element.spacing 6 ]
                [ Element.el
                    [ Element.Font.size 22
                    , Element.Font.semiBold
                    ]
                    (Element.text title)
                , EH.intervalInput lowIntervalColor interval newIntervalMsg
                ]
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 15
        , Element.Border.rounded 10
        ]
        [ iconElement
        , intervalAndTitleElement
        , descriptionElement
        ]


getModalOrNone : Model -> Element Msg
getModalOrNone model =
    case model.txChainStatus of
        Nothing ->
            Element.none

        Just txChainStatus ->
            txChainStatusModal txChainStatus model


txChainStatusModal : TxChainStatus -> Model -> Element Msg
txChainStatusModal txChainStatus model =
    case txChainStatus of
        Confirm createParameters ->
            let
                tokenUnitName =
                    Config.tokenUnitName model.web3Context.factoryType

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
                                [ Element.centerX
                                , Element.Font.size 18
                                , Element.Font.medium
                                , Element.Font.color EH.permanentTextColor
                                ]
                            )
                            (getWarningParagraphs createParameters
                                ++ [ [ Element.text <| "You will deposit "
                                     , depositAmountEl
                                     , Element.text <| " " ++ tokenUnitName ++ " (including the 1% dev fee) to open this trade."
                                     ]
                                   ]
                                ++ (case model.web3Context.factoryType of
                                        Token _ ->
                                            [ [ Element.text <| "This ususally requires two Metamask signatures. Your " ++ tokenUnitName ++ " will not be deposited until the final transaction has been mined." ] ]

                                        Native _ ->
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
                    { url = EthHelpers.makeViewTxUrl model.web3Context.factoryType txHash
                    , label = Element.text "See the transaction on Etherscan"
                    }
                , Element.text "Funds will not leave your wallet until you sign the next transaction."
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
                    { url = EthHelpers.makeViewTxUrl model.web3Context.factoryType txHash
                    , label = Element.text "See the transaction on Etherscan"
                    }
                , Element.text "You will be redirected when it's mined."
                ]


getWarningParagraphs : CTypes.CreateParameters -> List (List (Element Msg))
getWarningParagraphs createParameters =
    [ if TimeHelpers.compare createParameters.autoreleaseInterval (Time.millisToPosix (1000 * 60 * 20)) == LT then
        Just <|
            case createParameters.initiatorRole of
                Buyer ->
                    "That Release Window time is quite small! It might take a while to find a committed Seller."

                Seller ->
                    "That Release Window time is quite small! This may attract scammers. Only create this trade if you know what you're doing."

      else
        Nothing
    , if TimeHelpers.compare createParameters.autoabortInterval (Time.millisToPosix (1000 * 60 * 60)) == LT then
        Just <|
            case createParameters.initiatorRole of
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


inputPlaceholder : BuyerOrSeller -> Element.Input.Placeholder Msg
inputPlaceholder initiatorRole =
    Element.Input.placeholder
        []
        (case initiatorRole of
            Seller ->
                Element.text """Some examples:

I can accept transfers to a Schwab bank account (routing 121202211)
I can meet in person to accept cash in London, weekdays after 6, with a day of notice.
Hide the cash in Hume Park, Bulawayo, and tell me the location over chat."""

            Buyer ->
                Element.text """Some examples:

I can deliver cash anywhere within an hour drive of Phoneix, AZ, with 2 days of notice.
TransferWise
Interac e-Transfer
"""
        )


inputErrorTag : Attribute Msg
inputErrorTag =
    Element.htmlAttribute <| Html.Attributes.id "inputError"
