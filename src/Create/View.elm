module Create.View exposing (root)

import AppCmd exposing (AppCmd)
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
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Html.Attributes
import Images exposing (Image)
import List.Extra
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Prices
import Time
import TokenValue exposing (TokenValue)
import Wallet


root : Model -> Element Msg
root model =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
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
        , openButtonElement model.wallet
        ]


mainInputElement : Model -> Element Msg
mainInputElement model =
    let
        factory =
            Wallet.factoryWithDefault model.wallet
    in
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
            [ tradeTypeElement factory model
            , daiElement factory model
            , fiatElement model
            ]
        , feeNotifyElement model
        ]


tradeTypeElement : FactoryType -> Model -> Element Msg
tradeTypeElement factory model =
    EH.withHeader
        "Trade Type"
        (roleToggleElement
            (tokenUnitName factory)
            model.inputs.userRole
        )


roleToggleElement : String -> BuyerOrSeller -> Element Msg
roleToggleElement tokenName userRole =
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
            (Element.text <| "Sell " ++ tokenName)
        , Element.el
            ([ Element.Events.onClick <| ChangeRole Buyer ] ++ buyDaiStyles)
            (Element.text <| "Buy " ++ tokenName)
        ]


daiElement : FactoryType -> Model -> Element Msg
daiElement factory model =
    EH.niceBottomBorderEl <|
        EH.withHeader
            (case model.inputs.userRole of
                Buyer ->
                    "You're buying"

                Seller ->
                    "You're selling"
            )
            (daiInputElement
                factory
                model.inputs.daiAmount
                model.errors.daiAmount
            )


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
        ( Nothing
        , Just <|
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "click" "misclick" "dai symbol in dai input" 0
                ]
                (EH.daiSymbolAndLabel factoryType)
        )
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
                model.errors.fiatAmount
                model.errors.fiatType
            )


fiatInputElement : Prices.Symbol -> String -> Bool -> Maybe String -> Maybe String -> Element Msg
fiatInputElement symbol amountString showFiatTypeDropdown maybeAmountError maybeTypeError =
    let
        fiatCharElement =
            case Prices.char symbol of
                Just char ->
                    Element.el
                        [ Element.Events.onClick <|
                            AppCmd <|
                                AppCmd.gTag "click" "misclick" "currency symbol" 0
                        ]
                        (Element.text <| char)

                Nothing ->
                    Element.none

        flagClickedMsg =
            AppCmd <| AppCmd.gTag "click" "misclick" "currency flag" 0

        currencySelector =
            Element.el
                [ Element.below <|
                    EH.maybeErrorElement
                        [ inputErrorTag
                        , Element.moveDown 5
                        ]
                        maybeTypeError
                ]
                (EH.currencySelector showFiatTypeDropdown symbol (ShowCurrencyDropdown True) FiatTypeChanged flagClickedMsg)
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
                maybeAmountError
        ]
        ( Just fiatCharElement, Just currencySelector )
        "fiat input"
        Nothing
        amountString
        FiatAmountChanged


openButtonElement : Wallet.State -> Element Msg
openButtonElement wallet =
    Element.el [ Element.centerX ] <|
        case ( Wallet.userInfo wallet, Wallet.factory wallet ) of
            ( Just userInfo, Just factory ) ->
                EH.redButton "Open Trade" (CreateClicked factory userInfo)

            ( Nothing, _ ) ->
                EH.redButton "Connect to Wallet" Web3Connect

            ( _, Nothing ) ->
                EH.disabledButton "Unsupported Network" Nothing


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
                        ++ tokenUnitName (Wallet.factoryWithDefault model.wallet)
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
        , Element.Events.onClick <|
            AppCmd <|
                AppCmd.gTag "click" "misclick" "fee notify element" 0
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
        , Element.spacing 20
        ]
        [ openPhaseElement
            model.inputs.autorecallInterval
            model.errors.autorecallInterval
            model.inputs.userRole
        , committedPhaseElement
            model.inputs.paymentMethod
            model.errors.paymentMethod
            model.inputs.autoabortInterval
            model.errors.autoabortInterval
            model.inputs.userRole
        , judgmentPhaseElement
            model.inputs.autoreleaseInterval
            model.errors.autoreleaseInterval
            model.inputs.userRole
        ]


openPhaseElement : Time.Posix -> Maybe String -> BuyerOrSeller -> Element Msg
openPhaseElement interval maybeError userRole =
    Element.el
        [ Element.Border.rounded 8
        , Element.Background.color EH.white
        , Element.clipX
        , Element.clipY
        ]
    <|
        phaseElement
            Images.openWindowIcon
            "Open Window"
            (openWindowSummary userRole)
            interval
            maybeError
            AutorecallIntervalChanged


committedPhaseElement : String -> Maybe String -> Time.Posix -> Maybe String -> BuyerOrSeller -> Element Msg
committedPhaseElement paymentMethodText maybeTextError interval maybeIntervalError userRole =
    Element.column
        [ Element.spacing 15
        , Element.Border.rounded 8
        , Element.clipX
        , Element.clipY
        , Element.Background.color EH.white
        ]
        [ phaseElement
            Images.fiatBag
            "Payment Window"
            (paymentWindowSummary userRole)
            interval
            maybeIntervalError
            AutoabortIntervalChanged
        , paymentMethodsElement maybeTextError userRole paymentMethodText
        ]


judgmentPhaseElement : Time.Posix -> Maybe String -> BuyerOrSeller -> Element Msg
judgmentPhaseElement interval maybeError userRole =
    Element.el
        [ Element.Border.rounded 8
        , Element.clipX
        , Element.clipY
        , Element.Background.color EH.white
        ]
    <|
        phaseElement
            Images.releaseWindowIcon
            "Burn/Release Window"
            (releaseWindowSummary userRole)
            interval
            maybeError
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
        ++ " does not commit to the trade, returning the balance and the 1% fee to your wallet. This can also be manually triggered anytime before a "
        ++ committingParty
        ++ " commits."


paymentWindowSummary : BuyerOrSeller -> String
paymentWindowSummary userRole =
    case userRole of
        Buyer ->
            "After committing, you and the Seller have this long to complete the fiat payment, using one of your payment methods indicated below. If you fail to confirm payment within this window, 1/4 of your deposit is burned from both parties and the rest is refunded."

        Seller ->
            "After committing, you and the Buyer have this long to complete the fiat payment, using one of your payment methods indicated below. If the Buyer aborts or fails to confirm within this window, 1/12 of the trade amount is burned from both parties and the rest is refunded."


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
                , Element.Background.color <| Element.rgba255 155 203 255 0.2
                , Element.Border.width 0
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
        [ Element.spacing 20
        , Element.paddingEach
            { left = 45
            , right = 45
            , top = 0
            , bottom = 15
            }
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


phaseElement : Image -> String -> String -> Time.Posix -> Maybe String -> (Time.Posix -> Msg) -> Element Msg
phaseElement icon title summary interval maybeError newIntervalMsg =
    let
        iconAndTitleElement =
            Element.row
                [ Element.spacing 30 ]
                [ Images.toElement
                    [ Element.height <| Element.px 40
                    , Element.Events.onClick <|
                        AppCmd <|
                            AppCmd.gTag "click" "misclick" ("symbol for " ++ title) 0
                    ]
                    icon
                , Element.el
                    [ Element.Font.size 22
                    , Element.Font.semiBold
                    ]
                    (Element.text title)
                ]

        descriptionElement =
            Element.paragraph
                [ Element.Font.size 17
                , Element.Font.medium
                , Element.Font.color EH.permanentTextColor
                ]
                [ Element.text summary ]

        intervalElement =
            Element.el
                [ Element.Background.color <| Element.rgba255 155 203 255 0.2
                , Element.Border.rounded 5
                , Element.padding 15
                , Element.above <|
                    EH.maybeErrorElement
                        [ inputErrorTag ]
                        maybeError
                ]
                (EH.intervalInput EH.black interval newIntervalMsg)
    in
    Element.column
        [ Element.width Element.fill
        , Element.Border.rounded 10
        , Element.Background.color <| Element.rgb255 237 237 237
        , Element.spacing 2
        ]
        (List.map
            (Element.el
                [ Element.Background.color EH.white
                , Element.paddingXY 45 18
                , Element.width Element.fill
                ]
            )
            [ iconAndTitleElement
            , Element.row
                [ Element.width Element.fill
                , Element.spacing 25
                , Element.Background.color EH.white
                ]
                [ intervalElement
                , descriptionElement
                ]
            ]
        )


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
        Confirm factoryType createParameters ->
            let
                ( depositAmountEl, confirmButton ) =
                    case model.depositAmount of
                        Just depositAmount ->
                            ( TokenValue.tokenValue depositAmount
                                |> TokenValue.toConciseString
                                |> Element.text
                            , EH.redButton "Yes, I definitely want to open this trade." (ConfirmCreate factoryType createParameters depositAmount)
                            )

                        Nothing ->
                            ( Element.text "??"
                            , EH.disabledButton "(loading exact fees...)" Nothing
                            )
            in
            EH.closeableModal
                []
                (Element.column
                    [ Element.spacing 20
                    , Element.padding 20
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
                                     , Element.text <| " " ++ tokenUnitName factoryType ++ " (this includes the 1% dev fee) to open this trade."
                                     ]
                                   ]
                                ++ (case factoryType of
                                        Token _ ->
                                            [ [ Element.text <| "This ususally requires two Metamask signatures. Your " ++ tokenUnitName factoryType ++ " will not be deposited until the final transaction has been mined." ] ]

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

        ApproveNeedsSig tokenType ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "ApproveNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the approve call."
                    , Element.text "(check Metamask!)"
                    , Element.text "Note that there will be a second transaction to sign after this."
                    ]

        ApproveMining tokenType createParameters txHash ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "ApproveMining" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Mining the initial approve transaction..."
                    , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                        { url = EthHelpers.makeViewTxUrl (Token tokenType) txHash
                        , label = Element.text "See the transaction on Etherscan"
                        }
                    , Element.text "Funds will not leave your wallet until you sign the next transaction."
                    ]

        CreateNeedsSig _ ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "CreateNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the create call."
                    , Element.text "(check Metamask!)"
                    ]

        CreateMining factoryType txHash ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "CreateMining" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Mining the final create call..."
                    , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                        { url = EthHelpers.makeViewTxUrl factoryType txHash
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
                    "That Burn/Release Window time is quite small! It might take a while to find a committed Seller."

                Seller ->
                    "That Burn/Release Window time is quite small! This may attract scammers. Only create this trade if you know what you're doing."

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
        [ Element.Font.color <| Element.rgba 0 0 0 0.2 ]
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
