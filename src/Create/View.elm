module Create.View exposing (root)

import Contracts.Types as CTypes
import Create.PMWizard.View as PMWizard
import Create.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue
import Images exposing (Image)
import List.Extra
import PaymentMethods exposing (PaymentMethod)
import Time


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
        , phasesElement model
        , paymentMethodsHeaderElement model
        , paymentMethodsElement model
        ]


mainInputElement : Model -> Element Msg
mainInputElement model =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color EH.white
        , Element.Border.rounded 5
        , Element.padding 20
        , Element.spaceEvenly
        , EH.subtleShadow
        ]
        [ tradeTypeElement model
        , daiElement model
        , fiatElement model
        , marginElement model
        , buttonsElement model
        ]


tradeTypeElement : Model -> Element Msg
tradeTypeElement model =
    EH.inputWithHeader
        "Trade Type"
        (typeToggleElement model.inputs.openMode)


typeToggleElement : CTypes.OpenMode -> Element Msg
typeToggleElement openMode =
    let
        baseStyles =
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.pointer
            ]

        ( buyDaiStyles, sellDaiStyles ) =
            case openMode of
                CTypes.BuyerOpened ->
                    ( baseStyles
                    , baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    )

                CTypes.SellerOpened ->
                    ( baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    , baseStyles
                    )
    in
    Element.row [ Element.spacing 20 ]
        [ Element.el
            ([ Element.Events.onClick <| ChangeType CTypes.SellerOpened ] ++ sellDaiStyles)
            (Element.text "Sell DAI")
        , Element.el
            ([ Element.Events.onClick <| ChangeType CTypes.BuyerOpened ] ++ buyDaiStyles)
            (Element.text "Buy DAI")
        ]


daiElement : Model -> Element Msg
daiElement model =
    EH.niceBottomBorderEl <|
        EH.inputWithHeader
            (case model.inputs.openMode of
                CTypes.BuyerOpened ->
                    "You're buying"

                CTypes.SellerOpened ->
                    "You're selling"
            )
            (daiInputElement model.inputs.daiAmount)


daiInputElement : String -> Element Msg
daiInputElement amountString =
    EH.fancyInput
        [ Element.width <| Element.px 250
        , Element.Font.medium
        , Element.Font.size 24
        ]
        ( Nothing, Just <| EH.daiSymbolAndLabel )
        "dai input"
        Nothing
        amountString
        TradeAmountChanged


fiatElement : Model -> Element Msg
fiatElement model =
    EH.niceBottomBorderEl <|
        EH.inputWithHeader
            "For fiat"
            (fiatInputElement model.inputs.fiatType model.inputs.fiatAmount model.showFiatTypeDropdown)


fiatInputElement : String -> String -> Bool -> Element Msg
fiatInputElement typeString amountString showFiatTypeDropdown =
    let
        fiatCharElement =
            Element.text <| FiatValue.typeStringToCharStringDefaultEmpty typeString
    in
    EH.fancyInput
        [ Element.width <| Element.px 250
        , Element.Font.medium
        , Element.Font.size 24
        ]
        ( Just fiatCharElement, Just <| EH.currencySelector showFiatTypeDropdown typeString OpenCurrencySelector FiatTypeChanged ShowCurrencyDropdown FiatTypeArrowClicked )
        "fiat input"
        Nothing
        amountString
        FiatAmountChanged


marginElement : Model -> Element Msg
marginElement model =
    EH.niceBottomBorderEl <|
        EH.inputWithHeader
            "At margin"
            (marginInputElement model.inputs.margin (model.inputs.openMode == CTypes.SellerOpened))


marginInputElement : String -> Bool -> Element Msg
marginInputElement marginString upIsGreen =
    let
        ( color, arrowImage ) =
            case interpretMarginString marginString of
                Just margin ->
                    if margin == 0 then
                        ( EH.black, Images.none )

                    else if xor (margin > 0) upIsGreen then
                        ( EH.red, Images.marginSymbol (margin > 0) False )

                    else
                        ( EH.green, Images.marginSymbol (margin > 0) True )

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
        , EH.redButton "Publish Offer" BeginCreateProcess
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
                model.inputs.openMode
        , Element.el [ Element.width <| Element.fillPortion 1 ] <|
            committedPhaseElement
                model.inputs.autoabortInterval
                model.inputs.openMode
        , Element.el [ Element.width <| Element.fillPortion 1 ] <|
            claimedPhaseElement
                model.inputs.autoreleaseInterval
                model.inputs.openMode
        ]


openPhaseElement : Time.Posix -> CTypes.OpenMode -> Element Msg
openPhaseElement interval openMode =
    phaseElement
        Images.openWindowIcon
        "Open Window"
        (openWindowSummary openMode)
        interval
        Nothing
        AutorecallIntervalChanged


committedPhaseElement : Time.Posix -> CTypes.OpenMode -> Element Msg
committedPhaseElement interval openMode =
    phaseElement
        Images.paymentWindowIcon
        "Payment Window"
        (paymentWindowSummary openMode)
        interval
        (Just EH.red)
        AutoabortIntervalChanged


claimedPhaseElement : Time.Posix -> CTypes.OpenMode -> Element Msg
claimedPhaseElement interval openMode =
    phaseElement
        Images.releaseWindowIcon
        "Release Window"
        (releaseWindowSummary openMode)
        interval
        (Just EH.red)
        AutoreleaseIntervalChanged


openWindowSummary : CTypes.OpenMode -> String
openWindowSummary openMode =
    let
        committingParty =
            case openMode of
                CTypes.BuyerOpened ->
                    "seller"

                CTypes.SellerOpened ->
                    "buyer"
    in
    "The offer will expire by this time window if a "
        ++ committingParty
        ++ " does not commit to the trade. You can also remove the trade before this window runs out."


paymentWindowSummary : CTypes.OpenMode -> String
paymentWindowSummary openMode =
    case openMode of
        CTypes.BuyerOpened ->
            "You have this time window to send the fiat funds (<- replace me!) to the seller using one of your payment methods indicated below."

        CTypes.SellerOpened ->
            "The buyer has this time window to send the fiat funds (<- replace me!) to you using one of your payment methods indicated below."


releaseWindowSummary : CTypes.OpenMode -> String
releaseWindowSummary openMode =
    case openMode of
        CTypes.BuyerOpened ->
            "Once you confirm payment, the seller has this time window to decide whether to release the funds to you or burn everything."

        CTypes.SellerOpened ->
            "Once the buyer confirms payment, you have this time window to decide whether to release the funds to the buyer or burn everything."


phaseElement : Image -> String -> String -> Time.Posix -> Maybe Element.Color -> (Time.Posix -> Msg) -> Element Msg
phaseElement icon title summary interval lowIntervalColor newIntervalMsg =
    let
        descriptionElement =
            Element.column
                [ Element.spacing 15
                , Element.width Element.fill
                , Element.height <| Element.px 160
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


paymentMethodsHeaderElement : Model -> Element Msg
paymentMethodsHeaderElement model =
    let
        titleStr =
            "Accepted Payment Methods ("
                ++ (model.inputs.paymentMethods |> List.length |> String.fromInt)
                ++ ")"

        addMethodButton =
            Images.toElement
                [ Element.pointer
                , Element.Events.onClick OpenPMWizard
                ]
                Images.addButton
    in
    Element.row
        [ Element.spacing 8
        , Element.paddingXY 40 0
        , Element.Font.size 23
        , Element.Font.semiBold
        ]
        [ Element.text titleStr
        , addMethodButton
        ]


paymentMethodsElement : Model -> Element Msg
paymentMethodsElement model =
    Element.el
        [ Element.paddingXY 40 0
        , Element.width Element.fill
        ]
        (model.inputs.paymentMethods
            |> List.map pmElement
            |> doubleColumn
        )


pmElement : PaymentMethod -> Element Msg
pmElement pm =
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.shrink |> Element.maximum 300)
        , Element.spacing 1
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height Element.shrink
            , Element.paddingXY 60 40
            , EH.roundTopCorners 8
            , Element.Background.color EH.white
            , Element.Font.size 22
            , Element.Font.semiBold
            ]
            (Element.text <| PaymentMethods.getTitle pm.type_)
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.shrink
            , Element.paddingXY 60 40
            , EH.roundBottomCorners 8
            , Element.Background.color EH.white
            ]
            (Element.paragraph
                [ Element.Font.size 17
                , Element.Font.medium
                ]
                [ Element.text pm.info ]
            )
        ]


doubleColumn : List (Element Msg) -> Element Msg
doubleColumn elList =
    elList
        |> List.Extra.greedyGroupsOf 2
        |> List.map
            (\row ->
                if List.length row == 1 then
                    List.append row [ Element.none ]

                else
                    row
            )
        |> List.map
            (\row ->
                Element.row
                    [ Element.spacing 30
                    , Element.width Element.fill
                    ]
                    (row
                        |> List.map (Element.el [ Element.width Element.fill ])
                    )
            )
        |> Element.column
            [ Element.spacing 30
            , Element.width Element.fill
            ]


getModalOrNone : Model -> Element Msg
getModalOrNone model =
    case model.addPMModal of
        Nothing ->
            Element.none

        Just pmModal ->
            EH.modal <|
                Element.map
                    PMWizardMsg
                    (PMWizard.root pmModal model.inputs.openMode)
