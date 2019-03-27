module Create.View exposing (root)

import Contracts.Types as CTypes
import Create.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue
import Images


root : Model -> Element Msg
root model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.Events.onClick <| ShowCurrencyDropdown False
        ]
        [ mainInputElement model
        , phasesElement model
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
    Element.none


phasesElement : Model -> Element Msg
phasesElement model =
    Element.none


paymentMethodsElement : Model -> Element Msg
paymentMethodsElement model =
    Element.none
