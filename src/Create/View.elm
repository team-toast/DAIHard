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


root : Model -> Element Msg
root model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 30
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
    Element.none


fiatElement : Model -> Element Msg
fiatElement model =
    Element.none


marginElement : Model -> Element Msg
marginElement model =
    Element.none


buttonsElement : Model -> Element Msg
buttonsElement model =
    Element.none


phasesElement : Model -> Element Msg
phasesElement model =
    Element.none


paymentMethodsElement : Model -> Element Msg
paymentMethodsElement model =
    Element.none
