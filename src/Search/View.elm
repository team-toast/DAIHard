module Search.View exposing (root)

import CommonTypes exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Search.Types exposing (..)
import Time


root : Time.Posix -> Model -> Element Msg
root time model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ searchInputElement model.inputs
        , EH.hbreak
        , resultsElement model
        ]


searchInputElement : SearchInputs -> Element Msg
searchInputElement inputs =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 100
        , Element.spacing 10
        , Element.padding 20
        ]
        [ Element.el [ Element.width <| Element.fillPortion 3 ] <|
            daiRangeInput inputs.daiRange
        , Element.el [ Element.width <| Element.fillPortion 2 ] <|
            fiatTypeInput inputs.fiatType
        , Element.el [ Element.width <| Element.fillPortion 3 ] <|
            fiatRangeInput inputs.fiatRange
        , Element.el [ Element.width <| Element.fillPortion 6 ] <|
            paymentMethodsInput inputs.paymentMethod
        , Element.el [ Element.width <| Element.fillPortion 6 ] <|
            locationInput inputs.location
        , Element.el [] <|
            resetButton
        ]


daiRangeInput : AmountRange -> Element Msg
daiRangeInput range =
    dummyTextInput
        |> withInputHeader "Dai Amount"


fiatTypeInput : Maybe FiatType -> Element Msg
fiatTypeInput fiatType =
    dummyTextInput
        |> withInputHeader "Fiat Type"


fiatRangeInput : AmountRange -> Element Msg
fiatRangeInput range =
    dummyTextInput
        |> withInputHeader "Fiat Amount"


paymentMethodsInput : Maybe PaymentMethodQuery -> Element Msg
paymentMethodsInput pmQuery =
    dummyTextInput
        |> withInputHeader "Payment Methods"


locationInput : Maybe LocationQuery -> Element Msg
locationInput lQuery =
    dummyTextInput
        |> withInputHeader "Location"


dummyTextInput =
    Element.Input.text [ Element.width Element.fill, Element.height <| Element.px 40 ]
        { onChange = \_ -> NoOp
        , text = ""
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }


resetButton : Element Msg
resetButton =
    Element.Input.button
        [ Element.Background.color EH.buttonDeepBlue
        , Element.padding 10
        , Element.Border.rounded 5
        ]
        { onPress = Just NoOp
        , label =
            Element.el
                [ Element.Font.color EH.white
                , Element.centerX
                , Element.centerY
                ]
                (Element.text "Reset")
        }
        |> withInputHeader " "


withInputHeader : String -> Element Msg -> Element Msg
withInputHeader title element =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 8
        ]
        [ Element.el [ Element.Font.size 16 ] <| Element.text title
        , element
        ]


resultsElement : Model -> Element Msg
resultsElement model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        Element.none
