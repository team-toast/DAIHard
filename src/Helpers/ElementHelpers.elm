module ElementHelpers exposing (black, blue, bulletPointString, currencySelector, daiSymbol, daiSymbolAndLabel, daiValue, disabledTextColor, errorMessage, fakeLink, fancyInput, fiatTypeToSymbolElement, fiatValue, green, headerBackgroundColor, inputWithHeader, interval, intervalWithElapsedBar, lightGray, margin, pageBackgroundColor, red, roundBottomCorners, roundTopCorners, subtleShadow, testBorderStyles, textInputWithElement, tokenValue, white, yellow)

import CommonTypes exposing (..)
import Css
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FiatValue exposing (FiatValue)
import Html.Events
import Html.Styled
import Images
import Json.Decode
import List
import Maybe.Extra
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)



-- COLORS


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


red =
    Element.rgb255 244 0 103


green =
    Element.rgb255 51 183 2


blue =
    Element.rgb 0 0 1


yellow =
    Element.rgb 1 1 0


lightGray =
    Element.rgb255 233 237 242


pageBackgroundColor =
    Element.rgb 0.9 0.9 0.9


headerBackgroundColor =
    Element.rgb255 10 33 108


disabledTextColor =
    Element.rgba255 1 31 52 0.13



-- LINKS


fakeLink : String -> Element msg
fakeLink name =
    Element.link
        [ Element.Font.color (Element.rgb 0 0 1)
        , Element.Font.underline
        ]
        { url = "#"
        , label = Element.text name
        }



-- RENDERERS


tokenValue : TokenValue -> Element msg
tokenValue tv =
    let
        s =
            TokenValue.renderToString Nothing tv ++ " DAI"
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)


daiValue : TokenValue -> Element msg
daiValue tv =
    let
        numStr =
            TokenValue.toConciseString tv
    in
    Element.row [ Element.spacing 4 ]
        [ daiSymbol []
        , Element.el [ Element.Font.size 16 ] <| Element.text numStr
        ]


fiatValue : FiatValue -> Element msg
fiatValue fv =
    let
        currencyElement =
            case Dict.get fv.fiatType FiatValue.currencyTypes of
                Nothing ->
                    Element.el [ Element.Font.color red ] (Element.text "!")

                Just ( typeChar, image ) ->
                    Element.image
                        [ Element.height <| Element.px 26 ]
                        image
    in
    Element.row [ Element.spacing 4 ]
        [ currencyElement
        , Element.el
            [ Element.Font.color <| Element.rgba 0 0 0 0.5
            , Element.Font.medium
            , Element.width <| Element.px 50
            , Element.clip
            ]
            (Element.text fv.fiatType)
        , Element.text <| FiatValue.renderToString fv
        ]


margin : Bool -> Float -> Element msg
margin upIsGreen marginFloat =
    case marginFloatToConciseUnsignedString marginFloat of
        "0%" ->
            Element.el [ Element.Font.size 16 ] (Element.text "0%")

        unsignedPercentString ->
            let
                isUp =
                    marginFloat >= 0

                isGreen =
                    not <| xor isUp upIsGreen

                textColor =
                    if isGreen then
                        green

                    else
                        red
            in
            Element.row [ Element.spacing 4 ]
                [ marginSymbol [] isUp isGreen
                , Element.el [ Element.Font.color textColor, Element.Font.size 16 ]
                    (Element.text unsignedPercentString)
                ]


interval : Maybe Element.Color -> Time.Posix -> Element msg
interval maybeLowValColor i =
    case TimeHelpers.toHumanReadableInterval i of
        Nothing ->
            errorMessage "Interval display failed! Is it too big?" i

        Just hrInterval ->
            let
                lowValColor =
                    maybeLowValColor
                        |> Maybe.withDefault black

                hc =
                    if hrInterval.days == 0 then
                        lightGray

                    else
                        black

                ( mc, sc ) =
                    if hrInterval.days == 0 && hrInterval.hours == 0 then
                        ( lightGray, lowValColor )

                    else
                        ( black, black )
            in
            Element.row [ Element.spacing 5 ]
                [ timeUnitElement hrInterval.days 'd' hc
                , timeUnitElement hrInterval.hours 'h' mc
                , timeUnitElement hrInterval.min 'm' sc
                ]


timeUnitElement : Int -> Char -> Element.Color -> Element msg
timeUnitElement num unitChar color =
    let
        numStr =
            String.fromInt num
                |> String.padLeft 2 '0'
    in
    Element.el [ Element.Font.size 16, Element.Font.color color, Element.Font.medium ]
        (Element.text <| numStr ++ String.fromChar unitChar)


intervalWithElapsedBar : Time.Posix -> Time.Posix -> Element.Length -> Element msg
intervalWithElapsedBar i total width =
    let
        color =
            let
                seconds =
                    TimeHelpers.posixToSeconds i
            in
            if seconds < 60 * 60 then
                red

            else if seconds < 60 * 60 * 24 then
                yellow

            else
                green

        ratio =
            TimeHelpers.getRatio
                (TimeHelpers.sub total i)
                total
    in
    Element.column [ Element.spacing 5, Element.width width ]
        [ Element.el [ Element.centerX ] (interval Nothing i)
        , elapsedBar ratio color
        ]


elapsedBar : Float -> Element.Color -> Element msg
elapsedBar ratio filledBarColor =
    let
        barStyles =
            [ Element.height <| Element.px 3
            , Element.Border.rounded 20
            ]

        filledFillPortion =
            round (ratio * 200.0)

        unfilledFillPortion =
            200 - filledFillPortion

        backgroundBarEl =
            Element.el
                (barStyles ++ [ Element.width Element.fill, Element.Background.color lightGray ])
                Element.none

        filledBarEl =
            Element.el
                (barStyles ++ [ Element.width <| Element.fillPortion filledFillPortion, Element.Background.color filledBarColor ])
                Element.none

        spacerEl =
            Element.el
                [ Element.width <| Element.fillPortion unfilledFillPortion ]
                Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.behindContent backgroundBarEl
        ]
        [ filledBarEl
        , spacerEl
        ]



-- INPUTS


fancyInput : List (Attribute msg) -> ( Maybe (Element msg), Maybe (Element msg) ) -> String -> Maybe (Element.Input.Placeholder msg) -> String -> (String -> msg) -> Element msg
fancyInput attributes ( maybeLeftElement, maybeRightElement ) labelStr placeholder value msgConstructor =
    let
        inputElement =
            Element.Input.text
                [ Element.width Element.fill
                , Element.height <| Element.px 40
                , Element.Border.width 0
                ]
                { onChange = msgConstructor
                , text = value
                , placeholder = placeholder
                , label = Element.Input.labelHidden labelStr
                }
    in
    Element.row
        ([ Element.spacing 5
         , Element.Border.color lightGray
         , Element.Border.widthEach
            { bottom = 1
            , top = 0
            , right = 0
            , left = 0
            }
         ]
            ++ attributes
        )
        ([ Maybe.map
            (Element.el [ Element.alignLeft ])
            maybeLeftElement
         , Just inputElement
         , Maybe.map
            (Element.el [ Element.alignRight ])
            maybeRightElement
         ]
            |> Maybe.Extra.values
        )


textInputWithElement : List (Attribute msg) -> List (Attribute msg) -> Element msg -> String -> String -> Maybe (Element.Input.Placeholder msg) -> Maybe (Bool -> msg) -> (String -> msg) -> Element msg
textInputWithElement attributes inputAttributes addedElement labelStr value placeholder maybeShowHideDropdownMsg msgConstructor =
    let
        focusEventAttributes =
            case maybeShowHideDropdownMsg of
                Nothing ->
                    []

                Just showHideMsgConstructor ->
                    [ Element.Events.onFocus <| showHideMsgConstructor True
                    , onClickNoPropagation <| showHideMsgConstructor True
                    ]
    in
    Element.row
        (attributes
            ++ [ Element.width Element.fill
               , Element.height <| Element.px 40
               , Element.Border.shadow
                    { offset = ( 0, 3 )
                    , size = 0
                    , blur = 20
                    , color = Element.rgba255 233 237 242 0.05
                    }
               ]
        )
        [ Element.el
            [ Element.Background.color lightGray
            , Element.height <| Element.px 40
            , Element.Border.roundEach
                { topLeft = 4
                , bottomLeft = 4
                , topRight = 0
                , bottomRight = 0
                }
            ]
            (Element.el [ Element.centerY, Element.paddingXY 10 0 ] addedElement)
        , Element.Input.text
            (focusEventAttributes
                ++ inputAttributes
                ++ [ Element.width Element.fill
                   , Element.height <| Element.px 40
                   , Element.Border.color lightGray
                   , Element.Border.roundEach
                        { topLeft = 0
                        , bottomLeft = 0
                        , topRight = 4
                        , bottomRight = 4
                        }
                   , Element.Border.widthEach
                        { top = 1
                        , bottom = 1
                        , right = 1
                        , left = 0
                        }
                   ]
            )
            { onChange = msgConstructor
            , text = value
            , placeholder = placeholder
            , label = Element.Input.labelHidden labelStr
            }
        ]


currencySelector : Bool -> String -> (Bool -> msg) -> (String -> msg) -> Element msg
currencySelector showDropdown typeInput showHideMsgConstructor msgConstructor =
    let
        gotCurrency =
            Dict.get typeInput FiatValue.currencyTypes

        dropdownEl =
            case ( showDropdown, gotCurrency ) of
                ( False, _ ) ->
                    Element.none

                ( True, Just _ ) ->
                    Element.none

                ( True, Nothing ) ->
                    Element.column
                        [ Element.width <| Element.fill
                        , Element.Border.color black
                        , Element.Border.width 1
                        , Element.Background.color white
                        , Element.padding 5
                        , Element.width Element.fill
                        ]
                        (FiatValue.searchTypes typeInput
                            |> Dict.toList
                            |> List.map
                                (\( typeString, ( _, image ) ) ->
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing 9
                                        , Element.paddingXY 0 5
                                        , onClickNoPropagation <| msgConstructor typeString
                                        , Element.mouseOver [ Element.Background.color <| Element.rgb 0.8 0.8 1 ]
                                        ]
                                        [ Element.image [ Element.height <| Element.px 26 ] image
                                        , Element.el [ Element.Font.size 16, Element.Font.semiBold ] <| textWithoutTextCursor typeString
                                        ]
                                )
                        )
    in
    textInputWithElement
        [ Element.below dropdownEl ]
        []
        (fiatTypeToSymbolElement typeInput)
        "select currency"
        typeInput
        Nothing
        (Just showHideMsgConstructor)
        (String.toUpper >> msgConstructor)


textWithoutTextCursor : String -> Element msg
textWithoutTextCursor s =
    Html.Styled.styled
        Html.Styled.span
        [ Css.hover [ Css.cursor Css.default ] ]
        []
        [ Html.Styled.text s ]
        |> Html.Styled.toUnstyled
        |> Element.html


fiatTypeToSymbolElement : String -> Element msg
fiatTypeToSymbolElement fiatType =
    case Dict.get fiatType FiatValue.currencyTypes of
        Nothing ->
            Element.text "*"

        Just ( _, image ) ->
            Element.image [ Element.height <| Element.px 26 ] image


onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg =
    Html.Events.stopPropagationOn
        "click"
        (Json.Decode.succeed ( msg, True ))
        |> Element.htmlAttribute



-- STYLE HELPERS


roundBottomCorners : Int -> Attribute msg
roundBottomCorners r =
    Element.Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = r
        , bottomRight = r
        }


roundTopCorners : Int -> Attribute msg
roundTopCorners r =
    Element.Border.roundEach
        { topLeft = r
        , topRight = r
        , bottomLeft = 0
        , bottomRight = 0
        }


inputWithHeader : String -> Element msg -> Element msg
inputWithHeader headerString element =
    Element.column [ Element.spacing 10 ]
        [ Element.el
            [ Element.Font.size 17
            , Element.Font.semiBold
            ]
            (Element.text headerString)
        , element
        ]


subtleShadow : Attribute msg
subtleShadow =
    Element.Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 20
        , color = Element.rgba255 0 0 0 0.04
        }



-- SPECIAL CHARS


bulletPointString : String
bulletPointString =
    Char.fromCode 8226
        |> String.fromChar



-- IMAGES


daiSymbol : List (Attribute msg) -> Element msg
daiSymbol attributes =
    Element.image
        ((Element.height <| Element.px 26) :: attributes)
        Images.daiSymbol


daiSymbolAndLabel : Element msg
daiSymbolAndLabel =
    Element.row
        [ Element.spacing 5 ]
        [ daiSymbol []
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.Font.color <| Element.rgb255 109 127 138
            ]
            (Element.text "DAI")
        ]


marginSymbol : List (Attribute msg) -> Bool -> Bool -> Element msg
marginSymbol attributes isUp isGreen =
    Element.image
        ((Element.height <| Element.px 34) :: attributes)
        (Images.marginSymbol isUp isGreen)



-- DEBUG


testBorderStyles : List (Attribute msg)
testBorderStyles =
    [ Element.Border.width 1
    , Element.Border.color (Element.rgb 1 0 1)
    ]


errorMessage : String -> a -> Element msg
errorMessage str debugObj =
    let
        _ =
            Debug.log str debugObj
    in
    Element.el
        [ Element.padding 1
        , Element.Background.color <| Element.rgb 1 0 0
        , Element.Font.color white
        ]
        (Element.text <| "Error:" ++ str)



-- ETC


marginFloatToConciseUnsignedString : Float -> String
marginFloatToConciseUnsignedString f =
    let
        absPercentNumber =
            abs <| f * 100.0

        preDecimalString =
            floor absPercentNumber
                |> String.fromInt

        decimalRemainder =
            absPercentNumber - toFloat (floor absPercentNumber)

        extraDigitsNeeded =
            max 0 (3 - String.length preDecimalString)

        decimalString =
            case extraDigitsNeeded of
                0 ->
                    ""

                n ->
                    String.fromFloat decimalRemainder
                        |> String.dropLeft 1
                        |> String.left extraDigitsNeeded
    in
    preDecimalString ++ decimalString ++ "%"
