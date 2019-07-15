module Helpers.Element exposing (activePhaseBackgroundColor, bigTimeUnitElement, black, blue, blueButton, bulletPointString, button, closeButton, closeableModal, coloredMargin, comingSoonMsg, coolCurrencyHbreak, currencyLabelColor, currencySelector, daiSymbol, daiSymbolAndLabel, daiValue, daiYellow, darkGray, darkYellow, disabledButton, disabledTextColor, dollarGreen, elOnCircle, elapsedBar, elementColorToAvh4Color, ethAddress, etherscanAddressLink, fakeLink, fancyInput, fiatTypeToSymbolElement, fiatValue, green, headerBackgroundColor, interval, intervalInput, intervalWithElapsedBar, inverseBlueButton, lightBlue, lightGray, marginFloatToConciseUnsignedString, marginSymbol, maybeErrorElement, mediumGray, modal, niceBottomBorderEl, niceFloatingRow, onClickNoPropagation, orangeButton, pageBackgroundColor, permanentTextColor, pokeButton, red, redButton, roundBottomCorners, roundTopCorners, subtleShadow, testBorderStyles, textInputWithElement, textWithoutTextCursor, txProcessModal, uncoloredMargin, white, withHeader, yellow)

import Browser.Dom
import Collage exposing (Collage)
import Collage.Render
import Color exposing (Color)
import CommonTypes exposing (..)
import Config
import Css
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import FiatValue exposing (FiatValue)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Html.Attributes
import Html.Events
import Html.Styled
import Images
import Json.Decode
import List
import List.Extra
import Maybe.Extra
import Task
import Time
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


lightBlue =
    Element.rgba 0 0 1 0.2


yellow =
    Element.rgb 1 1 0


daiYellow =
    yellow


dollarGreen =
    green


darkYellow =
    Element.rgb 0.6 0.6 0


lightGray =
    Element.rgb255 233 237 242


mediumGray =
    Element.rgb255 200 205 210


darkGray =
    Element.rgb255 150 150 150


activePhaseBackgroundColor =
    Element.rgb255 9 32 107


permanentTextColor =
    Element.rgba255 1 31 52 0.64


pageBackgroundColor =
    Element.rgb 0.9 0.9 0.9


headerBackgroundColor =
    Element.rgb255 10 33 108


disabledTextColor =
    Element.rgba255 1 31 52 0.13


currencyLabelColor =
    Element.rgb255 109 127 138



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



-- VALUE RENDERERS


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
                    Images.toElement
                        [ Element.height <| Element.px 26 ]
                        image
    in
    Element.row [ Element.spacing 4 ]
        [ currencyElement
        , Element.el
            [ Element.Font.color <| Element.rgba 0 0 0 0.5
            , Element.Font.medium
            , Element.width <| Element.px 50
            ]
            (Element.text fv.fiatType)
        , Element.text <| FiatValue.renderToString fv
        ]


coloredMargin : Bool -> Float -> Element msg
coloredMargin upIsGreen marginFloat =
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
                [ marginSymbol [] isUp (Just isGreen)
                , Element.el [ Element.Font.color textColor, Element.Font.size 16 ]
                    (Element.text unsignedPercentString)
                ]


uncoloredMargin : Float -> Element msg
uncoloredMargin marginFloat =
    case marginFloatToConciseUnsignedString marginFloat of
        "0%" ->
            Element.el [ Element.Font.size 16 ] (Element.text "0%")

        unsignedPercentString ->
            let
                isUp =
                    marginFloat >= 0
            in
            Element.row [ Element.spacing 4 ]
                [ Element.el [ Element.Font.size 18 ]
                    (Element.text unsignedPercentString)
                ]


interval : List (Attribute msg) -> List (Attribute msg) -> ( Element.Color, Element.Color ) -> Time.Posix -> Element msg
interval containerAttributes textAttributes ( defaultColor, zeroColor ) i =
    if TimeHelpers.isNegative i then
        interval
            containerAttributes
            textAttributes
            ( defaultColor, zeroColor )
            (Time.millisToPosix 0)

    else
        let
            hrInterval =
                TimeHelpers.toHumanReadableInterval i

            dc =
                if hrInterval.days == 0 then
                    zeroColor

                else
                    defaultColor

            hc =
                if hrInterval.days == 0 && hrInterval.hours == 0 then
                    zeroColor

                else
                    defaultColor

            mc =
                if Time.posixToMillis i > 0 then
                    defaultColor

                else
                    zeroColor

            numStr num unitStr =
                (String.fromInt num
                    |> String.padLeft 2 '0'
                )
                    ++ unitStr
        in
        Element.row
            ([ Element.spacing 5 ] ++ containerAttributes)
            [ Element.el
                ([ Element.Font.color dc ] ++ textAttributes)
                (Element.text <| numStr hrInterval.days "d")
            , Element.el
                ([ Element.Font.color hc ] ++ textAttributes)
                (Element.text <| numStr hrInterval.hours "h")
            , Element.el
                ([ Element.Font.color mc ] ++ textAttributes)
                (Element.text <| numStr hrInterval.min "m")
            ]


intervalWithElapsedBar : List (Attribute msg) -> List (Attribute msg) -> ( Element.Color, Element.Color ) -> ( Time.Posix, Time.Posix ) -> Element msg
intervalWithElapsedBar containerAttributes textAttributes ( defaultColor, zeroColor ) ( i, total ) =
    let
        timeLeftRatio =
            TimeHelpers.getRatio
                i
                total

        color =
            if timeLeftRatio < 0.1 then
                red

            else if timeLeftRatio < 0.2 then
                yellow

            else
                green

        intervalEl =
            Element.el
                [ Element.centerX ]
                (interval
                    []
                    []
                    ( defaultColor, zeroColor )
                    i
                )
    in
    Element.column
        ([ Element.spacing 5 ] ++ containerAttributes)
        [ interval
            [ Element.centerX ]
            textAttributes
            ( defaultColor, zeroColor )
            i
        , elapsedBar (1 - timeLeftRatio) color
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


bigTimeUnitElement : Int -> Element.Color -> String -> Int -> Element msg
bigTimeUnitElement numDigits color labelString num =
    let
        numStr =
            String.fromInt num
                |> String.padLeft numDigits '0'
    in
    Element.el
        [ Element.Font.size 22
        , Element.Font.color color
        ]
        (Element.text <| numStr ++ labelString)


fiatTypeToSymbolElement : String -> Element msg
fiatTypeToSymbolElement fiatType =
    case Dict.get fiatType FiatValue.currencyTypes of
        Nothing ->
            Element.text "*"

        Just ( _, image ) ->
            Images.toElement [ Element.height <| Element.px 26 ] image



-- INPUTS


intervalInput : Maybe Element.Color -> Time.Posix -> (Time.Posix -> msg) -> Element msg
intervalInput maybeLowValColor i newIntervalMsg =
    let
        hrInterval =
            TimeHelpers.toHumanReadableInterval i

        lowValColor =
            maybeLowValColor
                |> Maybe.withDefault black

        dc =
            if hrInterval.days == 0 then
                lightGray

            else
                black

        ( hc, mc ) =
            if hrInterval.days == 0 && hrInterval.hours == 0 then
                ( lightGray, lowValColor )

            else
                ( black, black )

        withModifyArrows : Time.Posix -> Element msg -> Element msg
        withModifyArrows incAmount el =
            Element.column [ Element.spacing 4 ]
                [ Element.el
                    [ Element.padding 4
                    , Element.pointer
                    , Element.Events.onClick <|
                        newIntervalMsg <|
                            TimeHelpers.add i incAmount
                    ]
                    (Images.toElement
                        [ Element.height <| Element.px 10
                        ]
                        Images.upArrow
                    )
                , el
                , Element.el
                    [ Element.padding 4
                    , Element.pointer
                    , Element.Events.onClick <|
                        newIntervalMsg <|
                            (TimeHelpers.sub i incAmount
                                |> TimeHelpers.negativeToZero
                                |> (\t ->
                                        if Time.posixToMillis t == 0 then
                                            Time.millisToPosix <| 1000 * 60 * 5

                                        else
                                            t
                                   )
                            )
                    ]
                    (Images.toElement
                        [ Element.height <| Element.px 10
                        ]
                        Images.downArrow
                    )
                ]
    in
    Element.row
        [ Element.spaceEvenly
        , Element.width Element.fill
        ]
        [ bigTimeUnitElement 3 dc " days" hrInterval.days
            |> withModifyArrows (Time.millisToPosix <| 1000 * 60 * 60 * 24)
        , bigTimeUnitElement 2 hc " hours" hrInterval.hours
            |> withModifyArrows (Time.millisToPosix <| 1000 * 60 * 60)
        , bigTimeUnitElement 2 mc " min" hrInterval.min
            |> withModifyArrows (Time.millisToPosix <| 1000 * 60 * 5)
        ]


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
            ++ [ Element.height <| Element.px 40
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
                ++ [ Element.width <| Element.px 100
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


currencySelector : Bool -> String -> msg -> (String -> msg) -> Element msg
currencySelector showDropdown typeStringInput openCurrencySelectorMsg typeStringChangedMsgConstructor =
    let
        gotCurrency =
            Dict.get typeStringInput FiatValue.currencyTypes

        inputElement =
            Element.Input.text
                [ Element.width <| Element.px 80
                , Element.height <| Element.px 40
                , Element.Font.size 24
                , Element.Font.medium
                , Element.Border.color lightGray
                , onClickNoPropagation openCurrencySelectorMsg
                ]
                { onChange = String.toUpper >> typeStringChangedMsgConstructor
                , text = typeStringInput
                , placeholder = Nothing
                , label = Element.Input.labelHidden "currency type"
                }

        dropdownEl =
            case ( showDropdown, gotCurrency ) of
                ( False, _ ) ->
                    Element.none

                ( True, Just _ ) ->
                    Element.none

                ( True, Nothing ) ->
                    Element.wrappedRow
                        [ Element.width <| Element.px 350
                        , Element.Border.color black
                        , Element.Border.width 1
                        , Element.Background.color white
                        , Element.padding 10
                        , Element.centerX
                        ]
                        (FiatValue.searchTypes typeStringInput
                            |> Dict.toList
                            |> List.map
                                (\( typeString, ( _, image ) ) ->
                                    Element.row
                                        [ Element.width <| Element.px 80
                                        , Element.spacing 9
                                        , Element.paddingXY 0 5
                                        , onClickNoPropagation <| typeStringChangedMsgConstructor typeString
                                        , Element.mouseOver [ Element.Background.color <| Element.rgb 0.8 0.8 1 ]
                                        ]
                                        [ Images.toElement
                                            [ Element.height <| Element.px 26
                                            ]
                                            image
                                        , Element.el [ Element.Font.size 16, Element.Font.semiBold ] <| textWithoutTextCursor typeString
                                        ]
                                )
                        )
    in
    Element.row
        [ Element.spacing 4
        , Element.below dropdownEl
        ]
        [ FiatValue.typeStringToSymbol typeStringInput
        , inputElement
        ]



-- BUTTONS


button : ( Element.Color, Element.Color, Element.Color ) -> Element.Color -> String -> msg -> Element msg
button ( bgColor, bgHoverColor, bgPressedColor ) textColor text msg =
    Element.el
        [ Element.Border.rounded 4
        , Element.pointer
        , Element.Events.onClick msg
        , Element.paddingXY 25 17
        , Element.Font.color textColor
        , Element.Font.size 18
        , Element.Font.semiBold
        , Element.Background.color bgColor
        , Element.mouseDown [ Element.Background.color bgPressedColor ]
        , Element.mouseOver [ Element.Background.color bgHoverColor ]
        ]
        (Element.text text)


closeButton : msg -> Element msg
closeButton msg =
    Element.el
        [ Element.paddingEach
            { top = 0
            , left = 5
            , right = 5
            , bottom = 5
            }
        , Element.Events.onClick msg
        , Element.Border.rounded 30
        , Element.Background.color <| Element.rgba 1 1 1 0.4
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.3
        , Element.pointer
        , Element.Font.size 14
        ]
        (Element.el
            [ Element.Font.color <| Element.rgba 0 0 0 0.7 ]
            (Element.text "x")
        )


pokeButton : msg -> Element msg
pokeButton pokeMsg =
    Element.Input.button
        [ Element.Background.color <| Element.rgba255 16 7 234 0.2
        , Element.padding 5
        , Element.Border.rounded 4
        , Element.width Element.fill
        , Element.mouseOver [ Element.Background.color <| Element.rgba255 16 7 234 0.4 ]
        ]
        { onPress = Just pokeMsg
        , label =
            Element.el
                [ Element.centerX
                , Element.Font.color <| Element.rgb255 16 7 234
                , Element.Font.medium
                , Element.Font.size 14
                ]
                (Element.text "Poke")
        }


blueButton : String -> msg -> Element msg
blueButton text msg =
    button
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        white
        text
        msg


inverseBlueButton : String -> msg -> Element msg
inverseBlueButton text msg =
    button
        ( Element.rgba 0 0 1 0.05
        , Element.rgba 0 0 1 0.1
        , Element.rgba 0 0 1 0.2
        )
        blue
        text
        msg


redButton : String -> msg -> Element msg
redButton text msg =
    button
        ( Element.rgba 1 0 0 1
        , Element.rgba 1 0 0 0.8
        , Element.rgba 1 0 0 0.6
        )
        white
        text
        msg


disabledButton : String -> Maybe String -> Element msg
disabledButton text maybeTipText =
    Element.el
        [ Element.Border.rounded 4
        , Element.paddingXY 25 17
        , Element.Font.size 18
        , Element.Font.semiBold
        , Element.Background.color lightGray
        , Element.above <|
            maybeErrorElement
                [ Element.moveUp 5 ]
                maybeTipText
        ]
        (Element.text text)


orangeButton : String -> msg -> Element msg
orangeButton text msg =
    button
        ( Element.rgba 1 0.6 0.2 1
        , Element.rgba 1 0.6 0.2 0.8
        , Element.rgba 1 0.6 0.2 0.6
        )
        white
        text
        msg



-- STYLE HELPERS


textWithoutTextCursor : String -> Element msg
textWithoutTextCursor s =
    Html.Styled.styled
        Html.Styled.span
        [ Css.hover [ Css.cursor Css.default ] ]
        []
        [ Html.Styled.text s ]
        |> Html.Styled.toUnstyled
        |> Element.html


onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg =
    Html.Events.stopPropagationOn
        "click"
        (Json.Decode.succeed ( msg, True ))
        |> Element.htmlAttribute


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


withHeader : String -> Element msg -> Element msg
withHeader headerString element =
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


niceBottomBorderEl : Element msg -> Element msg
niceBottomBorderEl =
    Element.el
        [ Element.Border.color lightGray
        , Element.Border.widthEach
            { bottom = 2
            , top = 0
            , right = 0
            , left = 0
            }
        ]



-- SPECIAL CHARS


bulletPointString : String
bulletPointString =
    Char.fromCode 8226
        |> String.fromChar



-- IMAGES


daiSymbol : List (Attribute msg) -> Element msg
daiSymbol attributes =
    Images.toElement
        ((Element.height <| Element.px 26) :: attributes)
        Images.daiSymbol


daiSymbolAndLabel : FactoryType -> Element msg
daiSymbolAndLabel factoryType =
    Element.row
        [ Element.spacing 4 ]
        [ daiSymbol []
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.Font.color currencyLabelColor
            ]
            (Element.text <| Config.tokenUnitName factoryType)
        ]


marginSymbol : List (Attribute msg) -> Bool -> Maybe Bool -> Element msg
marginSymbol attributes isUp maybeIsGreen =
    Images.toElement
        ((Element.height <| Element.px 34) :: attributes)
        (Images.marginSymbol isUp maybeIsGreen)



-- DEBUG


testBorderStyles : List (Attribute msg)
testBorderStyles =
    [ Element.Border.width 1
    , Element.Border.color (Element.rgb 1 0 1)
    ]



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
                        |> String.left (extraDigitsNeeded + 1)
    in
    preDecimalString ++ decimalString ++ "%"


modal : Element.Color -> Element msg -> Element msg
modal overlayColor =
    Element.el
        [ Element.Background.color overlayColor
        , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
        , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
        , Element.htmlAttribute <| Html.Attributes.style "top" "0"
        , Element.htmlAttribute <| Html.Attributes.style "left" "0"
        , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
        ]


closeableModal : Element msg -> msg -> Element msg
closeableModal innerEl closeMsg =
    (modal <| Element.rgba 0 0 0.3 0.6) <|
        Element.el
            [ Element.centerX
            , Element.centerY
            , Element.height Element.shrink
            , Element.width (Element.shrink |> Element.maximum 400)
            , Element.Background.color white
            , Element.Border.rounded 8
            , Element.padding 30
            , Element.inFront <|
                Element.el
                    [ Element.alignRight
                    , Element.alignTop
                    , Element.moveUp 5
                    , Element.moveRight 5
                    ]
                    (closeButton closeMsg)
            ]
            (Element.paragraph [ Element.width Element.fill ] [ innerEl ])


txProcessModal : List (Element msg) -> Element msg
txProcessModal textLines =
    (modal <| Element.rgba 0 0 0.3 0.6)
        (textLines
            |> List.map
                (\line ->
                    Element.paragraph
                        [ Element.centerX
                        , Element.centerY
                        , Element.Font.size 20
                        , Element.Font.semiBold
                        , Element.Font.color white
                        , Element.Font.center
                        ]
                        [ line ]
                )
            |> Element.column
                [ Element.spacing 10
                , Element.centerX
                , Element.centerY
                , Element.Background.color <| Element.rgba 0 0 0 0.5
                , Element.Border.rounded 8
                , Element.padding 20
                ]
        )


comingSoonMsg : List (Attribute msg) -> String -> Element msg
comingSoonMsg attributes text =
    Element.paragraph
        ([ Element.Font.size 12
         , Element.Font.color red
         ]
            ++ attributes
        )
        [ Element.text text ]


niceFloatingRow : List (Element msg) -> Element msg
niceFloatingRow =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color white
        , Element.Border.rounded 5
        , Element.padding 20
        , Element.spaceEvenly
        , subtleShadow
        ]


elOnCircle : List (Attribute msg) -> Int -> Element.Color -> Element msg -> Element msg
elOnCircle attributes width color el =
    let
        circleElement =
            Collage.circle (toFloat width / 2)
                |> Collage.filled (Collage.uniform (elementColorToAvh4Color color))
                |> Collage.Render.svg
                |> Element.html
    in
    Element.el
        ([ Element.inFront el ] ++ attributes)
        circleElement


elementColorToAvh4Color : Element.Color -> Color
elementColorToAvh4Color c =
    Element.toRgb c
        |> (\rgba ->
                Color.rgba
                    rgba.red
                    rgba.green
                    rgba.blue
                    rgba.alpha
           )


maybeErrorElement : List (Attribute msg) -> Maybe String -> Element msg
maybeErrorElement attributes maybeError =
    case maybeError of
        Nothing ->
            Element.none

        Just errorString ->
            Element.el
                ([ Element.Border.rounded 5
                 , Element.Border.color <| Element.rgb 0.9 0 0
                 , Element.Border.width 1
                 , Element.Background.color <| Element.rgb 1 0.4 0.4
                 , Element.padding 5
                 , Element.centerX
                 , Element.centerY
                 , Element.width (Element.shrink |> Element.maximum 200)
                 , Element.Font.size 14
                 ]
                    ++ attributes
                )
                (Element.paragraph
                    []
                    [ Element.text errorString ]
                )


etherscanAddressLink : List (Attribute msg) -> FactoryType -> Address -> Element msg
etherscanAddressLink attributes factoryType address =
    Element.newTabLink
        attributes
        { url = EthHelpers.makeViewAddressUrl factoryType address
        , label = Element.text <| Eth.Utils.addressToString address
        }


coolCurrencyHbreak : Bool -> Element.Length -> Element msg
coolCurrencyHbreak reversed length =
    Element.el
        [ Element.width Element.fill
        , Element.inFront
            (FiatValue.currencyTypes
                |> Dict.toList
                |> List.map (Tuple.second >> Tuple.first)
                |> List.Extra.unique
                |> (if reversed then
                        List.reverse

                    else
                        identity
                   )
                |> List.Extra.cycle 100
                |> List.map (Element.el [ Element.Font.color (Element.rgba 0 0 0 0.2) ] << Element.text)
                |> List.intersperse
                    (Element.el
                        [ Element.Font.color (Element.rgba 0 0 0 0.1)
                        , Element.Font.size 14
                        ]
                     <|
                        Element.text bulletPointString
                    )
                |> Element.row
                    [ Element.spacing 3
                    , Element.width <| Element.fillPortion 100
                    , Element.width length
                    , Element.clip
                    ]
            )
        ]
        (Element.text " ")


ethAddress : Int -> Address -> Element msg
ethAddress fontSize address =
    Element.el
        [ Element.Border.rounded 4
        , Element.Background.color <| Element.rgba 0 0 1 0.1
        , Element.Font.color <| Element.rgb255 16 7 234
        , Element.Font.size fontSize
        , Element.Font.semiBold
        , Element.paddingXY 15 13
        ]
        (Element.text <| Eth.Utils.addressToString address)
