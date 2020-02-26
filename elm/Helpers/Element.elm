module Helpers.Element exposing (abortedIconColor, activePhaseBackgroundColor, addAlpha, basicOpenDropdown, bigTimeUnitElement, black, blue, blueButton, bulletPointString, burnedIconColor, button, closeButton, closeableModal, closeableModalBlackX, closeableModalWhiteX, coloredResponderProfit, comingSoonMsg, coolCurrencyHbreak, currencyLabelColor, daiSymbol, daiSymbolAndLabel, daiValue, daiYellow, darkGray, darkYellow, disabledButton, disabledTextColor, dollarGreen, dropdownSelector, elOnCircle, elapsedBar, elementColorToAvh4Color, ethAddress, etherscanAddressLink, fakeLink, fancyInput, grayButton, green, inputContainer, interval, intervalInput, intervalWithElapsedBar, inverseBlueButton, lightBlue, lightBlueButton, lightGray, lightRed, maybeErrorElement, mediumGray, modal, moveToFront, niceBottomBorderEl, niceFloatingRow, noSelectText, onClickNoPropagation, optionsDots, orangeButton, pageBackgroundColor, permanentTextColor, placeholderTextColor, pokeButton, price, redButton, releasedIconColor, responderProfitFloatToConciseString, responderProfitSymbol, roundBottomCorners, roundTopCorners, roundedComplexInputBox, scrollbarYEl, searchableOpenDropdown, simpleSubmodelContainer, softRed, submodelBackgroundColor, submodelContainer, subtleShadow, testBorderStyles, textInputWithElement, textWithoutTextCursor, thinGrayHRuler, transparent, txProcessModal, uncoloredResponderProfit, white, withHeader, withInputHeader, withInputHeaderAndMaybeError, withSelectedUnderline, yellow)

import Browser.Dom
import Collage exposing (Collage)
import Collage.Render
import Color exposing (Color)
import CommonTypes exposing (..)
import Config
import Css
import Currencies exposing (Price)
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
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


transparent =
    Element.rgba 0 0 0 0


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


softRed =
    Element.rgb255 255 0 110


lightRed =
    Element.rgb 1 0.8 0.8


green =
    Element.rgb255 51 183 2


blue =
    Element.rgb 0 0 1


lightBlue =
    Element.rgb 0.8 0.8 1


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


placeholderTextColor =
    Element.rgb255 213 217 222


mediumGray =
    Element.rgb255 200 205 210


darkGray =
    Element.rgb255 150 150 150


activePhaseBackgroundColor =
    Element.rgb255 9 32 107


permanentTextColor =
    Element.rgba255 1 31 52 0.8


submodelBackgroundColor =
    Element.rgb 0.95 0.98 1


pageBackgroundColor =
    Element.rgb255 242 243 247


disabledTextColor =
    Element.rgba255 1 31 52 0.13


currencyLabelColor =
    Element.rgb255 109 127 138


releasedIconColor =
    Element.rgb255 0 255 0


abortedIconColor =
    Element.rgb255 250 165 22


burnedIconColor =
    Element.rgb255 255 0 0


addAlpha : Float -> Element.Color -> Element.Color
addAlpha a color =
    let
        oldRgba =
            Element.toRgb color
    in
    Element.fromRgb
        { oldRgba
            | alpha = a
        }



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


price : Price -> Element msg
price p =
    Element.row [ Element.spacing 4 ]
        [ Element.text <| Currencies.toStringNoSymbol p
        , Element.el
            [ Element.Font.color <| Element.rgba 0 0 0 0.5
            , Element.Font.medium
            ]
            (Element.text p.symbol)
        , Currencies.icon p.symbol |> Maybe.withDefault Element.none
        ]


coloredResponderProfit : Bool -> Float -> Element msg
coloredResponderProfit upIsGreen responderProfitFloat =
    case responderProfitFloatToConciseString responderProfitFloat of
        "0%" ->
            Element.el [ Element.Font.size 16 ] (Element.text "0%")

        unsignedPercentString ->
            let
                isUp =
                    responderProfitFloat >= 0

                isGreen =
                    not <| xor isUp upIsGreen

                textColor =
                    if isGreen then
                        green

                    else
                        softRed
            in
            Element.el [ Element.Font.color textColor, Element.Font.size 16 ]
                (Element.text unsignedPercentString)


uncoloredResponderProfit : Float -> Element msg
uncoloredResponderProfit responderProfitFloat =
    case responderProfitFloatToConciseString responderProfitFloat of
        "0%" ->
            Element.el [ Element.Font.size 16 ] (Element.text "0%")

        unsignedPercentString ->
            let
                isUp =
                    responderProfitFloat >= 0
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
                softRed

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



-- priceSymbolToImageElement : Currencies.Symbol -> Element msg
-- priceSymbolToImageElement symbol =
--     case Dict.get symbol Currencies.charsAndImages of
--         Nothing ->
--             Element.text "*"
--         Just ( _, maybeImage ) ->
--             Images.toElement [ Element.height <| Element.px 26 ]
--                 (maybeImage
--                     |> Maybe.withDefault Images.none
--                 )
-- INPUTS


roundedComplexInputBox :
    List (Attribute msg)
    -> List (Element msg)
    ->
        { onChange : String.String -> msg
        , text : String.String
        , placeholder : Maybe.Maybe (Element.Input.Placeholder msg)
        , label : Element.Input.Label msg
        }
    -> List (Element msg)
    -> Element msg
roundedComplexInputBox extraAttributes leftItems inputConfig rightItems =
    Element.row
        (extraAttributes
            ++ [ Element.Border.rounded 20
               , Element.paddingXY 20 0
               , Element.Border.width 1
               , Element.Background.color white
               , Element.Border.color <| Element.rgba 0 0 0 0.1
               , Element.height <| Element.px 60
               ]
        )
        (leftItems
            ++ [ Element.Input.text
                    [ Element.Border.width 0
                    , Element.width Element.fill
                    , Element.Font.alignRight
                    ]
                    inputConfig
               ]
            ++ rightItems
        )


intervalInput : Element.Color -> Time.Posix -> (Time.Posix -> msg) -> Element msg
intervalInput numberColor i newIntervalMsg =
    let
        hrInterval =
            TimeHelpers.toHumanReadableInterval i

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
        , Element.spacing 10
        ]
        [ bigTimeUnitElement 3 numberColor " days" hrInterval.days
            |> withModifyArrows (Time.millisToPosix <| 1000 * 60 * 60 * 24)
        , bigTimeUnitElement 2 numberColor " hours" hrInterval.hours
            |> withModifyArrows (Time.millisToPosix <| 1000 * 60 * 60)
        , bigTimeUnitElement 2 numberColor " min" hrInterval.min
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


dropdownSelector : List ( Element msg, msg ) -> Element msg
dropdownSelector itemsAndMsgs =
    Element.column
        [ Element.Border.color <| Element.rgba 0 0 0 0.2
        , Element.Border.width 1
        , Element.Border.rounded 5
        , Element.Background.color white
        , Element.padding 10
        , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
        , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
        , Element.Border.shadow
            { offset = ( 2, 2 )
            , size = 0
            , blur = 10
            , color = Element.rgba 0 0 0 0.1
            }
        ]
        (itemsAndMsgs
            |> List.map
                (\( el, msg ) ->
                    Element.el
                        [ Element.paddingXY 0 5
                        , onClickNoPropagation msg
                        , Element.mouseOver [ Element.Background.color <| Element.rgb 0.8 0.8 1 ]
                        ]
                        el
                )
        )



-- BUTTONS


button : DisplayProfile -> List (Attribute msg) -> ( Element.Color, Element.Color, Element.Color ) -> Element.Color -> List String -> msg -> Element msg
button dProfile attributes ( bgColor, bgHoverColor, bgPressedColor ) textColor lines msg =
    Element.column
        (attributes
            ++ [ Element.Border.rounded 4
               , Element.spacing (8 |> changeForMobile 5 dProfile)
               , Element.pointer
               , Element.Events.onClick msg
               , Element.paddingXY 25 17 |> changeForMobile (Element.padding 10) dProfile
               , Element.Font.color textColor
               , Element.Font.size (18 |> changeForMobile 16 dProfile)
               , Element.Font.semiBold
               , Element.Background.color bgColor
               , Element.mouseDown [ Element.Background.color bgPressedColor ]
               , Element.mouseOver [ Element.Background.color bgHoverColor ]
               , noSelectText
               ]
        )
        (List.map
            (Element.el [ Element.centerX ] << Element.text)
            lines
        )


closeButton : Bool -> msg -> Element msg
closeButton isBlack msg =
    Element.el
        [ Element.padding 10
        , Element.Events.onClick msg
        , Element.pointer
        ]
        (Images.toElement [ Element.width <| Element.px 22 ]
            (if isBlack then
                Images.closeIconBlack

             else
                Images.closeIconWhite
            )
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


blueButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
blueButton dProfile attributes text msg =
    button dProfile
        attributes
        ( Element.rgba 0 0 1 1
        , Element.rgba 0 0 1 0.8
        , Element.rgba 0 0 1 0.6
        )
        white
        text
        msg


lightBlueButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
lightBlueButton dProfile attributes text msg =
    let
        color =
            Element.rgb255 25 169 214
    in
    button dProfile
        attributes
        ( color
        , color |> addAlpha 0.8
        , color |> addAlpha 0.6
        )
        white
        text
        msg


inverseBlueButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
inverseBlueButton dProfile attributes text msg =
    button dProfile
        attributes
        ( Element.rgba 0 0 1 0.05
        , Element.rgba 0 0 1 0.1
        , Element.rgba 0 0 1 0.2
        )
        blue
        text
        msg


redButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
redButton dProfile attributes text msg =
    button dProfile
        attributes
        ( Element.rgba 1 0 0 1
        , Element.rgba 1 0 0 0.8
        , Element.rgba 1 0 0 0.6
        )
        white
        text
        msg


disabledButton : DisplayProfile -> List (Attribute msg) -> String -> Maybe String -> Element msg
disabledButton dProfile attributes text maybeTipText =
    Element.el
        (attributes
            ++ [ Element.Border.rounded 4
               , Element.paddingXY 25 17 |> changeForMobile (Element.paddingXY 10 5) dProfile
               , Element.Font.size (18 |> changeForMobile 16 dProfile)
               , Element.Font.semiBold
               , Element.Background.color lightGray
               , Element.Font.center
               , noSelectText
               , Element.above <|
                    maybeErrorElement
                        [ Element.moveUp 5 ]
                        maybeTipText
               ]
        )
        (Element.text text)


orangeButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
orangeButton dProfile attributes text msg =
    button dProfile
        attributes
        ( Element.rgba 1 0.6 0.2 1
        , Element.rgba 1 0.6 0.2 0.8
        , Element.rgba 1 0.6 0.2 0.6
        )
        white
        text
        msg


grayButton : DisplayProfile -> List (Attribute msg) -> List String -> msg -> Element msg
grayButton dProfile attributes text msg =
    button dProfile
        attributes
        ( Element.rgba 0 0 0 0.05
        , Element.rgba 0 0 0 0.1
        , Element.rgba 0 0 0 0.12
        )
        black
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
            (Element.text <| tokenUnitName factoryType)
        ]


responderProfitSymbol : List (Attribute msg) -> Bool -> Maybe Bool -> Element msg
responderProfitSymbol attributes isUp maybeIsGreen =
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


responderProfitFloatToConciseString : Float -> String
responderProfitFloatToConciseString f =
    (if f > 0 then
        "+"

     else
        ""
    )
        ++ (if f < 0.1 then
                f
                    |> (*) 1000
                    |> round
                    |> toFloat
                    |> (\f_ -> f_ / 10)
                    |> String.fromFloat
                    |> (\s -> s ++ "%")

            else
                f
                    |> (*) 100
                    |> round
                    |> String.fromInt
                    |> (\s -> s ++ "%")
           )


modal : Element.Color -> Bool -> msg -> msg -> Element msg -> Element msg
modal overlayColor includeScrollbarY clickInsideMsg clickOutsideMsg el =
    Element.el
        ([ Element.behindContent <|
            Element.el
                [ Element.Background.color overlayColor
                , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
                , Element.htmlAttribute <| Html.Attributes.style "top" "0"
                , Element.htmlAttribute <| Html.Attributes.style "left" "0"
                , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                , Element.Events.onClick clickOutsideMsg
                ]
                Element.none
         , Element.width Element.fill
         , Element.height Element.fill
         , onClickNoPropagation clickInsideMsg
         ]
            ++ (if includeScrollbarY then
                    [ Element.scrollbarY ]

                else
                    []
               )
        )
        el


closeableModal : Bool -> List (Attribute msg) -> Element msg -> msg -> msg -> Bool -> Element msg
closeableModal isBlack extraAttributes innerEl clickInsideMsg closeMsg includeScrollbarY =
    modal
        (Element.rgba 0 0 0.3 0.6)
        includeScrollbarY
        clickInsideMsg
        closeMsg
    <|
        Element.el
            ([ Element.centerX
             , Element.centerY
             , Element.width (Element.fill |> Element.maximum 700)
             , Element.Background.color white
             , Element.Border.rounded 8
             , Element.inFront <|
                Element.el
                    [ Element.alignRight
                    , Element.alignTop
                    ]
                    (closeButton isBlack closeMsg)
             ]
                ++ extraAttributes
            )
            innerEl


closeableModalBlackX =
    closeableModal True


closeableModalWhiteX =
    closeableModal False


txProcessModal : List (Element msg) -> msg -> msg -> msg -> Element msg
txProcessModal textLines clickInsideMsg closeMsg clickOutsideMsg =
    modal
        (Element.rgba 0 0 0.3 0.6)
        False
        clickInsideMsg
        clickOutsideMsg
    <|
        Element.column
            [ Element.spacing 10
            , Element.centerX
            , Element.centerY
            , Element.Background.color <| Element.rgba 0 0 0 0.5
            , Element.Border.rounded 8
            , Element.padding 20
            , Element.inFront <|
                Element.el
                    [ Element.alignRight
                    , Element.alignTop
                    ]
                    (closeButton False closeMsg)
            ]
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
            )


comingSoonMsg : List (Attribute msg) -> String -> Element msg
comingSoonMsg attributes text =
    Element.paragraph
        ([ Element.Font.size 12
         , Element.Font.color softRed
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
        ([ Element.inFront <|
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                el
         ]
            ++ attributes
        )
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
                 , Element.Border.color softRed
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
            (Currencies.fiatCharsAndImages
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


scrollbarYEl : List (Attribute msg) -> Element msg -> Element msg
scrollbarYEl attrs body =
    Element.el [ Element.height Element.fill, Element.width Element.fill ] <|
        Element.el
            ([ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
             , Element.htmlAttribute <| Html.Attributes.style "top" "0"
             , Element.htmlAttribute <| Html.Attributes.style "right" "0"
             , Element.htmlAttribute <| Html.Attributes.style "bottom" "0"
             , Element.htmlAttribute <| Html.Attributes.style "left" "0"
             , Element.scrollbarY
             ]
                ++ attrs
            )
            body


simpleSubmodelContainer : Int -> Element msg -> Element msg
simpleSubmodelContainer maxWidth el =
    Element.el
        [ Element.width Element.fill
        ]
    <|
        Element.el
            [ Element.Background.color white
            , Element.Border.rounded 8
            , Element.centerX
            , Element.width (Element.fill |> Element.maximum maxWidth)
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.06
                }
            ]
            el


submodelContainer : Int -> DisplayProfile -> Maybe String -> Maybe (Element msg) -> Element msg -> Element msg
submodelContainer maxWidth dProfile maybeBigTitleText maybeTitleEl el =
    Element.column
        [ Element.spacing 60
        , Element.width Element.fill
        ]
        [ Maybe.map
            (Element.el
                [ Element.Font.color white
                , Element.Font.size 38
                , Element.centerX
                ]
                << Element.text
            )
            maybeBigTitleText
            |> Maybe.withDefault Element.none
        , Element.column
            [ Element.Background.color <| submodelBackgroundColor
            , Element.spacing (20 |> changeForMobile 5 dProfile)
            , Element.Border.rounded 8
            , Element.clip
            , Element.centerX
            , Element.width (Element.fill |> Element.maximum maxWidth)
            , Element.Border.shadow
                { offset = ( 0, 0 )
                , size = 1
                , blur = 3
                , color = Element.rgba 0 0 0 0.2
                }
            ]
            [ Maybe.map
                (Element.el
                    [ Element.width Element.fill
                    , Element.padding (15 |> changeForMobile 5 dProfile)
                    , Element.Background.color white
                    , Element.Border.shadow
                        { offset = ( 0, 0 )
                        , size = 0
                        , blur = 30
                        , color = Element.rgba 0 0 0 0.15
                        }
                    ]
                )
                maybeTitleEl
                |> Maybe.withDefault Element.none
            , el
            ]
        ]


withInputHeader : DisplayProfile -> List (Attribute msg) -> String -> Element msg -> Element msg
withInputHeader dProfile attributes titleStr el =
    withInputHeaderAndMaybeError dProfile attributes titleStr Nothing el


withInputHeaderAndMaybeError : DisplayProfile -> List (Attribute msg) -> String -> Maybe String -> Element msg -> Element msg
withInputHeaderAndMaybeError dProfile attributes titleStr maybeError el =
    Element.column
        (attributes
            ++ [ Element.spacing 10
               ]
        )
        [ Element.row
            [ Element.spacing (20 |> changeForMobile 10 dProfile)
            ]
            [ Element.el
                [ Element.Font.size (18 |> changeForMobile 16 dProfile)
                , Element.Font.semiBold
                , Element.Font.color <| Element.rgb255 1 31 52
                , Element.alignLeft
                ]
                (Element.text titleStr)
            , case maybeError of
                Just error ->
                    Element.el
                        [ Element.Font.size (12 |> changeForMobile 10 dProfile)
                        , Element.Font.color softRed
                        ]
                        (Element.text error)

                Nothing ->
                    Element.none
            ]
        , el
        ]


withSelectedUnderline : List (Attribute msg) -> Bool -> Element msg -> Element msg
withSelectedUnderline attributes selected el =
    Element.el
        (attributes
            ++ [ Element.Border.widthEach
                    { bottom = 2
                    , top = 0
                    , right = 0
                    , left = 0
                    }
               , Element.paddingEach
                    { bottom = 2
                    , top = 0
                    , right = 0
                    , left = 0
                    }
               , Element.Border.color
                    (if selected then
                        blue

                     else
                        Element.rgba 0 0 0 0
                    )
               ]
        )
        el


thinGrayHRuler : Element msg
thinGrayHRuler =
    Element.el
        [ Element.height <| Element.px 1
        , Element.width Element.fill
        , Element.Background.color <| Element.rgba 0 0 0 0.2
        ]
        Element.none


basicOpenDropdown : List (Attribute msg) -> Maybe (Element msg) -> List ( Element msg, msg ) -> Element msg
basicOpenDropdown attributes maybeFirstEl items =
    Element.el
        (attributes
            ++ [ Element.Background.color white
               , Element.Border.rounded 6
               , Element.Border.shadow
                    { offset = ( 0, 3 )
                    , size = 0
                    , blur = 20
                    , color = Element.rgba 0 0 0 0.08
                    }
               ]
        )
    <|
        Element.column
            [ Element.Background.color lightGray
            , Element.spacing 1
            , Element.width Element.fill
            , Element.height (Element.shrink |> Element.maximum 340)
            ]
            [ maybeFirstEl |> Maybe.withDefault Element.none
            , Element.column
                [ Element.scrollbarY
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color lightGray
                , Element.spacing 1
                ]
                (items
                    |> List.map
                        (\( el, onClick ) ->
                            Element.el
                                [ Element.paddingXY 14 10
                                , Element.Background.color white
                                , Element.width Element.fill
                                , Element.Events.onClick onClick
                                , Element.pointer
                                , Element.mouseOver
                                    [ Element.Background.color <| Element.rgba 0 0 1 0.15 ]
                                ]
                                el
                        )
                )
            ]


searchableOpenDropdown : List (Attribute msg) -> String -> List ( Element msg, List String, msg ) -> String -> (String -> msg) -> Element msg
searchableOpenDropdown attributes placeholderText items searchInput searchInputChangedMsg =
    let
        filteredItems =
            if searchInput == "" then
                items
                    |> List.map (\( a, b, c ) -> ( a, c ))

            else
                items
                    |> List.filterMap
                        (\( el, searchables, onClick ) ->
                            if List.any (String.contains (String.toLower searchInput)) (List.map String.toLower searchables) then
                                Just ( el, onClick )

                            else
                                Nothing
                        )
    in
    basicOpenDropdown
        attributes
        (Just <|
            Element.el
                [ Element.width Element.fill
                , Element.paddingXY 9 15
                , Element.Background.color white
                ]
            <|
                Element.row
                    [ Element.width Element.fill
                    , Element.Background.color <| Element.rgb 0.98 0.98 0.98
                    , Element.paddingXY 13 0
                    , Element.spacing 13
                    , Element.Border.rounded 4
                    ]
                    [ Images.toElement
                        [ Element.width <| Element.px 21 ]
                        Images.searchIcon
                    , Element.Input.text
                        [ Element.Border.width 0
                        , Element.width Element.fill
                        , Element.Background.color <| Element.rgb 0.98 0.98 0.98
                        ]
                        { onChange = searchInputChangedMsg
                        , text = searchInput
                        , placeholder =
                            Just <|
                                Element.Input.placeholder
                                    [ Element.Font.color placeholderTextColor ]
                                    (Element.text placeholderText)
                        , label = Element.Input.labelHidden "search"
                        }
                    ]
        )
        filteredItems


inputContainer : DisplayProfile -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
inputContainer dProfile attributes =
    Element.row <|
        [ Element.Background.color lightGray
        , Element.height <| Element.px (55 |> changeForMobile 40 dProfile)
        , Element.Border.rounded 4
        , Element.Border.width 1
        , Element.Border.color lightGray
        , Element.spacing 1
        ]
            ++ attributes
            ++ (case dProfile of
                    Desktop ->
                        []

                    Mobile ->
                        [ Element.Font.size 14 ]
               )


moveToFront : Attribute msg
moveToFront =
    Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"


optionsDots : DisplayProfile -> Element msg
optionsDots dProfile =
    case dProfile of
        Desktop ->
            Images.toElement
                []
                Images.threeDotsHorizontal

        Mobile ->
            Images.toElement
                [ Element.paddingXY 5 0 ]
                Images.threeDotsVertical


noSelectText : Attribute msg
noSelectText =
    Html.Attributes.style "user-select" "none"
        |> Element.htmlAttribute
