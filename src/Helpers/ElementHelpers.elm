module ElementHelpers exposing (black, block, blockBackgroundColor, blockBorderColor, blockPlusAttributes, blue, bulletPointString, buttonBlue, buttonDeepBlue, buttonGreen, buttonRed, buyer, clauseList, contractActionButton, contractBackgroundColor, contractBorderColor, contractInsetBackgroundColor, contractShadowAttribute, currencySelector, daiSymbol, daiValue, errorMessage, fakeLink, fiatSymbolElementFromFiatType, fiatValue, fillWidthBlock, green, hbreak, headerBackgroundColor, initiator, initiatorBackgroundColor, initiatorColor, interval, intervalWithElapsedBar, lightGray, margin, methodName, pageBackgroundColor, pageTitle, red, responder, responderBackgroundColor, responderColor, roundBottomCorners, roundTopCorners, secondsRemainingString, sectionHeading, sectionReference, seller, smallInput, subpageBackgroundColor, testBorderStyles, textInputWithElement, timeInput, timeValue, tokenValue, white, yellow)

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


initiatorColor =
    Element.rgb 0 0.7 0


responderColor =
    Element.rgb 1 0.55 0


buyerColor =
    initiatorColor


sellerColor =
    responderColor


initiatorBackgroundColor =
    Element.rgb 0.6 1 0.6


responderBackgroundColor =
    Element.rgb 1 0.7 0.5


buttonBlue =
    Element.rgb 0 0 1


buttonGreen =
    Element.rgb 0 1 0


buttonRed =
    Element.rgb 1 0 0


headerBackgroundColor =
    Element.rgb255 10 33 108


pageBackgroundColor =
    Element.rgb 0.9 0.9 0.9


subpageBackgroundColor =
    Element.rgb 0.95 0.95 0.95


blockBackgroundColor =
    Element.rgb 1 1 1


blockBorderColor =
    Element.rgb 0.1 0.4 0.7


contractBackgroundColor =
    Element.rgb 1 1 0.7


contractInsetBackgroundColor =
    Element.rgb 1 1 0.85


contractBorderColor =
    Element.rgb 0.5 0.5 0.1


defaultHbreakColor =
    Element.rgb 0.8 0.8 0.8


buttonDeepBlue =
    Element.rgb 0 0.15 0.35



-- HEADINGS


pageTitle : String -> Element msg
pageTitle s =
    let
        styles =
            [ Element.Font.size 36
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.centerX
            ]
    in
    Element.el styles (Element.text s)


sectionHeading : String -> Element msg
sectionHeading s =
    Element.column [ Element.spacing 30 ]
        [ Element.el [ Element.Font.size 30, Element.Font.bold ] (Element.text s)
        , Element.el [] Element.none
        ]


block : String -> Element msg -> Element msg
block title bodyElement =
    blockPlusAttributes title bodyElement []


blockPlusAttributes : String -> Element msg -> List (Attribute msg) -> Element msg
blockPlusAttributes title bodyElement attributes =
    let
        elementStyles =
            attributes

        headerStyles =
            [ Element.padding 10
            , Element.width Element.fill
            , Element.Background.color blockBorderColor
            , roundTopCorners 10
            ]

        bodyStyles =
            [ Element.Background.color blockBackgroundColor
            , Element.padding 20
            , Element.width Element.fill
            , roundBottomCorners 10
            , Element.Border.color blockBorderColor
            , Element.Border.width 3
            ]
    in
    Element.column elementStyles
        [ Element.el headerStyles
            (Element.el
                [ Element.Font.size 28, Element.Font.color white, Element.centerX ]
                (Element.text title)
            )
        , Element.el bodyStyles bodyElement
        ]


fillWidthBlock : String -> Element msg -> Element msg
fillWidthBlock title bodyElement =
    blockPlusAttributes title bodyElement [ Element.width Element.fill ]



-- SPECIAL TERMS


initiator : List (Attribute msg) -> Element msg
initiator attributes =
    Element.el (attributes ++ [ Element.Font.color initiatorColor ]) (Element.text "Initiator")


responder : List (Attribute msg) -> Element msg
responder attributes =
    Element.el (attributes ++ [ Element.Font.color responderColor ]) (Element.text "Responder")


buyer : List (Attribute msg) -> Element msg
buyer attributes =
    Element.el (attributes ++ [ Element.Font.color buyerColor ]) (Element.text "Buyer")


seller : List (Attribute msg) -> Element msg
seller attributes =
    Element.el (attributes ++ [ Element.Font.color sellerColor ]) (Element.text "Seller")



-- TEXT STYLES


methodName : String -> Element msg
methodName name =
    Element.el [ Element.Font.family [ Element.Font.monospace ], Element.Background.color (Element.rgb 0.9 0.9 0.9) ] (Element.text name)


sectionReference : String -> Element msg
sectionReference name =
    Element.el [ Element.Font.bold ] (Element.text name)


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
            TokenValue.renderToString Nothing tv ++ " Dai"
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


timeValue : Time.Posix -> Element msg
timeValue tv =
    let
        s =
            (TimeHelpers.posixToSeconds tv
                // (60 * 60 * 24)
                |> String.fromInt
            )
                ++ " days"
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)


secondsRemainingString : Time.Posix -> Time.Posix -> String
secondsRemainingString end now =
    let
        secondsLeftString =
            TimeHelpers.sub end now
                |> Time.posixToMillis
                |> (\millis -> millis // 1000)
                |> String.fromInt
    in
    secondsLeftString ++ " seconds"


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



-- GROUPINGS


clauseList : List (Element msg) -> Element msg
clauseList clauseElements =
    let
        constructClauseElement body =
            Element.row []
                [ Element.el [ Element.alignTop, Element.Font.size 24, Element.width (Element.px 50) ] (Element.el [ Element.centerX ] (Element.text bulletPointString))
                , body
                ]
    in
    Element.column [ Element.spacing 20, Element.padding 10 ]
        (List.map constructClauseElement clauseElements)



-- INPUTS


smallInput : String -> String -> (String -> msg) -> Element msg
smallInput labelStr valueStr msgConstructor =
    Element.Input.text [ Element.width (Element.px 100) ]
        { onChange = msgConstructor
        , text = valueStr
        , placeholder = Nothing
        , label = Element.Input.labelHidden labelStr
        }


timeInput : String -> String -> (String -> msg) -> Element msg
timeInput labelStr value msgConstructor =
    Element.row []
        [ Element.Input.text [ Element.width (Element.px 50) ]
            { onChange = msgConstructor
            , text = value
            , placeholder = Nothing
            , label = Element.Input.labelHidden labelStr
            }
        , Element.text " days"
        ]


textInputWithElement : List (Attribute msg) -> List (Attribute msg) -> Element msg -> String -> String -> Maybe (Element.Input.Placeholder msg) -> Maybe (Bool -> msg) -> (String -> msg) -> Element msg
textInputWithElement attributes inputAttributes addedElement labelStr value placeholder maybeShowHideMsgConstructor msgConstructor =
    let
        focusEventAttributes =
            case maybeShowHideMsgConstructor of
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
        (fiatSymbolElementFromFiatType typeInput)
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


fiatSymbolElementFromFiatType : String -> Element msg
fiatSymbolElementFromFiatType fiatType =
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



-- BUTTONS


contractActionButton : String -> Element.Color -> msg -> Element msg
contractActionButton name color msgConstructor =
    Element.Input.button
        [ Element.padding 15
        , Element.Background.color color
        , Element.Border.rounded 5
        ]
        { onPress = Just msgConstructor
        , label = Element.text name
        }



-- STYLE HELPERS


hbreak : Element msg
hbreak =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 1)
        , Element.Background.color defaultHbreakColor
        ]
        Element.none


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


contractShadowAttribute : Attribute msg
contractShadowAttribute =
    Element.Border.shadow
        { offset = ( -3, 10 )
        , size = 0
        , blur = 5
        , color = Element.rgb 0.5 0.5 0.5
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
        (Element.text str)



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
