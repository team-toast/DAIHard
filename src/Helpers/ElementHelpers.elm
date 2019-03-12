module ElementHelpers exposing (black, block, blockBackgroundColor, blockBorderColor, blockPlusAttributes, bulletPointString, buttonBlue, buttonDeepBlue, buttonGreen, buttonRed, buyer, clauseList, contractActionButton, contractBackgroundColor, contractBorderColor, contractInsetBackgroundColor, contractShadowAttribute, fakeLink, fillWidthBlock, hbreak, headerBackgroundColor, initiator, initiatorBackgroundColor, initiatorColor, methodName, pageBackgroundColor, pageTitle, responder, responderBackgroundColor, responderColor, roundBottomCorners, roundTopCorners, secondsRemainingString, sectionHeading, sectionReference, seller, smallInput, subpageBackgroundColor, testBorderStyles, timeInput, timeValue, tokenValue, usdValue, white)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import List
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)



-- COLORS


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


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
    Element.rgb 0 0.2 0.4


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


pageTitle : String -> Element.Element a
pageTitle s =
    let
        styles =
            [ Element.Font.size 36
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.centerX
            ]
    in
    Element.el styles (Element.text s)


sectionHeading : String -> Element.Element a
sectionHeading s =
    Element.column [ Element.spacing 30 ]
        [ Element.el [ Element.Font.size 30, Element.Font.bold ] (Element.text s)
        , Element.el [] Element.none
        ]


block : String -> Element.Element msg -> Element.Element msg
block title bodyElement =
    blockPlusAttributes title bodyElement []


blockPlusAttributes : String -> Element.Element msg -> List (Element.Attribute msg) -> Element.Element msg
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


fillWidthBlock : String -> Element.Element msg -> Element.Element msg
fillWidthBlock title bodyElement =
    blockPlusAttributes title bodyElement [ Element.width Element.fill ]



-- SPECIAL TERMS


initiator : List (Element.Attribute a) -> Element.Element a
initiator attributes =
    Element.el (attributes ++ [ Element.Font.color initiatorColor ]) (Element.text "Initiator")


responder : List (Element.Attribute a) -> Element.Element a
responder attributes =
    Element.el (attributes ++ [ Element.Font.color responderColor ]) (Element.text "Responder")


buyer : List (Element.Attribute a) -> Element.Element a
buyer attributes =
    Element.el (attributes ++ [ Element.Font.color buyerColor ]) (Element.text "Buyer")


seller : List (Element.Attribute a) -> Element.Element a
seller attributes =
    Element.el (attributes ++ [ Element.Font.color sellerColor ]) (Element.text "Seller")



-- TEXT STYLES


methodName : String -> Element.Element a
methodName name =
    Element.el [ Element.Font.family [ Element.Font.monospace ], Element.Background.color (Element.rgb 0.9 0.9 0.9) ] (Element.text name)


sectionReference : String -> Element.Element a
sectionReference name =
    Element.el [ Element.Font.bold ] (Element.text name)


fakeLink : String -> Element.Element a
fakeLink name =
    Element.link
        [ Element.Font.color (Element.rgb 0 0 1)
        , Element.Font.underline
        ]
        { url = "#"
        , label = Element.text name
        }



-- RENDERERS


tokenValue : TokenValue -> Element.Element a
tokenValue tv =
    let
        s =
            TokenValue.renderToString Nothing tv ++ " Dai"
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)


usdValue : TokenValue -> Element.Element a
usdValue tv =
    let
        s =
            "US$" ++ TokenValue.renderToString (Just 2) tv
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)


timeValue : Time.Posix -> Element.Element a
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



-- GROUPINGS


clauseList : List (Element.Element a) -> Element.Element a
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


smallInput : String -> String -> (String -> a) -> Element.Element a
smallInput labelStr valueStr msgConstructor =
    Element.Input.text [ Element.width (Element.px 100) ]
        { onChange = msgConstructor
        , text = valueStr
        , placeholder = Nothing
        , label = Element.Input.labelHidden labelStr
        }


timeInput : String -> String -> (String -> a) -> Element.Element a
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



-- BUTTONS


contractActionButton : String -> Element.Color -> msg -> Element.Element msg
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


hbreak : Element.Element msg
hbreak =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 1)
        , Element.Background.color defaultHbreakColor
        ]
        Element.none


roundBottomCorners : Int -> Element.Attribute msg
roundBottomCorners r =
    Element.Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = r
        , bottomRight = r
        }


roundTopCorners : Int -> Element.Attribute msg
roundTopCorners r =
    Element.Border.roundEach
        { topLeft = r
        , topRight = r
        , bottomLeft = 0
        , bottomRight = 0
        }


contractShadowAttribute : Element.Attribute msg
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



-- DEBUG


testBorderStyles : List (Element.Attribute msg)
testBorderStyles =
    [ Element.Border.width 1
    , Element.Border.color (Element.rgb 1 0 1)
    ]
