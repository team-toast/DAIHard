module ElementHelpers exposing (black, block, blockBackgroundColor, blockBorderColor, clauseList, fakeLink, initiator, methodName, pageBackgroundColor, pageTitle, responder, roundBottomCorners, roundTopCorners, sectionHeading, sectionReference, smallInput, subpageBackgroundColor, testBorderStyles, timeInput, tokenValue, usdValue)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import List
import Time
import TokenValue exposing (TokenValue)



-- COLORS


black =
    Element.rgb 0 0 0


white =
    Element.rgb 1 1 1


pageBackgroundColor =
    Element.rgb255 10 50 90


subpageBackgroundColor =
    Element.rgb 0.95 0.95 0.95


blockBackgroundColor =
    Element.rgb 1 1 1


blockBorderColor =
    Element.rgb 0.1 0.4 0.7



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
    let
        elementStyles =
            []

        headerStyles =
            [ Element.padding 5
            , Element.width Element.fill
            , Element.Background.color blockBorderColor
            , roundTopCorners 10
            ]

        bodyStyles =
            [ Element.Background.color blockBackgroundColor
            , Element.padding 20
            , roundBottomCorners 10
            , Element.Border.color blockBorderColor
            , Element.Border.width 1
            ]
    in
    Element.column elementStyles
        [ Element.el headerStyles
            (Element.el
                [ Element.Font.color white ]
                (Element.text title)
            )
        , Element.el bodyStyles bodyElement
        ]



-- SPECIAL TERMS


initiator : List (Element.Attribute a) -> Element.Element a
initiator attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb 0 1 0) ]) (Element.text "Initiator")


responder : List (Element.Attribute a) -> Element.Element a
responder attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb 1 0.55 0) ]) (Element.text "Responder")



-- TEXT STYLES


methodName : String -> Element.Element a
methodName name =
    Element.el [ Element.Font.family [ Element.Font.monospace ], Element.Background.color (Element.rgb 0.9 0.9 0.9) ] (Element.text name)


sectionReference : String -> Element.Element a
sectionReference name =
    Element.el [ Element.Font.bold ] (Element.text name)


fakeLink : String -> Element.Element a
fakeLink name =
    Element.link []
        { url = "#"
        , label = Element.text name
        }



-- RENDERERS


tokenValue : TokenValue -> Element.Element a
tokenValue tv =
    let
        s =
            Maybe.withDefault "?" (TokenValue.renderToString Nothing tv) ++ " Dai"
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)


usdValue : TokenValue -> Element.Element a
usdValue tv =
    let
        s =
            "US$" ++ Maybe.withDefault "?" (TokenValue.renderToString (Just 2) tv)
    in
    Element.el [ Element.Font.color (Element.rgb 0 0 1) ] (Element.text s)



-- GROUPINGS


clauseList : List (Element.Element a) -> Element.Element a
clauseList clauseElements =
    Element.column [ Element.spacing 30 ] clauseElements



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
        [ Element.Input.text []
            { onChange = msgConstructor
            , text = value
            , placeholder = Nothing
            , label = Element.Input.labelHidden labelStr
            }
        , Element.text " days"
        ]



-- STYLE HELPESR


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



-- DEBUG


testBorderStyles : List (Element.Attribute msg)
testBorderStyles =
    [ Element.Border.width 1
    , Element.Border.color (Element.rgb 1 0 1)
    ]
