module ElementHelpers exposing (black, clauseList, fakeLink, initiator, methodName, pageTitle, responder, sectionHeading, sectionReference, smallInput, timeInput, tokenValue, usdValue)

import Element
import Element.Background
import Element.Font
import Element.Input
import Time
import TokenValue exposing (TokenValue)


black : Element.Color
black =
    Element.rgb255 0 0 0


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


initiator : List (Element.Attribute a) -> Element.Element a
initiator attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb 0 1 0) ]) (Element.text "Initiator")


responder : List (Element.Attribute a) -> Element.Element a
responder attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb 1 0.55 0) ]) (Element.text "Responder")


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


clauseList : List (Element.Element a) -> Element.Element a
clauseList clauseElements =
    Element.column [ Element.spacing 30 ] clauseElements


smallInput : String -> String -> (String -> a) -> Element.Element a
smallInput labelStr valueStr msgConstructor =
    Element.Input.text []
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
