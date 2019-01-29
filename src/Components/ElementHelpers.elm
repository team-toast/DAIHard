module ElementHelpers exposing (clauseList, fakeLink, initiator, methodName, responder, sectionHeading, sectionReference, smallInput, timeInput, tokenValue, usdValue)

import Element
import Element.Background
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


initiator : List (Element.Attribute a) -> Element.Element a
initiator attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb255 0 255 0) ]) (Element.text "Initiator")


responder : List (Element.Attribute a) -> Element.Element a
responder attributes =
    Element.el (attributes ++ [ Element.Font.color (Element.rgb255 255 153 0) ]) (Element.text "Responder")


methodName : String -> Element.Element a
methodName name =
    Element.el [ Element.Font.family [ Element.Font.monospace ], Element.Background.color (Element.rgb255 230 230 230) ] (Element.text name)


sectionReference : String -> Element.Element a
sectionReference name =
    Element.el [ Element.Font.bold ] (Element.text name)


tokenValue : TokenValue -> Element.Element a
tokenValue tv =
    let
        s =
            Maybe.withDefault "?" (TokenValue.renderToString Nothing tv) ++ " Dai"
    in
    Element.el [ Element.Font.color (Element.rgb255 0 0 255) ] (Element.text s)


usdValue : TokenValue -> Element.Element a
usdValue tv =
    let
        s =
            "US$" ++ Maybe.withDefault "?" (TokenValue.renderToString (Just 2) tv)
    in
    Element.el [ Element.Font.color (Element.rgb255 0 0 255) ] (Element.text s)


fakeLink : String -> Element.Element a
fakeLink name =
    Element.link []
        { url = "#"
        , label = Element.text name
        }


smallInput : String -> String -> (String -> a) -> Element.Element a
smallInput labelStr valueStr msgConstructor =
    Element.Input.text []
        { onChange = msgConstructor
        , text = valueStr
        , placeholder = Nothing
        , label = Element.Input.labelHidden labelStr
        }


clauseList : List (Element.Element a) -> Element.Element a
clauseList clauseElements =
    Element.column [ Element.spacing 30 ] clauseElements


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


sectionHeading : String -> Element.Element a
sectionHeading s =
    Element.el [ Element.Font.size 30, Element.Font.bold ] (Element.text s)
