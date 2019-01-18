module HtmlElements exposing (fakeLink, initiator, methodName, responder, sectionReference, smallInput, timeInput, tokenValue, usdValue)

import Html
import Html.Attributes
import Html.Events
import TokenValue exposing (TokenValue)


initiator : Html.Html a
initiator =
    Html.span [ Html.Attributes.style [ ( "color", "green" ) ] ] [ Html.text "Initiator" ]


responder : Html.Html a
responder =
    Html.span [ Html.Attributes.style [ ( "color", "rgb(255, 153, 0)" ) ] ] [ Html.text "Responder" ]


methodName : String -> Html.Html a
methodName name =
    Html.code [ Html.Attributes.style [ ( "background-color", "rgb(230,230,230)" ) ] ] [ Html.text name ]


sectionReference : String -> Html.Html a
sectionReference name =
    Html.b [] [ Html.text name ]


tokenValue : TokenValue -> Html.Html a
tokenValue tokenValue =
    Html.span [ Html.Attributes.style [ ( "color", "blue" ) ] ]
        [ Html.text (Maybe.withDefault "?" (TokenValue.renderToString Nothing tokenValue))
        , Html.text " Dai"
        ]


usdValue : TokenValue -> Html.Html a
usdValue tokenValue =
    Html.span [ Html.Attributes.style [ ( "color", "blue" ) ] ]
        [ Html.text "US$"
        , Html.text (Maybe.withDefault "?" (TokenValue.renderToString (Just 2) tokenValue))
        ]


fakeLink : String -> Html.Html a
fakeLink name =
    Html.a [ Html.Attributes.href "#" ] [ Html.text name ]


smallInput : String -> (String -> a) -> Html.Html a
smallInput valueStr msgConstructor =
    Html.input [ Html.Attributes.type_ "text", Html.Attributes.size 5, Html.Attributes.value valueStr, Html.Events.onInput msgConstructor ] []


timeInput : Html.Html a
timeInput =
    Html.span []
        [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.size 1, Html.Attributes.value "3" ] []
        , Html.text " days"
        ]
