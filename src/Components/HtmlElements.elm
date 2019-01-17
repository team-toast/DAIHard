module HtmlElements exposing (smallInput, tokenValue)

import Html
import Html.Attributes
import Html.Events
import TokenValue exposing (TokenValue)


smallInput : String -> (String -> a) -> Html.Html a
smallInput valueStr msgConstructor =
    Html.input [ Html.Attributes.type_ "text", Html.Attributes.size 5, Html.Attributes.value valueStr, Html.Events.onInput msgConstructor ] []


tokenValue : TokenValue -> Html.Html a
tokenValue tokenValue =
    Html.span [ Html.Attributes.style [ ( "color", "blue" ) ] ]
        [ Html.text (Maybe.withDefault "?" (TokenValue.renderToString Nothing tokenValue))
        , Html.text " Dai"
        ]
