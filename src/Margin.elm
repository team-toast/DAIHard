module Margin exposing (margin, marginFromFloats, marginToString, stringToMarginFloat)

import FiatValue exposing (FiatValue)
import TokenValue exposing (TokenValue)


margin : TokenValue -> FiatValue -> Maybe Float
margin tokens fiat =
    let
        tokenFloat =
            TokenValue.getFloatValueWithWarning tokens

        fiatFloat =
            FiatValue.getFloatValueWithWarning fiat
    in
    case fiat.fiatType of
        "USD" ->
            Just <| marginFromFloats tokenFloat fiatFloat

        _ ->
            Nothing


marginFromFloats : Float -> Float -> Float
marginFromFloats tokens fiat =
    (fiat - tokens) / tokens


stringToMarginFloat : String -> Maybe Float
stringToMarginFloat =
    String.filter (\c -> c /= '%')
        >> String.toFloat
        >> Maybe.map (\f -> f / 100)


marginToString : Float -> String
marginToString margin_ =
    margin_
        * 100
        |> String.fromFloat
        |> (\s ->
                if margin_ > 0 then
                    "+" ++ s

                else
                    s
           )
