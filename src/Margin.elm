module Margin exposing (margin, marginFromFloats, marginToString, stringToMarginFloat)

import Prices exposing (Price)
import TokenValue exposing (TokenValue)


margin : TokenValue -> Price -> Maybe Float
margin dai price =
    case price.symbol of
        "USD" ->
            Just <|
                marginFromFloats
                    (TokenValue.getFloatValueWithWarning dai)
                    price.amount

        _ ->
            Nothing


marginFromFloats : Float -> Float -> Float
marginFromFloats dai fiat =
    (fiat - dai) / dai


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
