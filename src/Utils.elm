module Utils exposing (margin, marginToString)

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
            let
                difference =
                    fiatFloat - tokenFloat
            in
            Just <| difference / tokenFloat

        _ ->
            Nothing


marginToString : Float -> String
marginToString margin_ =
    (margin_
        * 100
        |> String.fromFloat
    )
        ++ "%"
