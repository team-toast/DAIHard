module Utils exposing (marginForBuyer, marginToString)

import FiatValue exposing (FiatValue)
import TokenValue exposing (TokenValue)


marginForBuyer : TokenValue -> FiatValue -> Maybe Float
marginForBuyer tokens fiat =
    let
        tokenFloat =
            TokenValue.getFloatValueWithWarning tokens

        fiatFloat =
            FiatValue.getFloatValueWithWarning fiat
    in
    case fiat.fiatType of
        FiatValue.USD ->
            let
                difference =
                    fiatFloat - tokenFloat
            in
            Just <| difference / tokenFloat


marginToString : Float -> String
marginToString margin =
    (margin
        * 100
        |> String.fromFloat
    )
        ++ "%"
