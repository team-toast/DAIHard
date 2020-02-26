module FormatFloat exposing (autoFormatFloat, formatFloat)

import FormatNumber
import FormatNumber.Humanize exposing (ZeroStrategy(..))
import FormatNumber.Locales exposing (usLocale)


formatFloat : Int -> Float -> String
formatFloat numDecimals =
    FormatNumber.humanize
        { usLocale
            | decimals = numDecimals
        }
        RemoveZeros


autoFormatFloat : Float -> String
autoFormatFloat f =
    let
        magnitude =
            floor <| logBase 10 f + 1

        numDecimals =
            max
                (3 - magnitude)
                0
    in
    formatFloat
        numDecimals
        f
