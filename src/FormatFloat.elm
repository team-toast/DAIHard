module FormatFloat exposing (formatFloat)

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
