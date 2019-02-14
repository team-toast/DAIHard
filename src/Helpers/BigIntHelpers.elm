module BigIntHelpers exposing (toInt)

import BigInt exposing (BigInt)


toInt : BigInt -> Maybe Int
toInt val =
    val
        |> BigInt.toString
        |> String.toInt
