module BigIntHelpers exposing (toInt, toIntWithWarning)

import BigInt exposing (BigInt)


toInt : BigInt -> Maybe Int
toInt val =
    val
        |> BigInt.toString
        |> String.toInt


toIntWithWarning : BigInt -> Int
toIntWithWarning val =
    case toInt val of
        Just res ->
            res

        Nothing ->
            let
                _ =
                    Debug.log "Warning! BigInt to Int conversion failed!" <| BigInt.toString val
            in
            0
