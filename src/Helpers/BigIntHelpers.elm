module BigIntHelpers exposing (decoder, encode, toInt, toIntWithWarning)

import BigInt exposing (BigInt)
import Json.Decode
import Json.Encode


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


encode : BigInt -> Json.Encode.Value
encode val =
    val
        |> BigInt.toString
        |> Json.Encode.string


decoder : Json.Decode.Decoder BigInt
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case BigInt.fromString string of
                    Just val ->
                        Json.Decode.succeed val

                    Nothing ->
                        Json.Decode.fail "Can't convert that to a BigInt"
            )
