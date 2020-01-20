module Helpers.BigInt exposing (decoder, encode, toInt, toIntWithWarning, toStringWithCommas)

import BigInt exposing (BigInt)
import Json.Decode
import Json.Encode


toInt : BigInt -> Maybe Int
toInt val =
    let
        converted =
            val
                |> BigInt.toString
                |> String.toInt
    in
    if Maybe.map BigInt.fromInt converted == Just val then
        converted

    else
        Nothing


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


toStringWithCommas : BigInt -> String
toStringWithCommas val =
    addCommasToNumstr (BigInt.toString val)


addCommasToNumstr : String -> String
addCommasToNumstr s =
    if String.length s <= 3 then
        s

    else
        addCommasToNumstr (String.dropRight 3 s) ++ "," ++ String.right 3 s


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
