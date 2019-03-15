module FiatValue exposing (FiatType(..), FiatValue, compare, decoder, encode, encodeFiatType, fiatTypeDecoder, getFloatValueWithWarning, renderToString)

import BigInt exposing (BigInt)
import BigIntHelpers
import Json.Decode
import Json.Encode


type alias FiatValue =
    { fiatType : FiatType
    , amount : BigInt
    }


type FiatType
    = USD


getFloatValueWithWarning : FiatValue -> Float
getFloatValueWithWarning value =
    let
        toFloat =
            value.amount
                |> BigInt.toString
                |> String.toFloat
    in
    case toFloat of
        Just f ->
            f

        Nothing ->
            let
                _ =
                    Debug.log "Error converting FiatValue to float--string -> float failed!" value
            in
            0


renderToString : FiatValue -> String
renderToString fv =
    case fv.fiatType of
        USD ->
            "$" ++ BigInt.toString fv.amount


compare : FiatValue -> FiatValue -> Order
compare f1 f2 =
    BigInt.compare f1.amount f2.amount


encode : FiatValue -> Json.Encode.Value
encode val =
    Json.Encode.list identity
        [ encodeFiatType val.fiatType
        , BigIntHelpers.encode val.amount
        ]


decoder : Json.Decode.Decoder FiatValue
decoder =
    Json.Decode.map2
        FiatValue
        (Json.Decode.index 0 fiatTypeDecoder)
        (Json.Decode.index 1 BigIntHelpers.decoder)


encodeFiatType : FiatType -> Json.Encode.Value
encodeFiatType fiatType =
    case fiatType of
        USD ->
            Json.Encode.string "USD"


fiatTypeDecoder : Json.Decode.Decoder FiatType
fiatTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\typeString ->
                if typeString == "USD" then
                    Json.Decode.succeed USD

                else
                    Json.Decode.fail "unrecognized currency type"
            )
