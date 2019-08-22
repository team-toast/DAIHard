module PriceFetch exposing (PriceAndTimestamp, fetch)

import CommonTypes exposing (ForeignCrypto, foreignCryptoFromName)
import Dict exposing (Dict)
import Http
import Iso8601
import Json.Decode
import Time


type alias PriceAndTimestamp =
    { price : Float
    , timestamp : Time.Posix
    }


fetch : (Result Http.Error (List ( ForeignCrypto, PriceAndTimestamp )) -> msg) -> Cmd msg
fetch msgConstructor =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://18.140.24.194/latest_query.json"
        , body = Http.emptyBody
        , expect = Http.expectJson msgConstructor responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


responseDecoder : Json.Decode.Decoder (List ( ForeignCrypto, PriceAndTimestamp ))
responseDecoder =
    Json.Decode.dict dataToPriceTupleDecoder
        |> Json.Decode.map Dict.values


dataToPriceTupleDecoder : Json.Decode.Decoder ( ForeignCrypto, PriceAndTimestamp )
dataToPriceTupleDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "symbol" foreignCryptoDecoder)
        (Json.Decode.field "quote" <|
            Json.Decode.field "USD" <|
                Json.Decode.map2
                    PriceAndTimestamp
                    (Json.Decode.field "price" Json.Decode.float)
                    (Json.Decode.field "last_updated" Iso8601.decoder)
        )


foreignCryptoDecoder : Json.Decode.Decoder ForeignCrypto
foreignCryptoDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (foreignCryptoFromName
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "")
            )
