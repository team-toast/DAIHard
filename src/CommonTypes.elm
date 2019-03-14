module CommonTypes exposing (FiatType(..), UserInfo, encodeFiatData, encodeFiatType, fiatDataDecoder, fiatTypeDecoder)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { address : Address
    , commPubkey : String
    }


type FiatType
    = USD


encodeFiatData : FiatType -> TokenValue -> Json.Encode.Value
encodeFiatData fiatType amount =
    Json.Encode.list identity
        [ encodeFiatType fiatType
        , TokenValue.encode amount
        ]


fiatDataDecoder : Json.Decode.Decoder ( FiatType, TokenValue )
fiatDataDecoder =
    Json.Decode.map2
        (\fiatType amount -> ( fiatType, amount ))
        (Json.Decode.index 0 fiatTypeDecoder)
        (Json.Decode.index 1 (TokenValue.decoder 2))


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
