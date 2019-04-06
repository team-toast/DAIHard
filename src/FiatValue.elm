module FiatValue exposing (FiatValue, compare, currencyTypes, decoder, encode, getFloatValueWithWarning, renderToString, renderToStringFull, searchTypes, typeStringToCharStringDefaultEmpty, typeStringToSymbol)

import BigInt exposing (BigInt)
import BigIntHelpers
import Dict exposing (Dict)
import Element exposing (Element)
import Images exposing (Image)
import Json.Decode
import Json.Encode


type alias FiatValue =
    { fiatType : String
    , amount : BigInt
    }


currencyTypes : Dict String ( String, Image )
currencyTypes =
    [ ( "AUD", "$" )
    , ( "CLP", "$" )
    , ( "EUR", "€" )
    , ( "IDR", "Rp" )
    , ( "KRW", "₩" )
    , ( "NZD", "$" )
    , ( "RUB", "₽" )
    , ( "TRY", "₺" )
    , ( "BRL", "R$" )
    , ( "CNY", "¥" )
    , ( "GBP", "£" )
    , ( "ILS", "₪" )
    , ( "MXN", "$" )
    , ( "PHP", "₱" )
    , ( "SEK", "kr" )
    , ( "TWD", "NT$" )
    , ( "CAD", "$" )
    , ( "CZK", "Kč" )
    , ( "HKD", "$" )
    , ( "INR", "₹" )
    , ( "MYR", "RM" )
    , ( "PKR", "₨" )
    , ( "SGD", "$" )
    , ( "USD", "$" )
    , ( "CHF", "Fr" )
    , ( "DKK", "kr" )
    , ( "HUF", "Ft" )
    , ( "JPY", "¥" )
    , ( "NOK", "kr" )
    , ( "PLN", "zł" )
    , ( "THB", "฿" )
    , ( "ZAR", "R" )
    , ( "VND", "₫" )
    ]
        |> List.map
            (\( typeString, typeChar ) ->
                ( typeString
                , ( typeChar
                  , Images.image
                        { src = "/DAIHard/static/img/currencies/" ++ typeString ++ ".svg"
                        , description = typeString
                        }
                  )
                )
            )
        |> Dict.fromList


searchTypes : String -> Dict String ( String, Image )
searchTypes input =
    currencyTypes
        |> Dict.filter
            (\typeString _ ->
                String.contains input typeString
            )


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
    typeStringToCharStringDefaultEmpty fv.fiatType ++ BigIntHelpers.toStringWithCommas fv.amount


renderToStringFull : FiatValue -> String
renderToStringFull fv =
    renderToString fv ++ " " ++ fv.fiatType


typeStringToCharStringDefaultEmpty : String -> String
typeStringToCharStringDefaultEmpty typeString =
    Dict.get typeString currencyTypes
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ""


typeStringToSymbol : String -> Element msg
typeStringToSymbol typeString =
    Dict.get typeString currencyTypes
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Images.qmarkCircle
        |> Images.toElement [ Element.height <| Element.px 26 ]


compare : FiatValue -> FiatValue -> Order
compare f1 f2 =
    BigInt.compare f1.amount f2.amount


encode : FiatValue -> Json.Encode.Value
encode val =
    Json.Encode.list identity
        [ Json.Encode.string val.fiatType
        , BigIntHelpers.encode val.amount
        ]


decoder : Json.Decode.Decoder FiatValue
decoder =
    Json.Decode.map2
        FiatValue
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.index 1 BigIntHelpers.decoder)
