module Prices exposing (Price, Symbol, char, charsAndImages, compare, decoder, encode, fromForeignCrypto, getIcon, searchPriceTypes, symbolList, toString, usd)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Helpers.BigInt as BigIntHelpers
import Images exposing (Image)
import Json.Decode
import Json.Encode
import Maybe.Extra


type alias Price =
    { symbol : Symbol
    , amount : Float
    }


type alias Symbol =
    String


usd : Float -> Price
usd =
    Price "USD"


charsAndImages : Dict Symbol ( Maybe String, Maybe Image )
charsAndImages =
    [ ( "AUD", Just "$" )
    , ( "CLP", Just "$" )
    , ( "EUR", Just "€" )
    , ( "IDR", Just "Rp" )
    , ( "KRW", Just "₩" )
    , ( "NZD", Just "$" )
    , ( "RUB", Just "₽" )
    , ( "TRY", Just "₺" )
    , ( "BRL", Just "R$" )
    , ( "CNY", Just "¥" )
    , ( "GBP", Just "£" )
    , ( "ILS", Just "₪" )
    , ( "MXN", Just "$" )
    , ( "PHP", Just "₱" )
    , ( "SEK", Just "kr" )
    , ( "TWD", Just "NT$" )
    , ( "CAD", Just "$" )
    , ( "CZK", Just "Kč" )
    , ( "HKD", Just "$" )
    , ( "INR", Just "₹" )
    , ( "MYR", Just "RM" )
    , ( "PKR", Just "₨" )
    , ( "SGD", Just "$" )
    , ( "USD", Just "$" )
    , ( "CHF", Just "Fr" )
    , ( "DKK", Just "kr" )
    , ( "HUF", Just "Ft" )
    , ( "JPY", Just "¥" )
    , ( "NOK", Just "kr" )
    , ( "PLN", Just "zł" )
    , ( "THB", Just "฿" )
    , ( "ZAR", Just "R" )
    , ( "VND", Just "₫" )
    ]
        |> List.map
            (\( typeString, typeChar ) ->
                ( typeString
                , ( typeChar
                  , Just <|
                        Images.image
                            { src = "/DAIHard/static/img/currencies/" ++ typeString ++ ".svg"
                            , description = typeString
                            }
                  )
                )
            )
        |> Dict.fromList


symbolList : List Symbol
symbolList =
    Dict.keys charsAndImages


searchPriceTypes : String -> Dict Symbol ( Maybe String, Maybe Image )
searchPriceTypes input =
    charsAndImages
        |> Dict.filter
            (\typeString _ ->
                String.contains input typeString
            )


char : Symbol -> Maybe String
char symbol =
    Dict.get symbol charsAndImages
        |> Maybe.andThen Tuple.first


toString : Price -> String
toString p =
    let
        numAndSymbol =
            String.fromFloat p.amount
                ++ " "
                ++ p.symbol
    in
    case Dict.get p.symbol charsAndImages of
        Just ( maybeChar, _ ) ->
            (maybeChar
                |> Maybe.withDefault ""
            )
                ++ numAndSymbol

        Nothing ->
            numAndSymbol


getIcon : Symbol -> Maybe (Element msg)
getIcon symbol =
    Dict.get symbol charsAndImages
        |> Maybe.map Tuple.second
        |> Maybe.Extra.join
        |> Maybe.map (Images.toElement [ Element.height <| Element.px 26 ])


compare : Price -> Price -> Order
compare p1 p2 =
    if p1.symbol == p2.symbol then
        Basics.compare p1.amount p2.amount

    else
        Basics.compare p1.symbol p2.symbol


encode : Price -> Json.Encode.Value
encode p =
    Json.Encode.list identity
        [ Json.Encode.string p.symbol
        , Json.Encode.float p.amount
        ]


decoder : Json.Decode.Decoder Price
decoder =
    Json.Decode.oneOf
        [ Json.Decode.map2
            -- For an older encoding
            Price
            (Json.Decode.index 0 Json.Decode.string)
            (Json.Decode.index 1 BigIntHelpers.decoder
                |> Json.Decode.map BigInt.toString
                |> Json.Decode.andThen
                    (\string ->
                        case String.toFloat string of
                            Just f ->
                                Json.Decode.succeed f

                            Nothing ->
                                Json.Decode.fail "Error converting BigInt to float"
                    )
            )
        , Json.Decode.map2
            Price
            (Json.Decode.index 0 Json.Decode.string)
            (Json.Decode.index 1 Json.Decode.float)
        ]


fromForeignCrypto : ForeignCrypto -> Float -> Price
fromForeignCrypto crypto amount =
    Price
        (foreignCryptoName crypto)
        amount
