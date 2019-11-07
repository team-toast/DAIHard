module Currencies exposing (Price, Symbol, charAndImage, compare, cryptoImages, encodePrice, fiatChar, fiatCharsAndImages, fiatList, foreignCryptoList, icon, image, priceDecoder, toString, usd)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import FormatFloat exposing (formatFloat)
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


fiatCharsAndImages : Dict Symbol ( String, Image )
fiatCharsAndImages =
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
    , ( "ZWL", "$" )
    ]
        |> List.map
            (\( typeString, typeChar ) ->
                ( typeString
                , ( typeChar
                  , Images.image
                        { src = "/DAIHard/static/img/currencies/fiat/" ++ typeString ++ ".svg"
                        , description = typeString
                        }
                  )
                )
            )
        |> Dict.fromList


cryptoImages : Dict Symbol Image
cryptoImages =
    (foreignCryptoList
        ++ [ "DAI"
           , "XDAI"
           ]
    )
        |> List.map
            (\symbol ->
                ( symbol
                , Images.image
                    { src = "/DAIHard/static/img/currencies/crypto/" ++ symbol ++ ".png"
                    , description = symbol
                    }
                )
            )
        |> Dict.fromList


fiatList : List Symbol
fiatList =
    Dict.keys fiatCharsAndImages


foreignCryptoList : List Symbol
foreignCryptoList =
    [ "BTC"
    , "ZEC"
    , "XMR"
    , "DASH"
    ]



-- searchPriceTypes : String -> Dict Symbol ( Maybe String, Maybe Image )
-- searchPriceTypes input =
--     charsAndImages
--         |> Dict.filter
--             (\typeString _ ->
--                 String.contains input typeString
--             )


fiatChar : Symbol -> Maybe String
fiatChar symbol =
    Dict.get symbol fiatCharsAndImages
        |> Maybe.map Tuple.first


charAndImage : Symbol -> ( Maybe String, Maybe Image )
charAndImage symbol =
    ( fiatChar symbol
    , image symbol
    )


image : Symbol -> Maybe Image
image symbol =
    if symbol == "DAI" then
        Just Images.daiSymbol

    else
        Maybe.Extra.or
            (Dict.get symbol cryptoImages)
            (Dict.get symbol fiatCharsAndImages
                |> Maybe.map Tuple.second
            )


toString : Price -> String
toString p =
    (fiatChar p.symbol
        |> Maybe.withDefault ""
    )
        ++ (formatFloat 8 p.amount
                ++ " "
                ++ p.symbol
           )


icon : Symbol -> Maybe (Element msg)
icon symbol =
    image symbol
        |> Maybe.map (Images.toElement [ Element.height <| Element.px 26 ])


compare : Price -> Price -> Order
compare p1 p2 =
    if p1.symbol == p2.symbol then
        Basics.compare p1.amount p2.amount

    else
        Basics.compare p1.symbol p2.symbol


encodePrice : Price -> Json.Encode.Value
encodePrice p =
    Json.Encode.list identity
        [ Json.Encode.string p.symbol
        , Json.Encode.float p.amount
        ]


priceDecoder : Json.Decode.Decoder Price
priceDecoder =
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
