module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address)
import Eth.Utils
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)


type Route
    = InitialBlank
    | CreateCrypto
    | CreateFiat
    | Trade FactoryType Int
    | Marketplace
    | AgentHistory Address
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.s "DAIHard"
        </> Url.Parser.oneOf
                [ Url.Parser.map CreateFiat (Url.Parser.s "fiat")
                , Url.Parser.map CreateCrypto Url.Parser.top
                , Url.Parser.map Trade (Url.Parser.s "trade" </> factoryParser </> Url.Parser.int)
                , Url.Parser.map Marketplace (Url.Parser.s "marketplace")
                , Url.Parser.map AgentHistory (Url.Parser.s "history" </> addressParser)
                , Url.Parser.map (\address -> AgentHistory address) (Url.Parser.s "history" </> addressParser)
                ]


addressParser : Parser (Address -> a) a
addressParser =
    Url.Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


factoryParser : Parser (FactoryType -> a) a
factoryParser =
    Url.Parser.custom
        "FACTORY"
        (\s ->
            case s of
                "eth" ->
                    Just <| Native Eth

                "keth" ->
                    Just <| Native Kovan

                "dai" ->
                    Just <| Token EthDai

                "kdai" ->
                    Just <| Token KovanDai

                "xdai" ->
                    Just <| Native XDai

                _ ->
                    Nothing
        )


factoryToString : FactoryType -> String
factoryToString factory =
    case factory of
        Native Eth ->
            "eth"

        Native Kovan ->
            "keth"

        Token EthDai ->
            "dai"

        Token KovanDai ->
            "kdai"

        Native XDai ->
            "xdai"


buyerOrSellerParser : Parser (BuyerOrSeller -> a) a
buyerOrSellerParser =
    Url.Parser.custom
        "BUYERORSELLER"
        (\s ->
            case s of
                "buyer" ->
                    Just Buyer

                "seller" ->
                    Just Seller

                _ ->
                    Nothing
        )


buyerOrSellerToString : BuyerOrSeller -> String
buyerOrSellerToString buyerOrSeller =
    case buyerOrSeller of
        Buyer ->
            "buyer"

        Seller ->
            "seller"


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


routeToString : Route -> String
routeToString route =
    case route of
        InitialBlank ->
            Url.Builder.absolute [ "DAIHard" ] []

        CreateCrypto ->
            Url.Builder.absolute [ "DAIHard" ] []

        CreateFiat ->
            Url.Builder.absolute [ "DAIHard", "create", "fiat" ] []

        Trade factory id ->
            Url.Builder.absolute [ "DAIHard", "trade", factoryToString factory, String.fromInt id ] []

        Marketplace ->
            Url.Builder.absolute [ "DAIHard", "marketplace" ] []

        AgentHistory address ->
            Url.Builder.absolute [ "DAIHard", "history", Eth.Utils.addressToString address ] []

        NotFound ->
            Url.Builder.absolute [] []
