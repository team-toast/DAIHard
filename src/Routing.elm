module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address)
import Eth.Utils
import Marketplace.Types
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)


type Route
    = Home
    | Create
    | Trade Int
    | Marketplace BuyerOrSeller
    | AgentHistory Address BuyerOrSeller
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.s "DAIHard"
        </> Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map Create (Url.Parser.s "create")
                , Url.Parser.map Trade (Url.Parser.s "trade" </> Url.Parser.int)
                , Url.Parser.map Marketplace (Url.Parser.s "marketplace" </> buyerOrSellerParser)
                , Url.Parser.map AgentHistory (Url.Parser.s "history" </> addressParser </> buyerOrSellerParser)
                , Url.Parser.map (\address -> AgentHistory address Seller) (Url.Parser.s "history" </> addressParser)
                ]


addressParser : Parser (Address -> a) a
addressParser =
    Url.Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


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
        Home ->
            Url.Builder.absolute [ "DAIHard" ] []

        Create ->
            Url.Builder.absolute [ "DAIHard", "create" ] []

        Trade id ->
            Url.Builder.absolute [ "DAIHard", "trade", String.fromInt id ] []

        Marketplace buyerOrSeller ->
            Url.Builder.absolute [ "DAIHard", "marketplace", buyerOrSellerToString buyerOrSeller ] []

        AgentHistory address buyerOrSeller ->
            Url.Builder.absolute
                [ "DAIHard"
                , "history"
                , Eth.Utils.addressToString address
                , case buyerOrSeller of
                    Buyer ->
                        "buyer"

                    Seller ->
                        "seller"
                ]
                []

        NotFound ->
            Url.Builder.absolute [] []
