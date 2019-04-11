module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import Contracts.Types as CTypes
import Eth.Utils
import Marketplace.Types
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)


type Route
    = Home
    | Create
    | Trade Int
    | Marketplace
    | MyTrades
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.s "DAIHard"
        </> Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map Create (Url.Parser.s "create")
                , Url.Parser.map Trade (Url.Parser.s "trade" </> Url.Parser.int)
                , Url.Parser.map Marketplace (Url.Parser.s "marketplace")
                , Url.Parser.map MyTrades (Url.Parser.s "mytrades")
                ]


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

        Marketplace ->
            Url.Builder.absolute [ "DAIHard", "marketplace" ] []

        MyTrades ->
            Url.Builder.absolute [ "DAIHard", "mytrades" ] []

        NotFound ->
            Url.Builder.absolute [] []
