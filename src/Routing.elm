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
    | Marketplace CTypes.OpenMode
    | MyTrades
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.s "DAIHard"
        </> Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map Create (Url.Parser.s "create")
                , Url.Parser.map Trade (Url.Parser.s "trade" </> Url.Parser.int)
                , Url.Parser.map Marketplace (Url.Parser.s "marketplace" </> openModeParser)
                , Url.Parser.map MyTrades (Url.Parser.s "mytrades")
                ]


openModeParser : Parser (CTypes.OpenMode -> a) a
openModeParser =
    Url.Parser.oneOf
        [ Url.Parser.map CTypes.BuyerOpened (Url.Parser.s "buys")
        , Url.Parser.map CTypes.SellerOpened (Url.Parser.s "sells")
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

        Marketplace openMode ->
            Url.Builder.absolute [ "DAIHard", "marketplace", openModeBuilder openMode ] []

        MyTrades ->
            Url.Builder.absolute [ "DAIHard", "mytrades" ] []

        NotFound ->
            Url.Builder.absolute [] []


openModeBuilder : CTypes.OpenMode -> String
openModeBuilder openMode =
    case openMode of
        CTypes.BuyerOpened ->
            "buys"

        CTypes.SellerOpened ->
            "sells"
