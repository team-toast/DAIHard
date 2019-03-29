module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import Contracts.Types
import Eth.Utils
import Search.Types
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query


type Route
    = Home
    | Create
    | Trade (Maybe Int)
    | Search Search.Types.SearchProfile
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.s "DAIHard"
        </> Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map Create (Url.Parser.s "create")
                , Url.Parser.map Trade (Url.Parser.s "trade" <?> Url.Parser.Query.int "id")
                , Url.Parser.map Search (Url.Parser.s "search" <?> typeParser)
                ]


typeParser : Url.Parser.Query.Parser Search.Types.SearchProfile
typeParser =
    Url.Parser.Query.custom "type"
        (\l ->
            case l of
                [] ->
                    Search.Types.OpenOffers Contracts.Types.BuyerOpened

                s :: [] ->
                    if s == "buys" then
                        Search.Types.OpenOffers Contracts.Types.BuyerOpened

                    else if s == "sells" then
                        Search.Types.OpenOffers Contracts.Types.SellerOpened

                    else
                        Search.Types.AgentHistory <| Eth.Utils.unsafeToAddress "0xc835c3dCfD49Bb7b3E4E90532Db48e270160f946"

                multiple ->
                    Search.Types.OpenOffers Contracts.Types.BuyerOpened
        )


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

        Trade maybeId ->
            Url.Builder.absolute [ "DAIHard", "trade" ]
                (case maybeId of
                    Nothing ->
                        []

                    Just id ->
                        [ Url.Builder.string "id" <| String.fromInt id ]
                )

        Search searchProfile ->
            Url.Builder.absolute [ "DAIHard", "search" ] <| buildSearchQueryParameters searchProfile

        NotFound ->
            Url.Builder.absolute [] []


buildSearchQueryParameters : Search.Types.SearchProfile -> List Url.Builder.QueryParameter
buildSearchQueryParameters searchProfile =
    case searchProfile of
        Search.Types.OpenOffers Contracts.Types.BuyerOpened ->
            [ Url.Builder.string "type" "buys" ]

        Search.Types.OpenOffers Contracts.Types.SellerOpened ->
            [ Url.Builder.string "type" "sells" ]

        _ ->
            []
