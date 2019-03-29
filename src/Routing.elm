module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import Contracts.Types
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query


type Route
    = Home
    | Create
    | Trade (Maybe Int)
    | Search (Maybe Contracts.Types.OpenMode)
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


typeParser : Url.Parser.Query.Parser (Maybe Contracts.Types.OpenMode)
typeParser =
    Url.Parser.Query.custom "type"
        (\l ->
            case l of
                [] ->
                    Nothing

                s :: [] ->
                    if s == "buys" then
                        Just Contracts.Types.BuyerOpened

                    else if s == "sells" then
                        Just Contracts.Types.SellerOpened

                    else
                        Nothing

                multiple ->
                    Nothing
        )


urlToRoute : Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            Url.Builder.absolute [] []

        Create ->
            Url.Builder.absolute [ "create" ] []

        Trade maybeId ->
            Url.Builder.absolute [ "trade" ]
                (case maybeId of
                    Nothing ->
                        []

                    Just id ->
                        [ Url.Builder.string "id" <| String.fromInt id ]
                )

        Search maybeOpenMode ->
            Url.Builder.absolute [ "search" ] <| buildMaybeOpenModeQueryParameters maybeOpenMode

        NotFound ->
            Url.Builder.absolute [] []


buildMaybeOpenModeQueryParameters : Maybe Contracts.Types.OpenMode -> List Url.Builder.QueryParameter
buildMaybeOpenModeQueryParameters maybeOpenMode =
    case maybeOpenMode of
        Just Contracts.Types.BuyerOpened ->
            [ Url.Builder.string "type" "buys" ]

        Just Contracts.Types.SellerOpened ->
            [ Url.Builder.string "type" "sells" ]

        Nothing ->
            []
