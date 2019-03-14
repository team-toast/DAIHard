module Routing exposing (Route(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query


type Route
    = Home
    | Create
    | Interact (Maybe Int)
    | Search
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Create (Url.Parser.s "create")
        , Url.Parser.map Interact (Url.Parser.s "interact" <?> Url.Parser.Query.int "id")
        , Url.Parser.map Search (Url.Parser.s "search")
        ]


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

        Interact maybeId ->
            Url.Builder.absolute [ "interact" ]
                (case maybeId of
                    Nothing ->
                        []

                    Just id ->
                        [ Url.Builder.string "id" <| String.fromInt id ]
                )

        Search ->
            Url.Builder.absolute [ "search" ] []

        NotFound ->
            Url.Builder.absolute [] []
