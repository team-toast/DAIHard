module Routing exposing (routeToString, urlToRoute)

import Types exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Create (Url.Parser.s "create")
        , Url.Parser.map Interact (Url.Parser.s "interact" <?> Url.Parser.Query.string "id")
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

        Interact maybeString ->
            Url.Builder.absolute [ "interact" ]
                (case maybeString of
                    Nothing ->
                        []

                    Just string ->
                        [ Url.Builder.string "id" string ]
                )

        NotFound ->
            Url.Builder.absolute [] []
