module Routing exposing (FullRoute, PageRoute(..), routeToString, urlToRoute)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address)
import Eth.Utils
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)


type alias FullRoute =
    { testing : Bool
    , pageRoute : PageRoute
    }


type PageRoute
    = InitialBlank
    | CreateCrypto
    | CreateFiat
    | Redeploy TradeReference
    | Trade TradeReference
    | Marketplace
    | AgentHistory Address
    | SugarSale
    | NotFound


fullRouteParser : Parser (FullRoute -> a) a
fullRouteParser =
    Url.Parser.oneOf
        [ Url.Parser.s "DAIHard" </> Url.Parser.s "test" </> Url.Parser.map (FullRoute True) pageRouteParser
        , Url.Parser.s "DAIHard" </> Url.Parser.map (FullRoute False) pageRouteParser
        ]


pageRouteParser : Parser (PageRoute -> a) a
pageRouteParser =
    Url.Parser.oneOf
        [ Url.Parser.map CreateFiat Url.Parser.top
        , Url.Parser.map CreateCrypto (Url.Parser.s "create" </> Url.Parser.s "crypto")
        , Url.Parser.map Redeploy (Url.Parser.s "redeploy" </> tradeRefParser)
        , Url.Parser.map Trade (Url.Parser.s "trade" </> tradeRefParser)
        , Url.Parser.map Marketplace (Url.Parser.s "marketplace")
        , Url.Parser.map AgentHistory (Url.Parser.s "history" </> addressParser)
        , Url.Parser.map (\address -> AgentHistory address) (Url.Parser.s "history" </> addressParser)
        , Url.Parser.map SugarSale (Url.Parser.s "sugar")
        ]


routeToString : FullRoute -> String
routeToString fullRoute =
    Url.Builder.absolute
        ((if fullRoute.testing then
            [ "DAIHard", "test" ]

          else
            [ "DAIHard" ]
         )
            ++ (case fullRoute.pageRoute of
                    InitialBlank ->
                        []

                    CreateCrypto ->
                        [ "create", "crypto" ]

                    CreateFiat ->
                        []

                    Redeploy tradeRef ->
                        [ "redeploy", factoryToString tradeRef.factory, String.fromInt tradeRef.id ]

                    Trade tradeRef ->
                        [ "trade", factoryToString tradeRef.factory, String.fromInt tradeRef.id ]

                    Marketplace ->
                        [ "marketplace" ]

                    AgentHistory address ->
                        [ "history", Eth.Utils.addressToString address ]

                    SugarSale ->
                        [ "sugar" ]

                    NotFound ->
                        []
               )
        )
        []


addressParser : Parser (Address -> a) a
addressParser =
    Url.Parser.custom
        "ADDRESS"
        (Eth.Utils.toAddress >> Result.toMaybe)


tradeRefParser : Parser (TradeReference -> a) a
tradeRefParser =
    Url.Parser.map
        TradeReference
        (factoryParser </> Url.Parser.int)


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


urlToRoute : Url -> FullRoute
urlToRoute url =
    Maybe.withDefault (FullRoute False NotFound) (Url.Parser.parse fullRouteParser url)
