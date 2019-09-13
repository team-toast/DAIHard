module ResponderProfit exposing (calculate)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies
import PriceFetch
import TokenValue


calculate : List ( Currencies.Symbol, PriceFetch.PriceData ) -> CTypes.FullTradeInfo -> Maybe Float
calculate prices trade =
    Maybe.map
        (\foreignCurrencyPrice ->
            let
                tradePriceInDai =
                    trade.terms.price.amount * foreignCurrencyPrice

                tradeAmount =
                    trade.parameters.tradeAmount
                        |> TokenValue.getFloatValueWithWarning

                ( responderCost, responderGain ) =
                    case trade.parameters.initiatorRole of
                        Buyer ->
                            ( tradeAmount, tradePriceInDai )

                        Seller ->
                            ( tradePriceInDai, tradeAmount )
            in
            (responderGain - responderCost) / responderCost
        )
        (PriceFetch.getPriceData trade.terms.price.symbol prices
            |> Maybe.andThen PriceFetch.priceDataToMaybe
        )
