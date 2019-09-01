module ResponderProfit exposing (calculate)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import PriceFetch
import TokenValue


calculate : List ( ForeignCrypto, PriceFetch.PriceData ) -> CTypes.FullTradeInfo -> Maybe Float
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
        (foreignCryptoFromName trade.terms.price.symbol
            |> Maybe.andThen (\crypto -> PriceFetch.getPriceData crypto prices)
            |> Maybe.andThen PriceFetch.priceDataToMaybe
        )
