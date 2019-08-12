module TradeTable.Filters.Types exposing (FilterSet, Model, Msg(..), Option, filterTrades, offerType, phases, role)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address)


type alias Model =
    List FilterSet


type Msg
    = SetOption String String Bool


type alias FilterSet =
    { label : String
    , options : List Option
    }


type alias Option =
    { label : String
    , checked : Bool
    , testTrade : CTypes.FullTradeInfo -> Bool
    }


phases : Bool -> Bool -> Bool -> Bool -> FilterSet
phases openChecked committedChecked judgementChecked closedChecked =
    FilterSet
        "Phase"
        [ Option
            "Open"
            openChecked
            (\t -> t.state.phase == CTypes.Open)
        , Option
            "Committed"
            committedChecked
            (\t -> t.state.phase == CTypes.Committed)
        , Option
            "Judgment"
            judgementChecked
            (\t -> t.state.phase == CTypes.Judgment)
        , Option
            "Closed"
            closedChecked
            (\t -> t.state.phase == CTypes.Closed)
        ]


role : Address -> Bool -> Bool -> FilterSet
role addr buyerChecked sellerChecked =
    FilterSet
        "Role"
        [ Option
            "Buyer"
            buyerChecked
            (\t -> CTypes.getBuyerOrSeller t addr == Just Buyer)
        , Option
            "Seller"
            sellerChecked
            (\t -> CTypes.getBuyerOrSeller t addr == Just Seller)
        ]


offerType : Bool -> Bool -> FilterSet
offerType buyingChecked sellingChecked =
    FilterSet
        "Offer Type"
        [ Option
            "Buying"
            buyingChecked
            (\t -> t.parameters.initiatorRole == Buyer)
        , Option
            "Selling"
            sellingChecked
            (\t -> t.parameters.initiatorRole == Seller)
        ]


filterTrades : List FilterSet -> List CTypes.FullTradeInfo -> List CTypes.FullTradeInfo
filterTrades filterSets trades =
    trades
        |> List.filter
            (\trade ->
                filterSets
                    |> List.all
                        (\filterSet ->
                            filterSet.options
                                |> List.any
                                    (\option ->
                                        option.checked && option.testTrade trade
                                    )
                        )
            )
