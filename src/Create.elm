module Create exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Components exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time)


type alias Model =
    { orderType : OrderType
    , tradeTokenAmount : Maybe TokenAmount
    , summonFee : TokenAmount
    , devFee : TokenAmount
    , totalTokensIn : TokenAmount
    , tradeFiatAmount : Int
    , autorecallInterval : Time
    , depositDeadlineInterval : Time
    , autoreleaseInterval : Time
    , logisticsInfo : String
    }


type OrderType
    = Sell
    | Buy


type Msg
    = OrderTypeChanged OrderType
    | TradeTokenAmountChanged String
    | SummonFeeChanged String
    | TotalTokensInChanged String
    | AutorecallIntervalChanged String
    | DepositDeadlineIntervalChanged String
    | AutoreleaseIntervalChanged String
    | LogisticsInfoChanged String


init : Int -> ( Model, Cmd Msg )
init tokenDecimals =
    ( { orderType = Sell
      , tradeTokenAmount = Nothing
      , summonFee = zeroTokens tokenDecimals
      , devFee = zeroTokens tokenDecimals
      , totalTokensIn = zeroTokens tokenDecimals
      , tradeFiatAmount = 0
      , autorecallInterval = 3 * 24 * Time.hour
      , depositDeadlineInterval = 3 * 24 * Time.hour
      , autoreleaseInterval = 3 * 24 * Time.hour
      , logisticsInfo = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case msg of
    --     OrderTypeChanged newType ->
    --         ( { model | orderType = newType }
    --         , Cmd.none
    --         )
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        headerDiv =
            div [ style [ ( "height", "50px" ) ] ]
                [ text "Create order" ]

        orderTypeInputDiv =
            div []
                (case model.orderType of
                    Sell ->
                        [ text "Order type: Sell"
                        , button [ onClick (OrderTypeChanged Buy) ] [ text "Toggle" ]
                        ]

                    Buy ->
                        [ text "Order type: Buy"
                        , button [ onClick (OrderTypeChanged Sell) ] [ text "Toggle" ]
                        ]
                )

        amountInputsDiv =
            div []
                [ div []
                    [ text
                        (case model.orderType of
                            Sell ->
                                "Uncoining "

                            Buy ->
                                "Coining "
                        )
                    , let
                        tradeTokenAmountInputValue =
                            case model.tradeTokenAmount of
                                Nothing ->
                                    ""

                                Just tradeTokenAmount ->
                                    tradeTokenAmount
                                        |> tokenAmountToString tradeTokenAmount.numDecimals
                      in
                      input [ type_ "text", size 5, placeholder "Amount", value tradeTokenAmountInputValue, onInput TradeTokenAmountChanged ] []
                    , text " Dai"
                    ]
                , div []
                    [ text "offering "
                    , input [ type_ "text", size 5, value (tokenAmountToString model.summonFee.numDecimals model.summonFee), onInput SummonFeeChanged ] []
                    , text " Dai as a summon fee"
                    ]
                , div []
                    [ text "+ "
                    , text (tokenAmountToString model.devFee.numDecimals model.devFee)
                    , text " Dai (1% dev fee)"
                    ]
                ]
    in
    div []
        [ headerDiv
        , orderTypeInputDiv
        , amountInputsDiv
        , div []
            [ text "You will buy "

            --, text model.
            ]
        ]


orderTypeToString : OrderType -> String
orderTypeToString orderType =
    if orderType == Sell then
        "sell"

    else
        "buy"
