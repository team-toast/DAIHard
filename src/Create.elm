module Create exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import BigInt
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import HtmlElements
import Time exposing (Time)
import TokenValue exposing (TokenValue)


type alias Model =
    { uncoiningAmount : TokenValue
    , summonFee : TokenValue
    , devFee : TokenValue
    , initialDeposit : TokenValue
    , uncoinerDeposit : TokenValue
    }


type OrderType
    = Sell
    | Buy


type Msg
    = UncoiningAmountChanged String
    | SummonFeeChanged String


init : Int -> ( Model, Cmd Msg )
init tokenDecimals =
    ( { uncoiningAmount = TokenValue.empty tokenDecimals
      , summonFee = TokenValue.empty tokenDecimals
      , devFee = TokenValue.empty tokenDecimals
      , initialDeposit = TokenValue.empty tokenDecimals
      , uncoinerDeposit = TokenValue.empty tokenDecimals
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UncoiningAmountChanged newAmtStr ->
            let
                newUncoiningAmount =
                    TokenValue.updateViaString model.uncoiningAmount newAmtStr

                newSummonFee =
                    case TokenValue.toBigInt newUncoiningAmount of
                        Nothing ->
                            model.summonFee

                        Just bigIntVal ->
                            TokenValue.updateViaBigInt model.summonFee (Just (BigInt.div bigIntVal (BigInt.fromInt 10)))
            in
            ( { model | uncoiningAmount = newUncoiningAmount, summonFee = newSummonFee }, Cmd.none )

        SummonFeeChanged newAmtStr ->
            ( { model | summonFee = TokenValue.updateViaString model.summonFee newAmtStr }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        header =
            h1 [] [ text "Uncoining Contract - draft" ]

        openerSection =
            div []
                [ i []
                    [ text "(Throughout this contract, you will be referred to as the "
                    , initiatorText
                    , text ".)"
                    ]
                , p []
                    [ text "Uncoining "
                    , HtmlElements.smallInput (TokenValue.getString model.uncoiningAmount) UncoiningAmountChanged
                    , text " Dai, with a summon fee of "
                    , HtmlElements.smallInput (TokenValue.getString model.summonFee) SummonFeeChanged
                    ]
                , p []
                    [ text "Upon contract creation, the "
                    , initiatorText
                    , text " deposits "
                    , HtmlElements.tokenValue model.initialDeposit
                    , text "."
                    ]
                ]
    in
    div []
        [ header
        , openerSection
        ]


initiatorText : Html Msg
initiatorText =
    span [ style [ ( "color", "green" ) ] ] [ text "Initiator" ]


orderTypeToString : OrderType -> String
orderTypeToString orderType =
    if orderType == Sell then
        "sell"

    else
        "buy"
