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
import HtmlElements
import TokenValue exposing (TokenValue)


type alias Model =
    { uncoiningAmount : TokenValue
    , summonFee : TokenValue
    , devFee : TokenValue
    , initialDeposit : TokenValue
    , uncoinerDeposit : TokenValue
    , preCommitBalance : TokenValue
    , postCommitBalance : TokenValue
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
      , preCommitBalance = TokenValue.empty tokenDecimals
      , postCommitBalance = TokenValue.empty tokenDecimals
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UncoiningAmountChanged newAmtStr ->
            let
                newModel =
                    { model | uncoiningAmount = TokenValue.updateViaString model.uncoiningAmount newAmtStr }
            in
            ( propogateUncoiningAmountChange newModel, Cmd.none )

        SummonFeeChanged newAmtStr ->
            ( { model | summonFee = TokenValue.updateViaString model.summonFee newAmtStr }, Cmd.none )


propogateUncoiningAmountChange : Model -> Model
propogateUncoiningAmountChange model =
    case TokenValue.toBigInt model.uncoiningAmount of
        Nothing ->
            model

        Just uncoiningAmountBigInt ->
            let
                summonFeeBigInt =
                    BigInt.div
                        uncoiningAmountBigInt
                        (BigInt.fromInt 10)

                summonFee =
                    TokenValue.updateViaBigInt model.summonFee summonFeeBigInt

                devFeeBigInt =
                    BigInt.div uncoiningAmountBigInt
                        (BigInt.fromInt 100)

                devFee =
                    TokenValue.updateViaBigInt model.devFee devFeeBigInt

                preCommitBalanceBigInt =
                    BigInt.add uncoiningAmountBigInt summonFeeBigInt

                preCommitBalance =
                    TokenValue.updateViaBigInt model.preCommitBalance preCommitBalanceBigInt

                initialDepositBigInt =
                    BigInt.add preCommitBalanceBigInt devFeeBigInt

                initialDeposit =
                    TokenValue.updateViaBigInt model.initialDeposit initialDepositBigInt
            in
            { model
                | summonFee = summonFee
                , devFee = devFee
                , initialDeposit = initialDeposit
            }


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
                    , text " and forwards an additional "
                    , HtmlElements.tokenValue model.devFee
                    , text " to developers of this tool, "
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
