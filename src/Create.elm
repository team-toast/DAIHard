module Create exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

-- Library
-- Internal

import Components exposing (..)
import Html exposing (..)
import Time exposing (Time)


type alias Model =
    { sellAmount : Maybe TokenAmount
    , summonFee : TokenAmount
    , creatorFee : TokenAmount
    , totalTokensIn : TokenAmount
    , fiatDepositAmount : Int
    , autorecallInterval : Time
    , depositDeadlineInterval : Time
    , autoreleaseInterval : Time
    , logisticsInfo : String
    }


type Msg
    = SellAmount String
    | SummonFee String
    | TotalTokensIn String
    | AutorecallInterval String
    | DepositDeadlineInterval String
    | AutoreleaseInterval String
    | LogisticsInfo String


init : Int -> ( Model, Cmd Msg )
init tokenDecimals =
    ( { sellAmount = Nothing
      , summonFee = zeroTokens tokenDecimals
      , creatorFee = zeroTokens tokenDecimals
      , totalTokensIn = zeroTokens tokenDecimals
      , fiatDepositAmount = 0
      , autorecallInterval = 3 * 24 * Time.hour
      , depositDeadlineInterval = 3 * 24 * Time.hour
      , autoreleaseInterval = 3 * 24 * Time.hour
      , logisticsInfo = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "Sell "

        --, input [ ]
        ]
