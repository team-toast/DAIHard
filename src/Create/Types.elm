module Create.Types exposing (AmountInputType(..), CurrencyType(..), Errors, Inputs, IntervalUnit(..), MarginButtonType(..), Mode(..), Model, Msg(..), TradeType(..), UpdateResult, UserInterval, amountIn, amountOut, currencySymbol, externalCurrencyPrice, getUserInterval, initiatorRole, intervalUnitToString, justModelUpdate, noErrors, tradeType, updateAmountIn, updateAmountOut, updateForeignCurrencyType, updateInType, updateOutType, updateUserInterval)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Currencies
import Helpers.Tuple as TupleHelpers
import Http
import PriceFetch
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , mode : Mode
    , now : Time.Posix
    , prices : List ( Currencies.Symbol, PriceFetch.PriceData )
    , lastAmountInputChanged : AmountInputType
    , inputs : Inputs
    , errors : Errors
    , dhTokenType : FactoryType
    , dhTokenAmount : Maybe Float
    , foreignCurrencyType : Currencies.Symbol
    , foreignCurrencyAmount : Maybe Float
    , margin : Float
    , intervals : ( UserInterval, UserInterval, UserInterval )
    , showInTypeDropdown : Bool
    , showOutTypeDropdown : Bool
    , showMarginModal : Bool
    , showIntervalModal : Maybe IntervalType
    , userAllowance : Maybe TokenValue
    }


type Msg
    = Refresh
    | UpdateNow Time.Posix
    | PricesFetched (Result Http.Error (List ( Currencies.Symbol, PriceFetch.PriceAndTimestamp )))
    | AllowanceFetched TokenFactoryType (Result Http.Error BigInt)
    | ChangeMode Mode
    | SwapClicked
    | AmountInChanged String
    | InTypeClicked
    | InTypeSelected CurrencyType
    | AmountOutChanged String
    | OutTypeClicked
    | OutTypeSelected CurrencyType
    | SearchInputChanged String
    | MarginBoxClicked
    | MarginInputChanged String
    | MarginButtonClicked MarginButtonType
    | ReceiveAddressChanged String
    | PaymentMethodChanged String
    | WindowBoxClicked IntervalType
    | IntervalInputChanged String
    | IntervalUnitChanged IntervalUnit
    | CloseModals
    | NoOp
    | CmdUp (CmdUp Msg)


type alias Inputs =
    { amountIn : String
    , inType : CurrencyType
    , amountOut : String
    , outType : CurrencyType
    , currencySearch : String
    , margin : String
    , marginType : MarginButtonType
    , receiveAddress : String
    , paymentMethod : String
    , interval : String
    }


type AmountInputType
    = AmountIn
    | AmountOut


type alias Errors =
    { amountIn : Maybe String
    , amountOut : Maybe String
    , margin : Maybe String
    , interval : Maybe String
    }


noErrors : Errors
noErrors =
    Errors Nothing Nothing Nothing Nothing


type Mode
    = CryptoSwap BuyerOrSeller
    | OnRamp
    | OffRamp


type CurrencyType
    = DHToken FactoryType
    | External Currencies.Symbol


currencySymbol : CurrencyType -> String
currencySymbol currencyType =
    case currencyType of
        DHToken tokenType ->
            tokenSymbol tokenType

        External symbol ->
            symbol


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { model = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , cmdUps = []
    }


type alias UserInterval =
    { num : Int
    , unit : IntervalUnit
    }


type IntervalUnit
    = Minute
    | Hour
    | Day
    | Week


intervalUnitToString : IntervalUnit -> String
intervalUnitToString intervalUnit =
    case intervalUnit of
        Minute ->
            "Minute"

        Hour ->
            "Hour"

        Day ->
            "Day"

        Week ->
            "Week"


externalCurrencyPrice : Model -> Maybe PriceFetch.PriceData
externalCurrencyPrice model =
    PriceFetch.getPriceData model.foreignCurrencyType model.prices


initiatorRole : Mode -> BuyerOrSeller
initiatorRole mode =
    case mode of
        CryptoSwap i ->
            i

        OffRamp ->
            Seller

        OnRamp ->
            Buyer


amountIn : Model -> Maybe Float
amountIn model =
    case initiatorRole model.mode of
        Buyer ->
            model.foreignCurrencyAmount

        Seller ->
            model.dhTokenAmount


amountOut : Model -> Maybe Float
amountOut model =
    case initiatorRole model.mode of
        Buyer ->
            model.dhTokenAmount

        Seller ->
            model.foreignCurrencyAmount


updateAmountOut : Maybe Float -> Model -> Model
updateAmountOut newMaybeAmount model =
    case initiatorRole model.mode of
        Seller ->
            { model | foreignCurrencyAmount = newMaybeAmount }

        Buyer ->
            { model | dhTokenAmount = newMaybeAmount }


updateAmountIn : Maybe Float -> Model -> Model
updateAmountIn newMaybeAmount model =
    case initiatorRole model.mode of
        Seller ->
            { model | dhTokenAmount = newMaybeAmount }

        Buyer ->
            { model | foreignCurrencyAmount = newMaybeAmount }


updateInType : CurrencyType -> Model -> Model
updateInType newType prevModel =
    case initiatorRole prevModel.mode of
        Seller ->
            case newType of
                DHToken tokenType ->
                    { prevModel | dhTokenType = tokenType }

                _ ->
                    let
                        _ =
                            Debug.log "Trying to update inType with the wrong currency type" ( initiatorRole prevModel.mode, newType )
                    in
                    prevModel

        Buyer ->
            case newType of
                External symbol ->
                    { prevModel | foreignCurrencyType = symbol }

                _ ->
                    let
                        _ =
                            Debug.log "Trying to update inType with the wrong currency type" ( initiatorRole prevModel.mode, newType )
                    in
                    prevModel


updateOutType : CurrencyType -> Model -> Model
updateOutType newType prevModel =
    case initiatorRole prevModel.mode of
        Seller ->
            case newType of
                External symbol ->
                    { prevModel | foreignCurrencyType = symbol }

                _ ->
                    let
                        _ =
                            Debug.log "Trying to update outType with the wrong currency type" ( initiatorRole prevModel.mode, newType )
                    in
                    prevModel

        Buyer ->
            case newType of
                DHToken tokenType ->
                    { prevModel | dhTokenType = tokenType }

                _ ->
                    let
                        _ =
                            Debug.log "Trying to update outType with the wrong currency type" ( initiatorRole prevModel.mode, newType )
                    in
                    prevModel


updateUserInterval : IntervalType -> UserInterval -> Model -> Model
updateUserInterval intervalType interval prevModel =
    let
        mapper =
            case intervalType of
                Expiry ->
                    TupleHelpers.tuple3MapFirst

                Payment ->
                    TupleHelpers.tuple3MapSecond

                Judgment ->
                    TupleHelpers.tuple3MapThird
    in
    { prevModel
        | intervals =
            prevModel.intervals
                |> mapper (always interval)
    }


type TradeType
    = Fiat
    | Crypto


tradeType : Mode -> TradeType
tradeType mode =
    case mode of
        CryptoSwap _ ->
            Crypto

        _ ->
            Fiat


updateForeignCurrencyType : Currencies.Symbol -> Model -> Model
updateForeignCurrencyType symbol prevModel =
    let
        prevInputs =
            prevModel.inputs
    in
    { prevModel
        | foreignCurrencyType = symbol
        , inputs =
            case initiatorRole prevModel.mode of
                Buyer ->
                    { prevInputs | inType = External symbol }

                Seller ->
                    { prevInputs | outType = External symbol }
    }


type MarginButtonType
    = Loss
    | Even
    | Profit


getUserInterval : IntervalType -> Model -> UserInterval
getUserInterval intervalType model =
    case intervalType of
        Expiry ->
            TupleHelpers.tuple3First model.intervals

        Payment ->
            TupleHelpers.tuple3Second model.intervals

        Judgment ->
            TupleHelpers.tuple3Third model.intervals
