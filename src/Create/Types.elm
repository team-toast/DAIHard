module Create.Types exposing (AmountInputType(..), CurrencyType(..), Errors, Inputs, IntervalUnit(..), MarginButtonType(..), Mode(..), Model, Msg(..), TradeType(..), TxChainStatus(..), UpdateResult, UserInterval, currencySymbol, externalCurrencyPrice, getAmountIn, getAmountOut, getTradeAmount, getTradePrice, getUserInterval, initiatorRole, intervalUnitToString, justModelUpdate, marginButtonTypeToString, maybeBuildPaymentMethods, maybeUserParameters, modeToString, noErrors, tradeType, updateAmountIn, updateAmountOut, updateForeignCurrencyType, updateInType, updateOutType, updateUserInterval, userIntervalToPosix, userIntervalToString)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Tuple as TupleHelpers
import Http
import PaymentMethods exposing (PaymentMethod)
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
    , userAllowance : Maybe BigInt
    , depositAmount : Maybe TokenValue
    , txChainStatus : Maybe TxChainStatus
    }


type TxChainStatus
    = Confirm FactoryType CTypes.CreateParameters
    | ApproveNeedsSig TokenFactoryType
    | ApproveMining TokenFactoryType CTypes.CreateParameters TxHash
    | CreateNeedsSig FactoryType
    | CreateMining FactoryType TxHash


type Msg
    = Refresh
    | UpdateNow Time.Posix
    | PricesFetched (Result Http.Error (List ( Currencies.Symbol, PriceFetch.PriceAndTimestamp )))
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
    | PlaceOrderClicked FactoryType UserInfo CTypes.UserParameters
    | AbortCreate
    | ConfirmCreate FactoryType CTypes.CreateParameters TokenValue
    | AllowanceFetched TokenFactoryType (Result Http.Error BigInt)
    | ApproveSigned TokenFactoryType CTypes.CreateParameters (Result String TxHash)
    | CreateSigned FactoryType (Result String TxHash)
    | CreateMined FactoryType (Result String TxReceipt)
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


modeToString : Mode -> String
modeToString mode =
    case mode of
        CryptoSwap Seller ->
            "CryptoSwap Seller"

        CryptoSwap Buyer ->
            "CryptoSwap Buyer"

        OffRamp ->
            "OffRamp"

        OnRamp ->
            "OnRamp"


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


userIntervalToString : UserInterval -> String
userIntervalToString interval =
    String.fromInt interval.num
        ++ " "
        ++ intervalUnitToString interval.unit
        ++ (if interval.num /= 1 then
                "s"

            else
                ""
           )


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


getAmountIn : Model -> Maybe Float
getAmountIn model =
    case initiatorRole model.mode of
        Buyer ->
            model.foreignCurrencyAmount

        Seller ->
            model.dhTokenAmount


getAmountOut : Model -> Maybe Float
getAmountOut model =
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


marginButtonTypeToString : MarginButtonType -> String
marginButtonTypeToString t =
    case t of
        Loss ->
            "Loss"

        Even ->
            "Even"

        Profit ->
            "Profit"


getUserInterval : IntervalType -> Model -> UserInterval
getUserInterval intervalType model =
    case intervalType of
        Expiry ->
            TupleHelpers.tuple3First model.intervals

        Payment ->
            TupleHelpers.tuple3Second model.intervals

        Judgment ->
            TupleHelpers.tuple3Third model.intervals


maybeUserParameters : Model -> Maybe CTypes.UserParameters
maybeUserParameters model =
    Maybe.map3
        (\tradeAmount price paymentMethods ->
            { initiatorRole = initiatorRole model.mode
            , tradeAmount = tradeAmount
            , price = price
            , paymentMethods = paymentMethods
            , autorecallInterval = userIntervalToPosix (TupleHelpers.tuple3First model.intervals)
            , autoabortInterval = userIntervalToPosix (TupleHelpers.tuple3Second model.intervals)
            , autoreleaseInterval = userIntervalToPosix (TupleHelpers.tuple3Third model.intervals)
            }
        )
        (getTradeAmount model)
        (getTradePrice model)
        (maybeBuildPaymentMethods model)


userIntervalToPosix : UserInterval -> Time.Posix
userIntervalToPosix interval =
    interval.num
        * (case interval.unit of
            Minute ->
                60

            Hour ->
                60 * 60

            Day ->
                60 * 60 * 24

            Week ->
                60 * 60 * 24 * 7
          )
        |> (*) 1000
        |> Time.millisToPosix


getTradeAmount : Model -> Maybe TokenValue
getTradeAmount model =
    model.dhTokenAmount
        |> Maybe.map
            (case initiatorRole model.mode of
                Seller ->
                    \f -> f / 1.01

                Buyer ->
                    identity
            )
        |> Maybe.map TokenValue.fromFloatWithWarning


getTradePrice : Model -> Maybe Currencies.Price
getTradePrice model =
    Maybe.map
        (Currencies.Price model.foreignCurrencyType)
        model.foreignCurrencyAmount


maybeBuildPaymentMethods : Model -> Maybe (List PaymentMethod)
maybeBuildPaymentMethods model =
    case model.mode of
        CryptoSwap Buyer ->
            Just
                [ PaymentMethod
                    PaymentMethods.Custom
                  <|
                    "Provide your "
                        ++ model.foreignCurrencyType
                        ++ " address immediately upon commitment, via chat."
                ]

        CryptoSwap Seller ->
            if model.inputs.receiveAddress == "" then
                Nothing

            else
                Just
                    [ PaymentMethod
                        PaymentMethods.Custom
                        ("Pay to " ++ model.inputs.receiveAddress ++ " immediately upon commitment.")
                    ]

        OffRamp ->
            if model.inputs.paymentMethod == "" then
                Nothing

            else
                Just
                    [ PaymentMethod
                        PaymentMethods.Custom
                        model.inputs.paymentMethod
                    ]

        OnRamp ->
            if model.inputs.paymentMethod == "" then
                Nothing

            else
                Just
                    [ PaymentMethod
                        PaymentMethods.Custom
                        model.inputs.paymentMethod
                    ]
