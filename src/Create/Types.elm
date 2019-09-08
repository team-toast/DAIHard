module Create.Types exposing (CurrencyType(..), Errors, Inputs, IntervalUnit(..), Mode(..), Model, Msg(..), UpdateResult, UserInterval, currencySymbol, intervalUnitToString, justModelUpdate, noErrors)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Currencies
import Http
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , mode : Mode
    , inputs : Inputs
    , errors : Errors
    , margin : Float
    , showInTypeDropdown : Bool
    , showOutTypeDropdown : Bool
    , showMarginModal : Bool
    , showIntervalModals : ( Bool, Bool, Bool )
    , userAllowance : Maybe TokenValue
    }


type Msg
    = Refresh
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
    | MarginLossClicked
    | MarginEvenClicked
    | MarginProfitClicked
    | ReceiveAddressChanged String
    | PaymentMethodChanged String
    | ExpiryWindowBoxClicked
    | PaymentWindowBoxClicked
    | BurnWindowBoxClicked
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
    , receiveAddress : String
    , paymentMethod : String
    , intervals : ( UserInterval, UserInterval, UserInterval )
    }


type alias Errors =
    { amountIn : Maybe String
    , amountOut : Maybe String
    , margin : Maybe String
    }


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
            tokenUnitName tokenType

        External symbol ->
            symbol


noErrors : Errors
noErrors =
    Errors Nothing Nothing Nothing


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
