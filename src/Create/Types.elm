module Create.Types exposing (Errors, Inputs, Model, Msg(..), TxChainStatus(..), UpdateResult, justModelUpdate, noErrors)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Create.PMWizard.Types as PMWizard
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.Eth as EthHelpers
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , inputs : Inputs
    , errors : Errors
    , showDhTypeDropdown : Bool
    , showFiatTypeDropdown : Bool
    , createParameters : Maybe CTypes.CreateParameters
    , depositAmount : Maybe BigInt
    , allowance : Maybe BigInt
    , txChainStatus : Maybe TxChainStatus
    }


type Msg
    = Refresh Time.Posix
    | ChangeRole BuyerOrSeller
    | TradeAmountChanged String
    | FiatTypeChanged String
    | FiatAmountChanged String
    | FiatTypeLostFocus
    | DhDropdownClicked
    | DhTypeChanged FactoryType
    | ShowCurrencyDropdown Bool
    | AutorecallIntervalChanged Time.Posix
    | AutoabortIntervalChanged Time.Posix
    | AutoreleaseIntervalChanged Time.Posix
    | ChangePaymentMethodText String
    | CreateClicked FactoryType UserInfo
    | AbortCreate
    | ConfirmCreate FactoryType CTypes.CreateParameters BigInt
    | AllowanceFetched TokenFactoryType (Result Http.Error BigInt)
    | ApproveSigned TokenFactoryType CTypes.CreateParameters (Result String TxHash)
    | CreateSigned FactoryType (Result String TxHash)
    | CreateMined FactoryType (Result String TxReceipt)
    | Web3Connect
    | NoOp
    | CmdUp (CmdUp Msg)


type TxChainStatus
    = Confirm FactoryType CTypes.CreateParameters
    | ApproveNeedsSig TokenFactoryType
    | ApproveMining TokenFactoryType CTypes.CreateParameters TxHash
    | CreateNeedsSig FactoryType
    | CreateMining FactoryType TxHash


type alias Inputs =
    { userRole : BuyerOrSeller
    , dhToken : FactoryType
    , daiAmount : String
    , fiatType : String
    , fiatAmount : String
    , paymentMethod : String
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias Errors =
    { dhToken : Maybe String
    , daiAmount : Maybe String
    , fiatAmount : Maybe String
    , fiatType : Maybe String
    , paymentMethod : Maybe String
    , autorecallInterval : Maybe String
    , autoabortInterval : Maybe String
    , autoreleaseInterval : Maybe String
    }


noErrors : Errors
noErrors =
    Errors Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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
