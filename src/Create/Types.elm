module Create.Types exposing (Errors, Inputs, Model, Msg(..), TxChainStatus(..), UpdateResult, justModelUpdate, noErrors)

import AppCmd exposing (AppCmd)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Create.PMWizard.Types as PMWizard
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { web3Context : Web3Context
    , userInfo : Maybe UserInfo
    , inputs : Inputs
    , errors : Errors
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
    | ShowCurrencyDropdown Bool
    | AutorecallIntervalChanged Time.Posix
    | AutoabortIntervalChanged Time.Posix
    | AutoreleaseIntervalChanged Time.Posix
    | ChangePaymentMethodText String
    | ClearDraft
    | CreateClicked UserInfo
    | AbortCreate
    | ConfirmCreate CTypes.CreateParameters BigInt
    | AllowanceFetched (Result Http.Error BigInt)
    | ApproveSigned CTypes.CreateParameters (Result String TxHash)
    | CreateSigned (Result String TxHash)
    | CreateMined (Result String TxReceipt)
    | Web3Connect
    | NoOp


type TxChainStatus
    = Confirm CTypes.CreateParameters
    | ApproveNeedsSig
    | ApproveMining CTypes.CreateParameters TxHash
    | CreateNeedsSig
    | CreateMining TxHash


type alias Inputs =
    { userRole : BuyerOrSeller
    , daiAmount : String
    , fiatType : String
    , fiatAmount : String
    , paymentMethod : String
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias Errors =
    { daiAmount : Maybe String
    , fiatAmount : Maybe String
    , fiatType : Maybe String
    , paymentMethod : Maybe String
    }


noErrors =
    Errors Nothing Nothing Nothing Nothing


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , appCmds : List (AppCmd Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { model = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , appCmds = []
    }
