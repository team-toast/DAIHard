module Create.Types exposing (Inputs, Model, Msg(..), TxChainStatus(..), UpdateResult, interpretMarginString, justModelUpdate)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Types as CTypes
import Create.PMWizard.Types as PMWizard
import Eth.Types exposing (Address, TxHash, TxReceipt)
import EthHelpers exposing (EthNode)
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { node : EthNode
    , userInfo : Maybe UserInfo
    , inputs : Inputs
    , showFiatTypeDropdown : Bool
    , addPMModal : Maybe PMWizard.Model
    , createParameters : Maybe CTypes.CreateParameters
    , depositAmount : Maybe BigInt
    , allowance : Maybe BigInt
    , txChainStatus : TxChainStatus
    }


type Msg
    = Refresh Time.Posix
    | ChangeType CTypes.OpenMode
    | TradeAmountChanged String
    | FiatTypeChanged String
    | FiatAmountChanged String
    | FiatTypeLostFocus
    | ShowCurrencyDropdown Bool
    | MarginStringChanged String
    | AutorecallIntervalChanged Time.Posix
    | AutoabortIntervalChanged Time.Posix
    | AutoreleaseIntervalChanged Time.Posix
    | OpenPMWizard
    | ClearDraft
    | BeginCreateProcess
    | AllowanceFetched (Result Http.Error BigInt)
    | ExtraFeesFetched (Result Http.Error DHF.GetExtraFees)
    | ApproveSigned (Result String TxHash)
    | CreateSigned (Result String TxHash)
    | CreateMined (Result String TxReceipt)
    | NoOp
    | PMWizardMsg PMWizard.Msg


type TxChainStatus
    = NoTx
    | FetchingFees
    | ApproveNeedsSig
    | ApproveMining TxHash
    | CreateNeedsSig
    | CreateMining TxHash
    | TxError String


type alias Inputs =
    { openMode : CTypes.OpenMode
    , daiAmount : String
    , fiatType : String
    , fiatAmount : String
    , margin : String
    , paymentMethods : List PaymentMethod
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , newRoute : Maybe Routing.Route
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { model = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , newRoute = Nothing
    }


interpretMarginString : String -> Maybe Float
interpretMarginString =
    String.toFloat >> Maybe.map ((*) 0.01)
