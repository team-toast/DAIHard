module Create.Types exposing (Inputs, Model, Msg(..), UpdateResult, interpretMarginString, justModelUpdate)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Types as CTypes
import Eth.Types exposing (Address, TxReceipt)
import EthHelpers exposing (EthNode)
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import TokenValue exposing (TokenValue)


type alias Model =
    { node : EthNode
    , userInfo : Maybe UserInfo
    , inputs : Inputs
    , showFiatTypeDropdown : Bool
    , createParameters : Maybe CTypes.CreateParameters
    , busyWithTxChain : Bool
    }


type alias Inputs =
    { openMode : CTypes.OpenMode
    , daiAmount : String
    , fiatType : String
    , fiatAmount : String
    , margin : String
    , paymentMethods : List PaymentMethod
    , autorecallInterval : String
    , autoabortInterval : String
    , autoreleaseInterval : String
    }


type Msg
    = ChangeType CTypes.OpenMode
    | TradeAmountChanged String
    | FiatTypeChanged String
    | FiatAmountChanged String
    | FiatTypeArrowClicked
    | FiatTypeLostFocus
    | ShowCurrencyDropdown Bool
    | OpenCurrencySelector
    | MarginStringChanged String
    | AddPaymentMethod PaymentMethod
    | BeginCreateProcess
    | ExtraFeesFetched (Result Http.Error DHF.GetExtraFees)
    | ApproveMined (Result String TxReceipt)
    | CreateMined (Result String TxReceipt)
    | NoOp


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
