module Create.Types exposing (ContractParameterInputs, Model, Msg(..))

import Contracts.Types
import Eth.Types exposing (Address, TxReceipt)
import TokenValue exposing (TokenValue)


type alias Model =
    { tokenAddress : Address
    , tokenDecimals : Int
    , factoryAddress : Address
    , userAddress : Maybe Address
    , parameterInputs : ContractParameterInputs
    , devFee : TokenValue
    , contractParameters : Maybe Contracts.Types.FullParameters
    , busyWithTxChain : Bool
    }


type alias ContractParameterInputs =
    { uncoiningAmount : String
    , price : String
    , transferMethods : String
    , autorecallInterval : String
    , depositDeadlineInterval : String
    , autoreleaseInterval : String
    }


type Msg
    = UncoiningAmountChanged String
    | PriceChanged String
    | AutorecallIntervalChanged String
    | DepositDeadlineIntervalChanged String
    | AutoreleaseIntervalChanged String
    | TransferMethodsChanged String
    | BeginCreateProcess
    | ApproveMined (Result String TxReceipt)
    | CreateMined (Result String TxReceipt)
    | NoOp
