module Create.Types exposing (ContractParameterInputs, Model, Msg(..), UpdateResult, justModelUpdate)

import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Types
import Eth.Types exposing (Address, TxReceipt)
import Routing
import TokenValue exposing (TokenValue)


type alias Model =
    { tokenAddress : Address
    , tokenDecimals : Int
    , factoryAddress : Address
    , userInfo : Maybe UserInfo
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
