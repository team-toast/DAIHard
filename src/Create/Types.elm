module Create.Types exposing (ContractParameterInputs, Model, Msg(..), UpdateResult, justModelUpdate)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Types
import Eth.Types exposing (Address, TxReceipt)
import EthHelpers
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import TokenValue exposing (TokenValue)


type alias Model =
    { ethNode : EthHelpers.EthNode
    , tokenAddress : Address
    , tokenDecimals : Int
    , factoryAddress : Address
    , userInfo : Maybe UserInfo
    , openMode : Contracts.Types.OpenMode
    , parameterInputs : ContractParameterInputs
    , contractParameters : Maybe Contracts.Types.CreateParameters
    , busyWithTxChain : Bool
    }


type alias ContractParameterInputs =
    { openMode : Contracts.Types.OpenMode
    , tradeAmount : String
    , totalPrice : String
    , paymentMethods : List PaymentMethod
    , autorecallInterval : String
    , autoabortInterval : String
    , autoreleaseInterval : String
    }


type Msg
    = TradeAmountChanged String
    | PriceChanged String
    | AutorecallIntervalChanged String
    | AutoabortIntervalChanged String
    | AutoreleaseIntervalChanged String
    | AddPaymentMethod PaymentMethod
    | BeginCreateProcess
    | DevFeeFetched (Result Http.Error BigInt)
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
