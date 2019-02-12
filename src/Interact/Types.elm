module Interact.Types exposing (Model, Msg(..), TTSInfo)

import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http
import RenderContract.Types


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userAddress : Maybe Address
    , tokenAddress : Address
    , tokenDecimals : Int
    , addressInput : String
    , ttsInfo : TTSInfo
    }


type alias TTSInfo =
    { address : Maybe Address
    , parameters : Maybe Contracts.Types.FullParameters
    , state : Maybe Contracts.Types.State
    }


type Msg
    = AddressInputChanged String
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Maybe Contracts.Types.FullParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
