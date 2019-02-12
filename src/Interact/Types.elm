module Interact.Types exposing (Model, Msg(..), TTSInfo, updateParameters, updateState)

import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http
import RenderContract.Types


updateParameters : TTSInfo -> Maybe Contracts.Types.FullParameters -> TTSInfo
updateParameters ttsInfo parameters =
    { ttsInfo | parameters = parameters }


updateState : TTSInfo -> Maybe Contracts.Types.State -> TTSInfo
updateState ttsInfo state =
    { ttsInfo | state = state }


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
