module Interact.Types exposing (Model, Msg(..), TTSInfo, updateAddress, updateParameters, updateState)

import BigInt exposing (BigInt)
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http
import RenderContract.Types


updateAddress : TTSInfo -> Maybe Address -> TTSInfo
updateAddress ttsInfo address =
    { ttsInfo | address = address }


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
    , idInput : String
    , ttsInfo : TTSInfo
    }


type alias TTSInfo =
    { id : BigInt
    , address : Maybe Address
    , parameters : Maybe Contracts.Types.FullParameters
    , state : Maybe Contracts.Types.State
    }


type Msg
    = AddressFetched (Result Http.Error Address)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Maybe Contracts.Types.FullParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
