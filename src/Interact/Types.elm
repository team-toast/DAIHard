module Interact.Types exposing (Model, Msg(..), TTSInfo, updateCreationInfo, updateParameters, updateState)

import BigInt exposing (BigInt)
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http
import RenderContract.Types
import Time


updateCreationInfo : TTSInfo -> Maybe TTF.CreatedSell -> TTSInfo
updateCreationInfo ttsInfo creationInfo =
    { ttsInfo
        | creationInfo = creationInfo
    }


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
    , creationInfo : Maybe TTF.CreatedSell
    , parameters : Maybe Contracts.Types.FullParameters
    , state : Maybe Contracts.Types.State
    }


type Msg
    = CreationInfoFetched (Result Http.Error TTF.CreatedSell)
    | StateFetched (Result Http.Error (Maybe Contracts.Types.State))
    | ParametersFetched (Result Http.Error (Maybe Contracts.Types.FullParameters))
    | ContractAction RenderContract.Types.Msg
    | PreCommitApproveMined (Result String Eth.Types.TxReceipt)
    | ContractActionMined (Result String Eth.Types.TxReceipt)
    | Refresh Time.Posix
    | InitiatorStatementsFetched (Result Http.Error (List (Eth.Types.Event TTS.InitiatorStatementLog)))
    | ResponderStatementsFetched (Result Http.Error (List (Eth.Types.Event TTS.ResponderStatementLog)))
