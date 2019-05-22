module Trade.ChatHistory.Types exposing (CommMessage, EncryptedMessage, Event, EventInfo(..), MessageContent(..), Model, Msg(..), StateChangeInfo(..))

import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address)
import Json.Decode
import Time


type alias Model =
    { userInfo : UserInfo
    , userRole : BuyerOrSeller
    , initiatingParty : BuyerOrSeller
    , history : Array Event
    , messageInput : String
    }


type Msg
    = NewEvent ( Int, CTypes.DAIHardEvent )
    | MessageInputChanged String
    | MessageSubmit
    | DecryptionFinished Json.Decode.Value


type alias Event =
    { eventInfo : EventInfo
    , blocknum : Int
    , time : Maybe Time.Posix
    }


type EventInfo
    = Statement CommMessage
    | StateChange StateChangeInfo


type alias CommMessage =
    { who : BuyerOrSeller
    , message : MessageContent
    , blocknum : Int
    }


type MessageContent
    = FailedDecode
    | Encrypted ( EncryptedMessage, EncryptedMessage )
    | FailedDecrypt
    | Decrypted String


type alias EncryptedMessage =
    { encapsulatedKey : String
    , iv : String
    , tag : String
    , message : String
    }


type StateChangeInfo
    = Initiated
    | Committed Address
    | Recalled
    | Claimed
    | Aborted
    | Released
    | Burned
