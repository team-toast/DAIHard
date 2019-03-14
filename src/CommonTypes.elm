module CommonTypes exposing (UserInfo)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { address : Address
    , commPubkey : String
    }
