module CommonTypes exposing (UserInfo)

import Eth.Types exposing (Address)


type alias UserInfo =
    { address : Address
    , commPubkey : String
    }
