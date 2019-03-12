module CommonTypes exposing (FiatType(..), UserInfo)

import Eth.Types exposing (Address)


type alias UserInfo =
    { address : Address
    , commPubkey : String
    }


type FiatType
    = USD
