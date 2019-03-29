module CommonTypes exposing (BuyerOrSeller(..), InitiatorOrResponder(..), UserInfo)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode
import TokenValue exposing (TokenValue)


type alias UserInfo =
    { address : Address
    , commPubkey : String
    }


type InitiatorOrResponder
    = Initiator
    | Responder


type BuyerOrSeller
    = Buyer
    | Seller
