module CommonTypes exposing (BuyerOrSeller(..), InitiatorOrResponder(..), Network(..), UserInfo)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode


type Network
    = Mainnet
    | Kovan
    | XDAI


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
