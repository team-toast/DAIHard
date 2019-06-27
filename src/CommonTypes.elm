module CommonTypes exposing (BuyerOrSeller(..), EthNetwork(..), InitiatorOrResponder(..), Network(..), UserInfo)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode


type Network
    = Eth EthNetwork
    | XDai
    | Rootstock
    | RootstockTest


type EthNetwork
    = Mainnet
    | Kovan


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
