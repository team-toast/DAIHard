module CommonTypes exposing (BuyerOrSeller(..), FactoryType(..), InitiatorOrResponder(..), NativeFactoryType(..), TokenFactoryType(..), UserInfo, buyerOrSellerToString)

import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode


type FactoryType
    = Native NativeFactoryType
    | Token TokenFactoryType


type NativeFactoryType
    = Eth
    | Kovan
    | Rootstock
    | RootstockTest
    | XDai


type TokenFactoryType
    = EthDai
    | KovanDai


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


buyerOrSellerToString : BuyerOrSeller -> String
buyerOrSellerToString role =
    case role of
        Buyer ->
            "Buyer"

        Seller ->
            "Seller"
