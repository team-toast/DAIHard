module CommonTypes exposing (..)
  
import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode

type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }

type FactoryType
    = Native NativeFactoryType
    | Token TokenFactoryType


type NativeFactoryType
    = Eth
    | Kovan
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
