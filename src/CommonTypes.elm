module CommonTypes exposing (BuyerOrSeller(..), FactoryType(..), GTagData, InitiatorOrResponder(..), NativeFactoryType(..), TokenFactoryType(..), UserInfo, buyerOrSellerToString, factoryName, networkNameForFactory, tokenUnitName)

import Eth.Net
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
    { network : Eth.Net.NetworkId
    , address : Address
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


factoryName : FactoryType -> String
factoryName factoryType =
    case factoryType of
        Token EthDai ->
            "Dai"

        Native Eth ->
            "Ether"

        Token KovanDai ->
            "Kovan Dai"

        Native Kovan ->
            "Kovan Ether"

        Native XDai ->
            "xDai"


tokenUnitName : FactoryType -> String
tokenUnitName factoryType =
    case factoryType of
        Token EthDai ->
            "Dai"

        Token KovanDai ->
            "Dai"

        Native Eth ->
            "ETH"

        Native Kovan ->
            "ETH"

        Native XDai ->
            "xDai"


networkNameForFactory : FactoryType -> String
networkNameForFactory factoryType =
    case factoryType of
        Token EthDai ->
            "Ethereum"

        Native Eth ->
            "Ethereum"

        Token KovanDai ->
            "Kovan"

        Native Kovan ->
            "Kovan"

        Native XDai ->
            "xDai"
