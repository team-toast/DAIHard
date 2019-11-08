module CommonTypes exposing (BuyerOrSeller(..), DisplayProfile(..), FactoryType(..), GTagData, InitiatorOrResponder(..), IntervalType(..), NativeFactoryType(..), TokenFactoryType(..), TradeReference, UserInfo, buyerOrSellerToString, changeForMobile, dhTokenList, factoryName, intervalTypeToString, networkNameForFactory, screenWidthToDisplayProfile, tokenSymbol, tokenUnitName)

import Dict
import Eth.Net
import Eth.Types exposing (Address)
import Json.Decode
import Json.Encode


type DisplayProfile
    = Desktop
    | Mobile


type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }


type FactoryType
    = Native NativeFactoryType
    | Token TokenFactoryType


type alias TradeReference =
    { factory : FactoryType
    , id : Int
    }


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


type IntervalType
    = Expiry
    | Payment
    | Judgment


screenWidthToDisplayProfile : Int -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile


changeForMobile : a -> DisplayProfile -> a -> a
changeForMobile changed dProfile original =
    case dProfile of
        Desktop ->
            original

        Mobile ->
            changed


intervalTypeToString : IntervalType -> String
intervalTypeToString intervalType =
    case intervalType of
        Expiry ->
            "Expiry"

        Payment ->
            "Payment"

        Judgment ->
            "Judgment"


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
            "(k)Dai"

        Native Eth ->
            "ETH"

        Native Kovan ->
            "(k)ETH"

        Native XDai ->
            "xDai"


tokenSymbol : FactoryType -> String
tokenSymbol factoryType =
    case factoryType of
        Token EthDai ->
            "DAI"

        Token KovanDai ->
            "DAI"

        Native Eth ->
            "ETH"

        Native Kovan ->
            "ETH"

        Native XDai ->
            "XDAI"


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


dhTokenList : List FactoryType
dhTokenList =
    [ Native XDai
    , Token EthDai
    ]
