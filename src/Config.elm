module Config exposing (activeFactories, devFeeAddress, factoryAddress, tokenContractAddress, tokenDecimals)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    18


activeFactories : Bool -> List FactoryType
activeFactories testMode =
    if testMode then
        [ Token KovanDai ]

    else
        [ Token EthDai
        , Native XDai
        ]


tokenContractAddress : TokenFactoryType -> Address
tokenContractAddress tokenFactoryType =
    case tokenFactoryType of
        EthDai ->
            Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"

        KovanDai ->
            Eth.Utils.unsafeToAddress "0xB64964e9C0B658Aa7B448cDbDdfCdcCaB26CC584"


factoryAddress : FactoryType -> Address
factoryAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x3BCd42008DF139Bac1b710605bB7b7839d52672B"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0x780cE226c3bB57C61BaeF6e4D1Bab5Ff89c7684D"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x716C806eC6f4703B35a5F613519C607c72c60408"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xE3C549cCE69CB9d19d50C57743b1C5d2D84841e0"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x237CFfc6c21DF0c58a0b4CcD1d5fe3d43818A27B"


devFeeAddress : FactoryType -> Address
devFeeAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x61F399ED1D5AEC3Bc9d4B026352d5764181d6b35"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0xF59ed429f9753B0498436DE1a3559AEC7a0c2a21"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x61F399ED1D5AEC3Bc9d4B026352d5764181d6b35"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xF59ed429f9753B0498436DE1a3559AEC7a0c2a21"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x092110996699c3E06e998d89F0f4586026e44F0F"
