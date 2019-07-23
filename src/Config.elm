module Config exposing (devFeeAddress, factoryAddress, tokenContractAddress, tokenDecimals, tokenUnitName)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    18


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

        Native Rootstock ->
            "SBTC"

        Native RootstockTest ->
            "SBTC"

        Native XDai ->
            "xDai"


tokenContractAddress : TokenFactoryType -> Address
tokenContractAddress tokenFactoryType =
    case tokenFactoryType of
        EthDai ->
            Eth.Utils.unsafeToAddress "0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359"

        KovanDai ->
            Eth.Utils.unsafeToAddress "0xC4375B7De8af5a38a93548eb8453a498222C4fF2"


factoryAddress : FactoryType -> Address
factoryAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x5677CFbA35a0Db0469d3d56020d556B942E9ce90"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0xbC69Aff9d93C5EA4a841166C46C68518D02aF818"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0xD3b1e8F2bDe0a2DdfC9F6e2EB6e2589e5Ba955b6"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xA30773FD520cdf845E1a00441aB09cE39B31F676"

        Native Rootstock ->
            Eth.Utils.unsafeToAddress ""

        Native RootstockTest ->
            Eth.Utils.unsafeToAddress ""

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x7E370099a7a789dC28810a72381bcd7Be834Ad74"


devFeeAddress : Address
devFeeAddress =
    Eth.Utils.unsafeToAddress "0x2b518987f8c21937B4d0b700b1224736a28fCA23"
