module Config exposing (devFeeAddress, factoryAddress, tokenDecimals)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    -- We aren't dealing with an ERC20 token, but we still need this number for similar manipulation of ETH/wei values.
    18


factoryAddress : Network -> Address
factoryAddress network =
    case network of
        Mainnet ->
            Eth.Utils.unsafeToAddress "0x9b92b0DFc385fD406fC72B839F43258AFfBC737E"

        Kovan ->
            Eth.Utils.unsafeToAddress "0xe5696BA01F50a97335cc4Fbc81fC7b13C2Cbdf50"
        
        XDAI ->
            Eth.Utils.unsafeToAddress "0x4078E3f7a5d475Eb615C643ce5729Cc0cC9Fb11D"


devFeeAddress : Address
devFeeAddress =
    Eth.Utils.unsafeToAddress "0x2b518987f8c21937B4d0b700b1224736a28fCA23"
