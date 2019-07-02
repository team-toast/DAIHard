module Config exposing (devFeeAddress, factoryAddress, tokenContractAddress, tokenDecimals)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    18


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
            Eth.Utils.unsafeToAddress "0x41a8a3C08932d285f2AF190c7338ABcC5cFfFBb4"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0x4cd4742b61A840630e3509eC0eDb988edB55673f"

        Native Eth ->
            Eth.Utils.unsafeToAddress ""

        Native Kovan ->
            Eth.Utils.unsafeToAddress ""

        Native Rootstock ->
            Eth.Utils.unsafeToAddress ""

        Native RootstockTest ->
            Eth.Utils.unsafeToAddress ""

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x4078E3f7a5d475Eb615C643ce5729Cc0cC9Fb11D"


devFeeAddress : Address
devFeeAddress =
    Eth.Utils.unsafeToAddress "0x2b518987f8c21937B4d0b700b1224736a28fCA23"
