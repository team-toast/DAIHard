module Config exposing (daiAddress, devFeeAddress, factoryAddress, tokenDecimals)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    18


daiAddress : Network -> Address
daiAddress network =
    case network of
        Mainnet ->
            Eth.Utils.unsafeToAddress "0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359"

        Kovan ->
            Eth.Utils.unsafeToAddress "0xC4375B7De8af5a38a93548eb8453a498222C4fF2"


factoryAddress : Network -> Address
factoryAddress network =
    case network of
        Mainnet ->
            Eth.Utils.unsafeToAddress "0x3833B7233F848350998e7775412d95655545c3Eb"

        Kovan ->
            Eth.Utils.unsafeToAddress "0x886D8E4B29bccaAe4e80f76687E57B3175B9cBB3"


devFeeAddress : Address
devFeeAddress =
    Eth.Utils.unsafeToAddress "0x2b518987f8c21937B4d0b700b1224736a28fCA23"
