module Network exposing (Network(..), daiAddress, factoryAddress, tokenDecimals)

import Eth.Net
import Eth.Types exposing (Address)
import Eth.Utils


tokenDecimals =
    18


type Network
    = Mainnet
    | Kovan


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
            Eth.Utils.unsafeToAddress "0x28d9d0bb434F31eaD818cE7BF84CF75d6a0fb3d8"

        Kovan ->
            Eth.Utils.unsafeToAddress "0x6717Ba6c49ae26Ed49710536933e25D4FAc885ba"
