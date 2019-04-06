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
            Eth.Utils.unsafeToAddress "0x6d540C9f4357FE128c6B3300a12A16B38a5bCB3b"

        Kovan ->
            Eth.Utils.unsafeToAddress "0x84aB93f3189715F1757F2F4356dC97B50960Deb6"