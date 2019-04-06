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
            Eth.Utils.unsafeToAddress "0x4B354bC1cEDd5887a3D7206a1e0b1f1882C446E0"

        Kovan ->
            Eth.Utils.unsafeToAddress "0xbE560578C135322B1415b408B632C7C36A25e572"