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
            Eth.Utils.unsafeToAddress "0x6aa144c705D74b3D4C7F657025F11dFbF3b577C6"

        Kovan ->
            Eth.Utils.unsafeToAddress "0xc0bf98d82FeC6C68a931304b944f6f6140855565"