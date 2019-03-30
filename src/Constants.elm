module Constants exposing (daiAddress, factoryAddress, tokenDecimals)

import Eth.Types exposing (Address)
import Eth.Utils


daiAddress =
    Eth.Utils.unsafeToAddress "0x89d24A6b4CcB1B6fAA2625fE562bDD9a23260359"


tokenDecimals =
    18


factoryAddress =
    Eth.Utils.unsafeToAddress "0x54382c6528e55b812117c1b54e04d149585e462d"
