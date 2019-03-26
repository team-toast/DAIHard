module Constants exposing (daiAddress, factoryAddress, tokenDecimals)

import Eth.Types exposing (Address)
import Eth.Utils


daiAddress =
    Eth.Utils.unsafeToAddress "0xC4375B7De8af5a38a93548eb8453a498222C4fF2"


tokenDecimals =
    18


factoryAddress =
    Eth.Utils.unsafeToAddress "0x02b37f71e1B274570fbb5b26f20D8eA1777Fb65a"
