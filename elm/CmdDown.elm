module CmdDown exposing (CmdDown(..))

import CommonTypes exposing (..)
import Wallet


type CmdDown
    = UpdateWallet Wallet.State
    | CloseAnyDropdownsOrModals
