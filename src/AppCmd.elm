module AppCmd exposing (AppCmd(..))

import Routing


type AppCmd
    = Web3Connect
    | GotoRoute Routing.Route
