module AppCmd exposing (AppCmd(..), GTagData)

import Routing


type alias GTagData =
    { event : String
    , category : String
    , label : String
    , value : Int
    }


type AppCmd
    = Web3Connect
    | GotoRoute Routing.Route
    | GTag GTagData
