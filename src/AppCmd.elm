module AppCmd exposing (AppCmd(..), gTag, map, mapList)

import CommonTypes exposing (..)
import Routing
import UserNotice as UN exposing (UserNotice)


type AppCmd msg
    = Web3Connect
    | GotoRoute Routing.Route
    | GTag GTagData
    | UserNotice (UserNotice msg)
    | BrowserNotification String (Maybe String) (Maybe String)


gTag : String -> String -> String -> Int -> AppCmd msg
gTag event category label value =
    GTag <|
        GTagData
            event
            category
            label
            value


map : (msg1 -> msg2) -> AppCmd msg1 -> AppCmd msg2
map f appCmd =
    case appCmd of
        UserNotice userNotice ->
            UserNotice (userNotice |> UN.map f)

        Web3Connect ->
            Web3Connect

        GotoRoute route ->
            GotoRoute route

        GTag data ->
            GTag data

        BrowserNotification a b c ->
            BrowserNotification a b c


mapList : (msg1 -> msg2) -> List (AppCmd msg1) -> List (AppCmd msg2)
mapList f appCmds =
    List.map
        (map f)
        appCmds
