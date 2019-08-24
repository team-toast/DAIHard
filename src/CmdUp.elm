module CmdUp exposing (CmdUp(..), gTag, map, mapList)

import CommonTypes exposing (..)
import Routing
import UserNotice as UN exposing (UserNotice)


type CmdUp msg
    = Web3Connect
    | GotoRoute Routing.Route
    | GTag GTagData
    | UserNotice (UserNotice msg)
    | BrowserNotification String (Maybe String) (Maybe String)
    | RequestBrowserNotificationPermission


gTag : String -> String -> String -> Int -> CmdUp msg
gTag event category label value =
    GTag <|
        GTagData
            event
            category
            label
            value


map : (msg1 -> msg2) -> CmdUp msg1 -> CmdUp msg2
map f cmdUp =
    case cmdUp of
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

        RequestBrowserNotificationPermission ->
            RequestBrowserNotificationPermission


mapList : (msg1 -> msg2) -> List (CmdUp msg1) -> List (CmdUp msg2)
mapList f cmdUps =
    List.map
        (map f)
        cmdUps
