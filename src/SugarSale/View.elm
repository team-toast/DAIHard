module SugarSale.View exposing (root)

import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FormatFloat exposing (formatFloat)
import Helpers.Element as EH
import Images
import Routing
import SugarSale.Types exposing (..)
import TokenValue exposing (TokenValue)
import Wallet


root : DisplayProfile -> Model -> ( Element Msg, List (Element Msg) )
root dProfile model =
    Debug.todo ""
