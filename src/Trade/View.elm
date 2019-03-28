module Trade.View exposing (root)

import Array
import Contracts.Types as CTypes
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Utils
import Time
import Trade.Types exposing (..)


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    Element.none