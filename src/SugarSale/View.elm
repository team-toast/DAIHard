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
    ( Element.column
        [ Element.width Element.fill
        , Element.paddingEach
            { bottom = 40
            , top = 0
            , right = 0
            , left = 0
            }
        ]
        [ EH.simpleSubmodelContainer
            (1200 |> changeForMobile 400 dProfile)
            (Element.column
                [ Element.width Element.fill
                , Element.spacing (20 |> changeForMobile 10 dProfile)
                , Element.padding (20 |> changeForMobile 10 dProfile)
                ]
                [ bucketsRow dProfile model
                , entryUX dProfile model
                ]
            )
        ]
    , []
    )


bucketsRow : DisplayProfile -> Model -> Element Msg
bucketsRow dProfile model =
    Element.text "buckets row"


entryUX : DisplayProfile -> Model -> Element Msg
entryUX dProfile model =
    Element.text "entry UX"
