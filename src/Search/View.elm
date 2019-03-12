module Search.View exposing (root)

import Element
import Element.Background
import Element.Border
import ElementHelpers as EH
import Search.Types exposing (..)
import Time


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ searchInputElement model
        , EH.hbreak
        , resultsElement model
        ]


searchInputElement : Model -> Element.Element Msg
searchInputElement model =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 100
        , Element.spacing 10
        , Element.padding 15
        ]
        [ Element.none
        ]


resultsElement : Model -> Element.Element Msg
resultsElement model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        Element.none
