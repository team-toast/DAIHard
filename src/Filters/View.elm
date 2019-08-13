module Filters.View exposing (view)

import Element exposing (Element)
import Element.Font
import Element.Input
import Filters.Types exposing (..)


view : Model -> Element Msg
view filterSets =
    Element.row
        [ Element.spacing 20 ]
    <|
        List.map viewFilterSet filterSets


viewFilterSet : FilterSet -> Element Msg
viewFilterSet filterSet =
    Element.column
        [ Element.spacing 15
        , Element.alignTop
        ]
        [ Element.el
            [ Element.Font.size 28
            , Element.Font.medium
            ]
            (Element.text filterSet.label)
        , Element.column
            [ Element.spacing 5 ]
          <|
            List.map (viewOption filterSet.label) filterSet.options
        ]


viewOption : String -> Option -> Element Msg
viewOption filterSetLabel option =
    Element.Input.checkbox
        []
        { onChange = SetOption filterSetLabel option.label
        , icon = Element.Input.defaultCheckbox
        , checked = option.checked
        , label = Element.Input.labelRight [] <| Element.text option.label
        }
