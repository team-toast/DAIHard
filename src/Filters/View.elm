module Filters.View exposing (view)

import CommonTypes exposing (..)
import Element exposing (Element)
import Element.Font
import Element.Input
import Filters.Types exposing (..)


view : DisplayProfile -> Model -> Element Msg
view dProfile filterSets =
    Element.row
        [ Element.spacing (40 |> changeForMobile 20 dProfile) ]
    <|
        List.map (viewFilterSet dProfile) filterSets


viewFilterSet : DisplayProfile -> FilterSet -> Element Msg
viewFilterSet dProfile filterSet =
    Element.column
        [ Element.spacing (15 |> changeForMobile 8 dProfile)
        , Element.alignTop
        ]
        [ Element.el
            [ Element.Font.size (28 |> changeForMobile 18 dProfile)
            , Element.Font.medium
            ]
            (Element.text <| filterTypeLabel filterSet.type_)
        , Element.column
            [ Element.spacing 5 ]
          <|
            List.map (viewOption dProfile filterSet.type_) filterSet.options
        ]


viewOption : DisplayProfile -> FilterType -> Option -> Element Msg
viewOption dProfile filterType option =
    Element.Input.checkbox
        []
        { onChange = SetOption filterType option.label
        , icon = Element.Input.defaultCheckbox
        , checked = option.checked
        , label =
            Element.Input.labelRight
                [ Element.Font.size (18 |> changeForMobile 12 dProfile) ]
            <|
                Element.text option.label
        }
