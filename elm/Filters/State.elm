module Filters.State exposing (init, update)

import CmdUp exposing (CmdUp)
import Filters.Types exposing (..)
import List.Extra


init : List FilterSet -> Model
init =
    identity


update : Msg -> Model -> ( Model, List (CmdUp Msg) )
update msg filterSets =
    case msg of
        SetOption filterType optionLabel checked ->
            ( filterSets
                |> List.Extra.updateIf (.type_ >> (==) filterType)
                    (\filterSet ->
                        { filterSet
                            | options =
                                filterSet.options
                                    |> List.Extra.updateIf (.label >> (==) optionLabel)
                                        (setOption checked)
                        }
                    )
            , [ CmdUp.gTag
                    "filter changed"
                    "input"
                    (filterTypeLabel filterType
                        ++ " - "
                        ++ optionLabel
                    )
                    (if checked then
                        1

                     else
                        0
                    )
              ]
            )


setOption : Bool -> Option -> Option
setOption checked option =
    { option | checked = checked }
