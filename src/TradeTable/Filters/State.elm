module TradeTable.Filters.State exposing (init, update)

import List.Extra
import TradeTable.Filters.Types exposing (..)


init : List FilterSet -> Model
init =
    identity


update : Msg -> Model -> Model
update msg filterSets =
    case msg of
        SetOption filterSetLabel optionLabel checked ->
            filterSets
                |> List.Extra.updateIf (.label >> (==) filterSetLabel)
                    (\filterSet ->
                        { filterSet
                            | options =
                                filterSet.options
                                    |> List.Extra.updateIf (.label >> (==) optionLabel)
                                        (setOption checked)
                        }
                    )


setOption : Bool -> Option -> Option
setOption checked option =
    { option | checked = checked }
