module Create.PMWizard.State exposing (init, update)

import Create.PMWizard.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)


init : Model
init =
    ChooseType


update : Msg -> Model -> UpdateResult
update msg model =
    case msg of
        CloseClicked ->
            UpdateResult
                Nothing
                Cmd.none
                Nothing

        NoOp ->
            justModelUpdate model
