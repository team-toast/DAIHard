module Create.PMWizard.Types exposing (Model(..), Msg(..), UpdateResult, justModelUpdate)

import PaymentMethods exposing (PaymentMethod)


type Model
    = ChooseType
    | Details PaymentMethod


type Msg
    = SelectType PaymentMethods.Type
    | ChangeDetails String
    | SaveAndAddAnother
    | Save
    | Back
    | CloseClicked
    | NoOp


type alias UpdateResult =
    { model : Maybe Model
    , cmd : Cmd Msg
    , newPaymentMethod : Maybe PaymentMethod
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        (Just model)
        Cmd.none
        Nothing
