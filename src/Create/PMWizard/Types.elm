module Create.PMWizard.Types exposing (Model(..), Msg(..), UpdateResult, getTitle, justModelUpdate)

import PaymentMethods exposing (PaymentMethod)


type Model
    = ChooseType
    | Details PaymentMethod


type Msg
    = SelectType PaymentMethods.Type
    | ChangeDetails String
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


getTitle : Model -> String
getTitle model =
    case model of
        ChooseType ->
            "Choose Accepted Payment Methods"

        Details paymentMethod ->
            case paymentMethod.type_ of
                PaymentMethods.Cash ->
                    "Cash Drop/Handoff"

                PaymentMethods.Bank ->
                    "Bank Transfer"

                PaymentMethods.Custom ->
                    "Custom Payment Method"
