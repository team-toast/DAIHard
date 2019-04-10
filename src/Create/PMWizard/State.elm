module Create.PMWizard.State exposing (init, update)

import Create.PMWizard.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)


init : Model
init =
    Details <| PaymentMethods.blank PaymentMethods.Custom


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        SelectType pmType ->
            justModelUpdate
                (Details <| PaymentMethods.blank pmType)

        ChangeDetails s ->
            case prevModel of
                Details paymentMethod ->
                    justModelUpdate <|
                        Details { paymentMethod | info = s }

                ChooseType ->
                    let
                        _ =
                            Debug.log "Trying to change details, but no type selected!" ""
                    in
                    justModelUpdate prevModel

        SaveAndAddAnother ->
            case prevModel of
                Details paymentMethod ->
                    UpdateResult
                        (Just ChooseType)
                        Cmd.none
                        (Just paymentMethod)

                ChooseType ->
                    let
                        _ =
                            Debug.log "add a payment method, but no type selected!" ""
                    in
                    justModelUpdate prevModel

        Save ->
            case prevModel of
                Details paymentMethod ->
                    UpdateResult
                        Nothing
                        Cmd.none
                        (Just paymentMethod)

                ChooseType ->
                    let
                        _ =
                            Debug.log "add a payment method, but no type selected!" ""
                    in
                    justModelUpdate prevModel

        Back ->
            justModelUpdate ChooseType

        CloseClicked ->
            UpdateResult
                Nothing
                Cmd.none
                Nothing

        NoOp ->
            justModelUpdate prevModel
