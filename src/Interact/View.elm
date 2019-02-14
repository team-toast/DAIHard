module Interact.View exposing (root)

import Element
import Element.Font
import Element.Input
import Interact.Types exposing (..)
import RenderContract.Types
import RenderContract.View
import Time


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    Element.column [ Element.spacing 40, Element.width Element.fill ]
        [ maybeContractElement time model
        ]



-- addressInputFormElement : Model -> Element.Element Msg
-- addressInputFormElement model =
--     Element.column [ Element.width Element.fill, Element.spacing 10 ]
--         [ Element.el [ Element.centerX, Element.Font.size 16 ] (Element.text "Uncoining Contract at:")
--         , Element.Input.text [ Element.centerX, Element.width (Element.px 430), Element.Font.size 16 ]
--             { onChange = AddressInputChanged
--             , text = model.addressInput
--             , placeholder = Just (Element.Input.placeholder [] (Element.text "contract address"))
--             , label = Element.Input.labelHidden "address"
--             }
--         ]


maybeContractElement : Time.Posix -> Model -> Element.Element Msg
maybeContractElement time model =
    case ( model.userAddress, model.ttsInfo.parameters, model.ttsInfo.state ) of
        ( Just userAddress, Just parameters, Just state ) ->
            let
                context =
                    { state = state
                    , currentTime = time
                    , userIsInitiator = userAddress == parameters.initiatorAddress
                    , userIsResponder =
                        case state.responder of
                            Just responderAddress ->
                                userAddress == responderAddress

                            Nothing ->
                                False
                    }
            in
            Element.map ContractAction (RenderContract.View.render (RenderContract.Types.Active context) parameters)

        ( Nothing, _, _ ) ->
            Element.text "Can't find user address!"

        ( _, Nothing, _ ) ->
            Element.text "Don't have contract parameters!"

        ( _, _, Nothing ) ->
            Element.text "Don't have contract state!"
