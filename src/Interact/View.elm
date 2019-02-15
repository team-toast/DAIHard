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
