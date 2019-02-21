module Interact.View exposing (root)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Interact.Types exposing (..)
import RenderContract.Types
import RenderContract.View
import Time


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    Element.row [ Element.spacing 40, Element.paddingXY 40 0, Element.width Element.fill ]
        [ Element.el [ Element.width (Element.fillPortion 3) ] (maybeContractElement time model)
        , Element.el [ Element.width (Element.fillPortion 2), Element.alignTop ] (commsElement model)
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


commsElement : Model -> Element.Element Msg
commsElement model =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ messagesElement model.messages
        , maybeCommInputElement model
        ]


messagesElement : List CommMessage -> Element.Element Msg
messagesElement messages =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        (messages
            |> List.map renderMessage
        )


renderMessage : CommMessage -> Element.Element Msg
renderMessage message =
    let
        roleDependentAttributes =
            case message.who of
                Initiator ->
                    [ Element.Background.color EH.initiatorBackgroundColor
                    , Element.alignLeft
                    ]

                Responder ->
                    [ Element.Background.color EH.responderBackgroundColor
                    , Element.alignRight
                    ]
    in
    Element.el
        ([ Element.Border.rounded 7
         , Element.Border.width 1
         , Element.Border.color (Element.rgb 0 0 1)
         , Element.padding 7
         ]
            ++ roleDependentAttributes
        )
        (Element.text
            ((case message.who of
                Initiator ->
                    "I: "

                Responder ->
                    "R: "
             )
                ++ message.message
            )
        )


maybeCommInputElement : Model -> Element.Element Msg
maybeCommInputElement model =
    case ( model.userAddress, model.ttsInfo.parameters, model.ttsInfo.state ) of
        ( Just userAddress, Just parameters, Just state ) ->
            Element.text "displayinput"

        _ ->
            Element.none
