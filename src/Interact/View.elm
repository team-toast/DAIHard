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
    case ( model.userInfo, model.ttsInfo.parameters, model.ttsInfo.state ) of
        ( Just userInfo, Just parameters, Just state ) ->
            let
                context =
                    { state = state
                    , currentTime = time
                    , userIsInitiator = userInfo.address == parameters.initiatorAddress
                    , userIsResponder =
                        case state.responder of
                            Just responderAddress ->
                                userInfo.address == responderAddress

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
    Element.column [ Element.width Element.fill, Element.spacing 20 ]
        [ Element.el [ Element.centerX, Element.Font.size 36 ]
            (Element.text "Chat")
        , Element.column [ Element.width Element.fill, Element.spacing 10, Element.Border.width 1, Element.Border.rounded 5, Element.padding 10 ]
            [ messagesElement model.messages
            , maybeCommInputElement model
            ]
        ]


messagesElement : List CommMessage -> Element.Element Msg
messagesElement messages =
    case messages of
        [] ->
            Element.el [ Element.centerX, Element.Font.color (Element.rgb 0.5 0.5 0.5), Element.Font.italic ]
                (Element.text "no messages found.")

        messageList ->
            Element.column [ Element.width Element.fill, Element.spacing 10 ]
                (messageList
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
        (Element.paragraph []
            [ Element.text
                ((case message.who of
                    Initiator ->
                        "I: "

                    Responder ->
                        "R: "
                 )
                    ++ message.message
                )
            ]
        )


maybeCommInputElement : Model -> Element.Element Msg
maybeCommInputElement model =
    case ( model.userInfo, model.ttsInfo.parameters, model.ttsInfo.state ) of
        ( Just userInfo, Just parameters, Just state ) ->
            case getUserRole parameters state userInfo.address of
                Just _ ->
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ Element.Input.multiline [ Element.width Element.fill, Element.height (Element.px 100) ]
                            { onChange = MessageInputChanged
                            , text = model.messageInput
                            , placeholder = Nothing
                            , label = Element.Input.labelHidden "messageInput"
                            , spellcheck = False
                            }
                        , Element.Input.button [ Element.centerX, Element.Font.size 24 ]
                            { onPress = Just MessageSubmit
                            , label = Element.text "Submit"
                            }
                        ]

                Nothing ->
                    Element.none

        _ ->
            Element.none
