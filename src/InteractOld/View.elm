module Interact.View exposing (root)

import Array
import Contracts.Types
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Utils
import Interact.Types exposing (..)
import Time


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    Element.row [ Element.spacing 40, Element.paddingXY 40 0, Element.width Element.fill ]
        [ Element.el [ Element.width (Element.fillPortion 3) ] (maybeContractElement time model)
        , Element.el [ Element.width (Element.fillPortion 2), Element.alignTop ] (historyAndCommsElement model)
        ]


maybeContractElement : Time.Posix -> Model -> Element.Element Msg
maybeContractElement time model =
    case ( model.userInfo, model.trade ) of
        ( Just userInfo, Contracts.Types.LoadedTrade tradeInfo ) ->
            Element.column
                [ Element.spacing 5
                , Element.Background.color EH.white
                ]
                ([ Element.text "Contract rendering currently lobotomized, but here are some buttons!"
                 ]
                    ++ List.map
                        (\( msg, name ) ->
                            Element.Input.button []
                                { onPress = Just <| ContractAction msg
                                , label = Element.text name
                                }
                        )
                        [ ( Commit, "Commit" )
                        , ( Recall, "Recall" )
                        , ( Claim, "Claim" )
                        , ( Abort, "Abort" )
                        , ( Release, "Release" )
                        , ( Burn, "Burn" )
                        , ( Poke, "Poke" )
                        ]
                )

        ( Nothing, _ ) ->
            Element.text "Can't find user address. Is Metamask unlocked?"

        ( _, _ ) ->
            Element.text "Contract state is not yet loaded."


historyAndCommsElement : Model -> Element.Element Msg
historyAndCommsElement model =
    Element.column [ Element.width Element.fill, Element.spacing 20 ]
        [ Element.el [ Element.centerX, Element.Font.size 36 ]
            (Element.text "Chat")
        , Element.column [ Element.width Element.fill, Element.spacing 10, Element.Border.width 1, Element.Border.rounded 5, Element.padding 10 ]
            [ historyElement
                (model.history |> Array.toList |> List.sortBy .blocknum)
            , maybeCommInputElement model
            ]
        ]


historyElement : List Event -> Element.Element Msg
historyElement messages =
    case messages of
        [] ->
            Element.el [ Element.centerX, Element.Font.color (Element.rgb 0.5 0.5 0.5), Element.Font.italic ]
                (Element.text "no messages found.")

        messageList ->
            Element.column [ Element.width Element.fill, Element.spacing 10 ]
                (messageList
                    |> List.map renderEvent
                )


renderEvent : Event -> Element.Element Msg
renderEvent event =
    case event.eventInfo of
        Statement message ->
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
                            ++ (case message.message of
                                    FailedDecode ->
                                        "DECODE FAILED"

                                    Encrypted _ ->
                                        "(encrypted data)"

                                    FailedDecrypt ->
                                        "DECRYPT FAILED"

                                    Decrypted data ->
                                        data
                               )
                        )
                    ]
                )

        StateChange stateChange ->
            let
                maybeElementInfo =
                    case stateChange of
                        Opened ->
                            Just ( Element.rgb 0 0 1, EH.white, "Initiator opened the trade" )

                        Recalled ->
                            Just ( Element.rgb 0 0 1, EH.white, "Initiator recalled the trade" )

                        Committed address ->
                            let
                                addressString =
                                    (Eth.Utils.addressToString address
                                        |> String.left 8
                                    )
                                        ++ ".."
                            in
                            Just ( Element.rgb 1 0 1, EH.white, addressString ++ " committed to the trade" )

                        Aborted ->
                            Just ( Element.rgb 1 0 0, EH.white, "Buyer aborted the trade" )

                        Claimed ->
                            Just ( Element.rgb 0 1 0, EH.white, "Buyer marked the fiat transfer complete" )

                        Released ->
                            Just ( Element.rgb 0 0 1, EH.white, "Seller released the Dai and closed the contract" )

                        Burned ->
                            Just ( Element.rgb 0 0 1, EH.white, "Seller burned the Dai and closed the contract" )
            in
            case maybeElementInfo of
                Nothing ->
                    Element.none

                Just ( bgColor, textColor, text ) ->
                    Element.el
                        [ Element.Border.rounded 3
                        , Element.Border.width 1
                        , Element.Border.color (Element.rgb 1 0 1)
                        , Element.centerX
                        , Element.Background.color bgColor
                        ]
                        (Element.paragraph
                            [ Element.Font.color textColor ]
                            [ Element.text text ]
                        )


maybeCommInputElement : Model -> Element.Element Msg
maybeCommInputElement model =
    case ( model.userInfo, model.trade ) of
        ( Just userInfo, Contracts.Types.LoadedTrade tradeInfo ) ->
            case tradeInfo.state.phase of
                Contracts.Types.Created ->
                    Element.none

                Contracts.Types.Open ->
                    Element.none

                _ ->
                    case getUserRole tradeInfo userInfo.address of
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
