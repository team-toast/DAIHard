module Trade.ChatHistory.View exposing (window)

import Array exposing (Array)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Eth.Utils
import Helpers.Element as EH
import Trade.ChatHistory.Types exposing (..)
import Wallet


window : Model -> Element Msg
window model =
    Element.el
        [ Element.Background.color EH.white
        , Element.Border.rounded 8
        , EH.subtleShadow
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (historyAndCommsElement model)


historyAndCommsElement : Model -> Element.Element Msg
historyAndCommsElement model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        , Element.Border.width 1
        , Element.Border.rounded 5
        , Element.padding 20
        ]
        [ historyElement
            model.trade.factory
            model.userRole
            (model.history |> Array.toList |> List.sortBy .blocknum)
        , commInputElement model
        ]


historyElement : FactoryType -> BuyerOrSeller -> List Event -> Element.Element Msg
historyElement factoryType userRole messages =
    case messages of
        [] ->
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.rounded 5
                , Element.Border.width 1
                , Element.Border.color EH.black
                , Element.centerX
                , Element.Font.color (Element.rgb 0.5 0.5 0.5)
                , Element.Font.italic
                ]
                (Element.text "no messages found.")

        messageList ->
            EH.scrollbarYEl
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.rounded 5
                , Element.Border.width 1
                , Element.Border.color EH.black
                , Element.padding 10
                ]
            <|
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.spacing 10
                    ]
                    (messageList
                        |> List.map (renderEvent factoryType userRole)
                    )


renderEvent : FactoryType -> BuyerOrSeller -> Event -> Element.Element Msg
renderEvent factoryType userRole event =
    let
        userMessageAttributes =
            [ Element.alignRight
            , Element.Background.color <| EH.blue
            , Element.Font.color <| EH.white
            , Element.Border.roundEach
                { topLeft = 15
                , topRight = 15
                , bottomLeft = 15
                , bottomRight = 0
                }
            ]

        counterpartyMessageAttributes =
            [ Element.alignLeft
            , Element.Background.color <| EH.mediumGray
            , Element.Font.color <| EH.black
            , Element.Border.roundEach
                { topLeft = 15
                , topRight = 15
                , bottomLeft = 0
                , bottomRight = 15
                }
            ]
    in
    case event.eventInfo of
        Statement message ->
            let
                roleDependentAttributes =
                    if message.who == userRole then
                        userMessageAttributes

                    else
                        counterpartyMessageAttributes
            in
            Element.el
                ([ Element.padding 7
                 ]
                    ++ roleDependentAttributes
                )
                (Element.paragraph []
                    [ Element.text
                        ((case message.who of
                            Buyer ->
                                "B: "

                            Seller ->
                                "S: "
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
                        Initiated ->
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
                            Just ( Element.rgb 0 0 1, EH.white, "Seller released the " ++ tokenUnitName factoryType ++ " and closed the contract" )

                        Burned ->
                            Just ( Element.rgb 0 0 1, EH.white, "Seller burned the " ++ tokenUnitName factoryType ++ " and closed the contract" )
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


commInputElement : Model -> Element.Element Msg
commInputElement model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
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
