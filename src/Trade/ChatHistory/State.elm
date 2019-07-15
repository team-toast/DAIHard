module Trade.ChatHistory.State exposing (handleNewEvent, init, update)

import AppCmd exposing (AppCmd)
import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Json.Decode
import Json.Encode
import Maybe.Extra
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.Types exposing (..)
import UserNotice as UN


init : Web3Context -> UserInfo -> BuyerOrSeller -> BuyerOrSeller -> List ( Int, CTypes.DAIHardEvent ) -> ( Model, Bool )
init web3Context userInfo buyerOrSeller initiatorRole initialEvents =
    Model
        web3Context
        userInfo
        buyerOrSeller
        initiatorRole
        Array.empty
        ""
        |> handleInitialEvents initialEvents


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NewEvent ( blocknum, event ) ->
            let
                ( newModel, shouldCallDecrypt ) =
                    handleNewEvent blocknum event prevModel
            in
            UpdateResult
                newModel
                shouldCallDecrypt
                Nothing
                []

        MessageInputChanged newMessageStr ->
            UpdateResult
                { prevModel | messageInput = newMessageStr }
                False
                Nothing
                []

        MessageSubmit ->
            UpdateResult
                { prevModel | messageInput = "" }
                False
                (Just prevModel.messageInput)
                []

        DecryptionFinished decryptedMessageValue ->
            case decodeDecryptionResult decryptedMessageValue of
                Ok ( id, message ) ->
                    case Array.get id prevModel.history of
                        Just historyEvent ->
                            case historyEvent.eventInfo of
                                Statement commMessage ->
                                    let
                                        newCommMessage =
                                            { commMessage
                                                | message = Decrypted message
                                            }

                                        newHistoryEvent =
                                            { historyEvent
                                                | eventInfo = Statement newCommMessage
                                            }

                                        newHistory =
                                            Array.set id newHistoryEvent prevModel.history
                                    in
                                    UpdateResult
                                        { prevModel | history = newHistory }
                                        False
                                        Nothing
                                        []

                                _ ->
                                    UpdateResult
                                        prevModel
                                        False
                                        Nothing
                                        [ AppCmd.UserNotice <|
                                            UN.unexpectedError "got a decryption result, but for an event that is not a message!" historyEvent
                                        ]

                        Nothing ->
                            UpdateResult
                                prevModel
                                False
                                Nothing
                                [ AppCmd.UserNotice <|
                                    UN.unexpectedError "got a decryption result, but for an id out of bounds!" ( id, prevModel.history )
                                ]

                Err s ->
                    UpdateResult
                        prevModel
                        False
                        Nothing
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Error decoding decryption result" s
                        ]


handleInitialEvents : List ( Int, CTypes.DAIHardEvent ) -> Model -> ( Model, Bool )
handleInitialEvents initialEvents prevModel =
    let
        helper : List ( Int, CTypes.DAIHardEvent ) -> ( Model, Bool ) -> ( Model, Bool )
        helper events ( model, shouldDecrypt ) =
            case events of
                [] ->
                    ( model, shouldDecrypt )

                ( blocknum, event ) :: remainingEvents ->
                    let
                        ( thisModel, thisShouldDecrypt ) =
                            handleNewEvent blocknum event model
                    in
                    helper remainingEvents ( thisModel, shouldDecrypt || thisShouldDecrypt )
    in
    helper initialEvents ( prevModel, False )


handleNewEvent : Int -> CTypes.DAIHardEvent -> Model -> ( Model, Bool )
handleNewEvent blocknum event prevModel =
    let
        toBuyerOrSeller =
            CTypes.initiatorOrResponderToBuyerOrSeller prevModel.initiatorRole

        maybeHistoryEventInfo =
            case event of
                CTypes.InitiatedEvent _ ->
                    Just <| StateChange Initiated

                CTypes.CommittedEvent data ->
                    Just <| StateChange (Committed data.responder)

                CTypes.RecalledEvent ->
                    Just <| StateChange Recalled

                CTypes.ClaimedEvent ->
                    Just <| StateChange Claimed

                CTypes.AbortedEvent ->
                    Just <| StateChange Aborted

                CTypes.ReleasedEvent ->
                    Just <| StateChange Released

                CTypes.BurnedEvent ->
                    Just <| StateChange Burned

                CTypes.InitiatorStatementLogEvent data ->
                    Just <|
                        Statement <|
                            { who = Initiator |> toBuyerOrSeller
                            , message =
                                case decodeEncryptedMessages data.statement of
                                    Just decodedMessages ->
                                        Encrypted decodedMessages

                                    _ ->
                                        FailedDecode
                            , blocknum = blocknum
                            }

                CTypes.ResponderStatementLogEvent data ->
                    Just <|
                        Statement <|
                            { who = Responder |> toBuyerOrSeller
                            , message =
                                case decodeEncryptedMessages data.statement of
                                    Just decodedMessages ->
                                        Encrypted decodedMessages

                                    _ ->
                                        FailedDecode
                            , blocknum = blocknum
                            }

                CTypes.PokeEvent ->
                    Nothing

        maybeNewEvent =
            Maybe.map
                (\historyEventInfo ->
                    { eventInfo = historyEventInfo
                    , blocknum = blocknum
                    , time = Nothing
                    }
                )
                maybeHistoryEventInfo

        newHistory =
            Array.append
                prevModel.history
                (Array.fromList <|
                    Maybe.Extra.values [ maybeNewEvent ]
                )

        newModel =
            { prevModel | history = newHistory }
    in
    ( newModel
    , case maybeHistoryEventInfo of
        Just (Statement _) ->
            True

        _ ->
            False
    )
