module Trade.ChatHistory.State exposing (handleNewEvent, init, update)

import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth
import Json.Decode
import Json.Encode
import Maybe.Extra
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.Types exposing (..)


init : UserInfo -> BuyerOrSeller -> BuyerOrSeller -> List ( Int, CTypes.DAIHardEvent ) -> ( Model, Bool )
init userInfo buyerOrSeller initiatingParty initialEvents =
    Model
        userInfo
        buyerOrSeller
        initiatingParty
        Array.empty
        ""
        |> handleInitialEvents initialEvents


update : Msg -> Model -> ( Model, Bool, Maybe String )
update msg prevModel =
    case msg of
        NewEvent ( blocknum, event ) ->
            let
                ( newModel, shouldCallDecrypt ) =
                    handleNewEvent blocknum event prevModel
            in
            ( newModel
            , shouldCallDecrypt
            , Nothing
            )

        MessageInputChanged newMessageStr ->
            ( { prevModel | messageInput = newMessageStr }
            , False
            , Nothing
            )

        MessageSubmit ->
            ( { prevModel | messageInput = "" }
            , False
            , Just prevModel.messageInput
            )

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
                                    ( { prevModel | history = newHistory }
                                    , False
                                    , Nothing
                                    )

                                _ ->
                                    let
                                        _ =
                                            Debug.log "got a decryption result, but for an event that is not a message!" ""
                                    in
                                    ( prevModel, False, Nothing )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "got a decryption result, but for an id out of bounds!" ""
                            in
                            ( prevModel, False, Nothing )

                Err errstr ->
                    let
                        _ =
                            Debug.log "Error decoding decryption result" errstr
                    in
                    ( prevModel, False, Nothing )


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
            CTypes.initiatorOrResponderToBuyerOrSeller prevModel.initiatingParty

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
