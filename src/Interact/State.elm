port module Interact.State exposing (decodeSizedStringHelper, encodeSizedStrings, init, subscriptions, update, updateUserInfo)

import Array
import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address)
import EthHelpers
import EventSentryHack exposing (EventSentry)
import Interact.Types exposing (..)
import Json.Decode
import Json.Encode
import Maybe.Extra
import RenderContract.Types
import Result.Extra
import Time
import TokenValue


init : EthHelpers.EthNode -> Address -> Address -> Int -> Maybe UserInfo -> BigInt -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode factoryAddress tokenAddress tokenDecimals userInfo ttsId =
    let
        cmd =
            getContractCreationInfoCmd ethNode factoryAddress ttsId
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , tokenAddress = tokenAddress
      , tokenDecimals = tokenDecimals
      , ttsId = ttsId
      , ttsInfo = NothingLoaded
      , messages = Array.empty
      , messageInput = ""
      , eventSentries = Nothing
      }
    , cmd
    , ChainCmd.none
    )


getContractCreationInfoCmd : EthHelpers.EthNode -> Address -> BigInt -> Cmd Msg
getContractCreationInfoCmd ethNode factoryAddress id =
    Contracts.Wrappers.getCreationInfoFromIdCmd ethNode factoryAddress id CreationInfoFetched


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        Refresh time ->
            case ( model.ttsInfo, model.eventSentries ) of
                ( Loaded ttsInfo, Just sentries ) ->
                    ( model
                    , Cmd.batch
                        [ Contracts.Wrappers.getStateCmd model.ethNode model.tokenDecimals ttsInfo.creationInfo.address StateFetched
                        , getSentryPollCmd sentries
                        ]
                    , ChainCmd.none
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, ChainCmd.none )

        CreationInfoFetched fetchResult ->
            case fetchResult of
                Ok createdSell ->
                    let
                        newCreationInfo =
                            { address = createdSell.address_
                            , blocknum =
                                case BigIntHelpers.toInt createdSell.blocknum of
                                    Just blocknum ->
                                        blocknum

                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "Error converting blocknum from bigint" createdSell.blocknum
                                        in
                                        0
                            }

                        sentries =
                            ( EventSentryHack.init
                                model.ethNode.http
                                newCreationInfo.address
                                TTS.initiatorStatementLogEvent
                                TTS.initiatorStatementLogDecoder
                                InitiatorStatementsFetched
                                newCreationInfo.blocknum
                                InitiatorStatementEventSentryMsg
                            , EventSentryHack.init
                                model.ethNode.http
                                newCreationInfo.address
                                TTS.responderStatementLogEvent
                                TTS.responderStatementLogDecoder
                                ResponderStatementsFetched
                                newCreationInfo.blocknum
                                ResponderStatementEventSentryMsg
                            )

                        pollCmd =
                            getSentryPollCmd sentries

                        newModel =
                            { model
                                | ttsInfo = partialInfo newCreationInfo
                                , eventSentries = Just sentries
                            }
                    in
                    ( newModel
                    , Cmd.batch
                        [ Contracts.Wrappers.getParametersAndStateCmd newModel.ethNode newModel.tokenDecimals newCreationInfo.address ParametersFetched StateFetched
                        , pollCmd
                        ]
                    , ChainCmd.none
                    )

                Err errstr ->
                    let
                        _ =
                            Debug.log "can't fetch full state: " errstr
                    in
                    ( model, Cmd.none, ChainCmd.none )

        StateFetched fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    ( { model | ttsInfo = model.ttsInfo |> updateState state }, Cmd.none, ChainCmd.none )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    ( { model | ttsInfo = model.ttsInfo |> updateParameters parameters }, Cmd.none, ChainCmd.none )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        InitiatorStatementsFetched fetchResult ->
            case fetchResult of
                Ok events ->
                    let
                        nextMessageID =
                            Array.length model.messages

                        newMessages =
                            events
                                |> List.map
                                    (\event ->
                                        { who = Initiator
                                        , message =
                                            case
                                                ( decodeEncryptedMessage event.returnData.encryptedForInitiator
                                                , decodeEncryptedMessage event.returnData.encryptedForResponder
                                                )
                                            of
                                                ( Just decodedForInitiator, Just decodedForResponder ) ->
                                                    Encrypted ( decodedForInitiator, decodedForResponder )

                                                _ ->
                                                    FailedDecode
                                        , blocknum = event.blockNumber
                                        }
                                    )
                                |> Array.fromList

                        newModel =
                            { model
                                | messages =
                                    Array.append model.messages newMessages
                            }

                        cmd =
                            let
                                userRole =
                                    Maybe.map2
                                        getUserRole
                                        (case model.ttsInfo of
                                            Loaded ttsInfo ->
                                                Just ttsInfo

                                            troublesomeGarbage ->
                                                let
                                                    _ =
                                                        Debug.log "Trying to build decryption command, but missing crucial info" troublesomeGarbage
                                                in
                                                Nothing
                                        )
                                        (model.userInfo
                                            |> Maybe.map (\i -> i.address)
                                        )
                                        |> Maybe.Extra.join
                            in
                            case userRole of
                                Nothing ->
                                    Cmd.none

                                Just role ->
                                    decryptNewMessagesCmd newModel role
                    in
                    ( newModel, cmd, ChainCmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "error with initiator statement fetch" errstr
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ResponderStatementsFetched fetchResult ->
            case fetchResult of
                Ok events ->
                    let
                        nextMessageID =
                            Array.length model.messages

                        newMessages =
                            events
                                |> List.map
                                    (\event ->
                                        { who = Responder
                                        , message =
                                            case
                                                ( decodeEncryptedMessage event.returnData.encryptedForInitiator
                                                , decodeEncryptedMessage event.returnData.encryptedForResponder
                                                )
                                            of
                                                ( Just decodedForInitiator, Just decodedForResponder ) ->
                                                    Encrypted ( decodedForInitiator, decodedForResponder )

                                                _ ->
                                                    FailedDecode
                                        , blocknum = event.blockNumber
                                        }
                                    )
                                |> Array.fromList

                        newModel =
                            { model
                                | messages =
                                    Array.append model.messages newMessages
                            }

                        cmd =
                            let
                                userRole =
                                    Maybe.map2
                                        getUserRole
                                        (case model.ttsInfo of
                                            Loaded ttsInfo ->
                                                Just ttsInfo

                                            troublesomeGarbage ->
                                                let
                                                    _ =
                                                        Debug.log "Trying to build decryption command, but missing crucial info" troublesomeGarbage
                                                in
                                                Nothing
                                        )
                                        (model.userInfo
                                            |> Maybe.map (\i -> i.address)
                                        )
                                        |> Maybe.Extra.join
                            in
                            case userRole of
                                Nothing ->
                                    Cmd.none

                                Just role ->
                                    decryptNewMessagesCmd newModel role
                    in
                    ( newModel, cmd, ChainCmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "error with initiator statement fetch" errstr
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ContractAction actionMsg ->
            let
                chainCmd =
                    case model.ttsInfo of
                        Loaded ttsInfo ->
                            case actionMsg of
                                RenderContract.Types.Recall ->
                                    let
                                        txParams =
                                            TTS.recall ttsInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Commit ->
                                    let
                                        txParams =
                                            TokenContract.approve
                                                model.tokenAddress
                                                ttsInfo.creationInfo.address
                                                (TokenValue.getBigInt ttsInfo.parameters.responderDeposit)
                                                |> Eth.toSend

                                        customSend =
                                            { onMined = Just ( PreCommitApproveMined, Nothing )
                                            , onSign = Nothing
                                            , onBroadcast = Nothing
                                            }
                                    in
                                    ChainCmd.custom customSend
                                        txParams

                                RenderContract.Types.Claim ->
                                    let
                                        txParams =
                                            TTS.claim ttsInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Release ->
                                    let
                                        txParams =
                                            TTS.release ttsInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Burn ->
                                    let
                                        txParams =
                                            TTS.burn ttsInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Poke ->
                                    let
                                        txParams =
                                            TTS.poke ttsInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                        ttsInfoNotYetLoaded ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but contract info is not yet loaded :/" ttsInfoNotYetLoaded
                            in
                            ChainCmd.none
            in
            ( model, Cmd.none, chainCmd )

        ContractActionMined _ ->
            let
                _ =
                    Debug.log "mined!" ""
            in
            ( model, Cmd.none, ChainCmd.none )

        PreCommitApproveMined txReceiptResult ->
            case txReceiptResult of
                Err s ->
                    let
                        _ =
                            Debug.log "error mining transaction" s
                    in
                    ( model, Cmd.none, ChainCmd.none )

                Ok txReceipt ->
                    case ( model.ttsInfo, model.userInfo ) of
                        ( Loaded ttsInfo, Just userInfo ) ->
                            let
                                txParams =
                                    TTS.commit ttsInfo.creationInfo.address userInfo.commPubkey
                                        |> Eth.toSend
                            in
                            ( model, Cmd.none, ChainCmd.custom genericCustomSend txParams )

                        incomplete ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but missing crucial info" incomplete
                            in
                            ( model, Cmd.none, ChainCmd.none )

        MessageInputChanged newMessageStr ->
            ( { model | messageInput = newMessageStr }
            , Cmd.none
            , ChainCmd.none
            )

        MessageSubmit ->
            case ( model.userInfo, model.ttsInfo ) of
                ( Just userInfo, Loaded ttsInfo ) ->
                    let
                        cmd =
                            encryptToPubkeys (encodeEncryptionArgs model.messageInput (getCommPubkeys ttsInfo))
                    in
                    ( model, cmd, ChainCmd.none )

                incomplete ->
                    let
                        _ =
                            Debug.log "Incomplete data found when processing MessageSubmit" incomplete
                    in
                    ( model, Cmd.none, ChainCmd.none )

        EncryptionFinished encryptedMessagesValue ->
            let
                encodedEncryptionMessages =
                    decodeEncryptionResult encryptedMessagesValue
                        |> Result.map
                            (\( initiatorMessage, responderMessage ) ->
                                ( encodeEncryptedMessage initiatorMessage
                                , encodeEncryptedMessage responderMessage
                                )
                            )
            in
            case ( model.userInfo, model.ttsInfo, encodedEncryptionMessages ) of
                ( Just userInfo, Loaded ttsInfo, Ok ( Ok initiatorMessage, Ok responderMessage ) ) ->
                    case getUserRole ttsInfo userInfo.address of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "How did you click that button? You don't seem to be the Initiator or Responder..." ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        Just userRole ->
                            let
                                txParams =
                                    case userRole of
                                        Initiator ->
                                            TTS.initiatorStatement ttsInfo.creationInfo.address initiatorMessage responderMessage
                                                |> Eth.toSend

                                        Responder ->
                                            TTS.responderStatement ttsInfo.creationInfo.address initiatorMessage responderMessage
                                                |> Eth.toSend
                            in
                            ( model
                            , Cmd.none
                            , ChainCmd.custom genericCustomSend txParams
                            )

                problematicBullshit ->
                    let
                        _ =
                            Debug.log "MessageSubmit called, but something has gone terribly wrong" problematicBullshit
                    in
                    ( model, Cmd.none, ChainCmd.none )

        DecryptionFinished decryptedMessageValue ->
            case decodeDecryptionResult decryptedMessageValue of
                Ok ( id, message ) ->
                    case Array.get id model.messages of
                        Just commMessage ->
                            let
                                newCommMessage =
                                    { commMessage
                                        | message = Decrypted message
                                    }

                                newMessageArray =
                                    Array.set id newCommMessage model.messages
                            in
                            ( { model | messages = newMessageArray }
                            , Cmd.none
                            , ChainCmd.none
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "got a decryption result, but for an id out of bounds!" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "Error decoding decryption result" errstr
                    in
                    ( model, Cmd.none, ChainCmd.none )

        InitiatorStatementEventSentryMsg eventMsg ->
            case model.eventSentries of
                Just eventSentries ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentryHack.update
                                eventMsg
                                (Tuple.first eventSentries)
                    in
                    ( { model
                        | eventSentries =
                            Just
                                ( newEventSentry, Tuple.second eventSentries )
                      }
                    , cmd
                    , ChainCmd.none
                    )

                Nothing ->
                    let
                        _ =
                            Debug.log "get an eventSentry msg, but there aren't any eventSentries..!?" eventMsg
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ResponderStatementEventSentryMsg eventMsg ->
            case model.eventSentries of
                Just eventSentries ->
                    let
                        ( newEventSentry, cmd ) =
                            EventSentryHack.update
                                eventMsg
                                (Tuple.second eventSentries)
                    in
                    ( { model
                        | eventSentries =
                            Just
                                ( Tuple.first eventSentries, newEventSentry )
                      }
                    , cmd
                    , ChainCmd.none
                    )

                Nothing ->
                    let
                        _ =
                            Debug.log "get an eventSentry msg, but there aren't any eventSentries..!?" eventMsg
                    in
                    ( model, Cmd.none, ChainCmd.none )


encodeEncryptionArgs : String -> List String -> Json.Encode.Value
encodeEncryptionArgs message commPubkeys =
    Json.Encode.object
        [ ( "message", Json.Encode.string message )
        , ( "pubkeyHexStrings"
          , Json.Encode.list Json.Encode.string commPubkeys
          )
        ]


encodeDecryptionArgs : Int -> EncryptedMessage -> Json.Encode.Value
encodeDecryptionArgs messageID encryptedMessage =
    Json.Encode.object
        [ ( "id", Json.Encode.int messageID )
        , ( "encapsulation", Json.Encode.string encryptedMessage.encapsulatedKey )
        , ( "iv", Json.Encode.string encryptedMessage.iv )
        , ( "tag", Json.Encode.string encryptedMessage.tag )
        , ( "encrypted", Json.Encode.string encryptedMessage.message )
        ]


decodeEncryptionResult : Json.Decode.Value -> Result String ( EncryptedMessage, EncryptedMessage )
decodeEncryptionResult value =
    let
        encryptedMessageDecoder =
            Json.Decode.map4 EncryptedMessage
                (Json.Decode.field "encapsulation" Json.Decode.string)
                (Json.Decode.field "iv" Json.Decode.string)
                (Json.Decode.field "tag" Json.Decode.string)
                (Json.Decode.field "encrypted" Json.Decode.string)

        decoder =
            Json.Decode.list encryptedMessageDecoder
    in
    case Json.Decode.decodeValue decoder value of
        Err decodeErr ->
            Err (Json.Decode.errorToString decodeErr)

        Ok list ->
            list
                |> Array.fromList
                |> (\arr ->
                        case ( Array.get 0 arr, Array.get 1 arr ) of
                            ( Just initiatorMessage, Just responderMessage ) ->
                                Ok ( initiatorMessage, responderMessage )

                            _ ->
                                Err "Decoded list has less than 2 items."
                   )


decodeDecryptionResult : Json.Decode.Value -> Result String ( Int, String )
decodeDecryptionResult value =
    let
        decoder =
            Json.Decode.map2
                Tuple.pair
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "message" Json.Decode.string)
    in
    Json.Decode.decodeValue decoder value
        |> Result.mapError Json.Decode.errorToString


encodeEncryptedMessage : EncryptedMessage -> Result String String
encodeEncryptedMessage encryptedMessage =
    encodeSizedStrings
        [ encryptedMessage.encapsulatedKey, encryptedMessage.iv, encryptedMessage.tag, encryptedMessage.message ]


encodeSizedStrings : List String -> Result String String
encodeSizedStrings strings =
    let
        prependWithLengthAsChar s =
            let
                len =
                    String.length s
            in
            if len > 0x0010FFFF then
                -- Char.fromCode / Char.toCode encoding hack breaks past this limit
                Err "string is too long"

            else
                Ok <|
                    String.cons (Char.fromCode len) s
    in
    strings
        |> List.map prependWithLengthAsChar
        |> Result.Extra.combine
        |> Result.map (String.join "")


decodeEncryptedMessage : String -> Maybe EncryptedMessage
decodeEncryptedMessage encoded =
    let
        stringArray =
            decodeSizedStringHelper (String.toList encoded) []
                |> Array.fromList
    in
    Maybe.map4
        EncryptedMessage
        (Array.get 0 stringArray)
        (Array.get 1 stringArray)
        (Array.get 2 stringArray)
        (Array.get 3 stringArray)


decodeSizedStringHelper : List Char -> List String -> List String
decodeSizedStringHelper remaining processed =
    case remaining of
        [] ->
            processed

        c :: r ->
            let
                len =
                    Char.toCode c

                str =
                    List.take len r
                        |> String.fromList

                newRemaining =
                    List.drop len r

                newProcessed =
                    processed ++ [ str ]
            in
            decodeSizedStringHelper newRemaining newProcessed


decryptNewMessagesCmd : Model -> InitiatorOrResponder -> Cmd Msg
decryptNewMessagesCmd model userRole =
    model.messages
        |> Array.toIndexedList
        |> List.map
            (\( id, commMessage ) ->
                case commMessage.message of
                    Encrypted messages ->
                        let
                            encryptedMessage =
                                case userRole of
                                    Initiator ->
                                        Tuple.first messages

                                    Responder ->
                                        Tuple.second messages
                        in
                        encodeDecryptionArgs id encryptedMessage
                            |> decryptMessage

                    _ ->
                        Cmd.none
            )
        |> Cmd.batch


getCommPubkeys : TTSFullInfo -> List String
getCommPubkeys ttsInfo =
    case ttsInfo.state.responderCommPubkey of
        Just responderCommPubkey ->
            [ ttsInfo.parameters.initiatorCommPubkey
            , responderCommPubkey
            ]

        Nothing ->
            let
                _ =
                    Debug.log "Trying to encrypt a message, but can't find the responderCommPubkey! Is the contract still in the Open phase?" ""
            in
            []


getSentryPollCmd : ( EventSentry TTS.InitiatorStatementLog Msg, EventSentry TTS.ResponderStatementLog Msg ) -> Cmd Msg
getSentryPollCmd sentries =
    Cmd.batch
        [ EventSentryHack.pollForChanges (Tuple.first sentries)
        , EventSentryHack.pollForChanges (Tuple.second sentries)
        ]


genericCustomSend =
    { onMined = Just ( ContractActionMined, Nothing )
    , onSign = Nothing
    , onBroadcast = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 3000 Refresh
        , encryptionFinished EncryptionFinished
        , decryptionFinished DecryptionFinished
        ]


port encryptToPubkeys : Json.Encode.Value -> Cmd msg


port encryptionFinished : (Json.Decode.Value -> msg) -> Sub msg


port decryptMessage : Json.Encode.Value -> Cmd msg


port decryptionFinished : (Json.Decode.Value -> msg) -> Sub msg
