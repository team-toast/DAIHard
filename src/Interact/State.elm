port module Interact.State exposing (decodeSizedStringHelper, encodeSizedStrings, init, subscriptions, update, updateUserInfo)

import Array
import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Generated.Toastytrade as TT
import Contracts.Types
import Contracts.Wrappers
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Interact.Types exposing (..)
import Json.Decode
import Json.Encode
import Maybe.Extra
import RenderContract.Types
import Result.Extra
import Time
import TokenValue


init : EthHelpers.EthNode -> Address -> Address -> Int -> Maybe UserInfo -> Int -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode factoryAddress tokenAddress tokenDecimals userInfo tradeId =
    let
        getCreationInfoCmd =
            getContractCreationInfoCmd ethNode factoryAddress tradeId

        ( eventSentry, eventSentryCmd ) =
            EventSentry.init EventSentryMsg ethNode.http
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , tokenAddress = tokenAddress
      , tokenDecimals = tokenDecimals
      , trade = Contracts.Types.partialTradeInfo tradeId
      , history = Array.empty
      , messageInput = ""
      , eventSentry = eventSentry
      , sentryWatching = False
      }
    , Cmd.batch [ getCreationInfoCmd, eventSentryCmd ]
    , ChainCmd.none
    )


getContractCreationInfoCmd : EthHelpers.EthNode -> Address -> Int -> Cmd Msg
getContractCreationInfoCmd ethNode factoryAddress id =
    Contracts.Wrappers.getCreationInfoFromIdCmd ethNode factoryAddress (BigInt.fromInt id) CreationInfoFetched


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        Refresh time ->
            case model.trade of
                Contracts.Types.Loaded tradeInfo ->
                    ( model
                    , Cmd.batch
                        [ Contracts.Wrappers.getStateCmd model.ethNode model.tokenDecimals tradeInfo.creationInfo.address StateFetched
                        ]
                    , ChainCmd.none
                    )

                _ ->
                    ( model, Cmd.none, ChainCmd.none )

        CreationInfoFetched fetchResult ->
            case fetchResult of
                Ok createdSell ->
                    let
                        newCreationInfo =
                            { address = createdSell.address_
                            , blocknum = BigIntHelpers.toIntWithWarning createdSell.blocknum
                            }

                        newModel =
                            { model
                                | trade = model.trade |> Contracts.Types.updateCreationInfo newCreationInfo
                            }
                    in
                    ( newModel
                    , Contracts.Wrappers.getParametersAndStateCmd newModel.ethNode newModel.tokenDecimals newCreationInfo.address ParametersFetched StateFetched
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
                    let
                        newTrade =
                            model.trade |> Contracts.Types.updateState state
                    in
                    case ( model.sentryWatching, newTrade ) of
                        ( False, Contracts.Types.Loaded info ) ->
                            let
                                ( newSentry, sentryCmd, _ ) =
                                    EventSentry.watch
                                        EventsFetched
                                        model.eventSentry
                                        { address = info.creationInfo.address
                                        , fromBlock = Eth.Types.BlockNum info.creationInfo.blocknum
                                        , toBlock = Eth.Types.LatestBlock
                                        , topics = []
                                        }

                                newModel =
                                    { model
                                        | trade = newTrade
                                        , eventSentry = newSentry
                                        , sentryWatching = True
                                    }
                            in
                            ( newModel
                            , sentryCmd
                            , ChainCmd.none
                            )

                        _ ->
                            ( { model | trade = newTrade }
                            , Cmd.none
                            , ChainCmd.none
                            )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Ok parameters) ->
                    let
                        newTrade =
                            model.trade |> Contracts.Types.updateParameters parameters
                    in
                    case ( model.sentryWatching, newTrade ) of
                        ( False, Contracts.Types.Loaded info ) ->
                            let
                                ( newSentry, sentryCmd, _ ) =
                                    EventSentry.watch
                                        EventsFetched
                                        model.eventSentry
                                        { address = info.creationInfo.address
                                        , fromBlock = Eth.Types.BlockNum info.creationInfo.blocknum
                                        , toBlock = Eth.Types.LatestBlock
                                        , topics = []
                                        }

                                newModel =
                                    { model
                                        | trade = newTrade
                                        , eventSentry = newSentry
                                        , sentryWatching = True
                                    }
                            in
                            ( newModel
                            , sentryCmd
                            , ChainCmd.none
                            )

                        _ ->
                            ( { model | trade = newTrade }
                            , Cmd.none
                            , ChainCmd.none
                            )

                badResult ->
                    let
                        _ =
                            Debug.log "bad parametersFetched result" badResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        EventsFetched logs ->
            let
                ( newModel, cmd ) =
                    handleNewLog logs model
            in
            ( newModel, cmd, ChainCmd.none )

        ContractAction actionMsg ->
            let
                chainCmd =
                    case model.trade of
                        Contracts.Types.Loaded tradeInfo ->
                            case actionMsg of
                                RenderContract.Types.Recall ->
                                    let
                                        txParams =
                                            TT.recall tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Commit ->
                                    let
                                        fullDepositAmount =
                                            TokenValue.getBigInt <|
                                                case tradeInfo.parameters.openMode of
                                                    Contracts.Types.BuyerOpened ->
                                                        tradeInfo.parameters.tradeAmount

                                                    Contracts.Types.SellerOpened ->
                                                        tradeInfo.parameters.buyerDeposit

                                        txParams =
                                            TokenContract.approve
                                                model.tokenAddress
                                                tradeInfo.creationInfo.address
                                                fullDepositAmount
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
                                            TT.claim tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Abort ->
                                    let
                                        txParams =
                                            TT.abort tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Release ->
                                    let
                                        txParams =
                                            TT.release tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Burn ->
                                    let
                                        txParams =
                                            TT.burn tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Poke ->
                                    let
                                        txParams =
                                            TT.poke tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                        tradeInfoNotYetLoaded ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but contract info is not yet loaded :/" tradeInfoNotYetLoaded
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
                    case ( model.trade, model.userInfo ) of
                        ( Contracts.Types.Loaded tradeInfo, Just userInfo ) ->
                            let
                                txParams =
                                    TT.commit tradeInfo.creationInfo.address userInfo.commPubkey
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
            case ( model.userInfo, model.trade ) of
                ( Just userInfo, Contracts.Types.Loaded tradeInfo ) ->
                    let
                        cmd =
                            encryptToPubkeys (encodeEncryptionArgs model.messageInput (getCommPubkeys tradeInfo))
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
            case ( model.userInfo, model.trade, encodedEncryptionMessages ) of
                ( Just userInfo, Contracts.Types.Loaded tradeInfo, Ok ( Ok initiatorMessage, Ok responderMessage ) ) ->
                    case getUserRole tradeInfo userInfo.address of
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
                                            TT.initiatorStatement tradeInfo.creationInfo.address initiatorMessage responderMessage
                                                |> Eth.toSend

                                        Responder ->
                                            TT.responderStatement tradeInfo.creationInfo.address initiatorMessage responderMessage
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
                    case Array.get id model.history of
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
                                            Array.set id newHistoryEvent model.history
                                    in
                                    ( { model | history = newHistory }
                                    , Cmd.none
                                    , ChainCmd.none
                                    )

                                _ ->
                                    let
                                        _ =
                                            Debug.log "got a decryption result, but for an event that is not a message!" ""
                                    in
                                    ( model, Cmd.none, ChainCmd.none )

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

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        model.eventSentry
            in
            ( { model
                | eventSentry =
                    newEventSentry
              }
            , cmd
            , ChainCmd.none
            )


handleNewLog : Eth.Types.Log -> Model -> ( Model, Cmd Msg )
handleNewLog log model =
    let
        decodedEvent =
            Eth.Decode.event Contracts.Types.eventDecoder log
    in
    case decodedEvent.returnData of
        Err err ->
            let
                _ =
                    Debug.log "Error decoding contract event" err
            in
            ( model, Cmd.none )

        Ok returnData ->
            let
                info =
                    case returnData of
                        Contracts.Types.InitiatorStatementLogEvent data ->
                            Statement <|
                                { who = Initiator
                                , message =
                                    case
                                        ( decodeEncryptedMessage data.encryptedForInitiator
                                        , decodeEncryptedMessage data.encryptedForResponder
                                        )
                                    of
                                        ( Just decodedForInitiator, Just decodedForResponder ) ->
                                            Encrypted ( decodedForInitiator, decodedForResponder )

                                        _ ->
                                            FailedDecode
                                , blocknum = decodedEvent.blockNumber
                                }

                        Contracts.Types.ResponderStatementLogEvent data ->
                            Statement <|
                                { who = Responder
                                , message =
                                    case
                                        ( decodeEncryptedMessage data.encryptedForInitiator
                                        , decodeEncryptedMessage data.encryptedForResponder
                                        )
                                    of
                                        ( Just decodedForInitiator, Just decodedForResponder ) ->
                                            Encrypted ( decodedForInitiator, decodedForResponder )

                                        _ ->
                                            FailedDecode
                                , blocknum = decodedEvent.blockNumber
                                }

                        Contracts.Types.CommittedEvent data ->
                            StateChange (Committed data.responder)

                        Contracts.Types.RecalledEvent ->
                            StateChange Recalled

                        Contracts.Types.AbortedEvent ->
                            StateChange Aborted

                        Contracts.Types.ReleasedEvent ->
                            StateChange Released

                        Contracts.Types.BurnedEvent ->
                            StateChange Burned

                        Contracts.Types.PhaseChangeEvent data ->
                            if data.newPhase == BigInt.fromInt 1 then
                                StateChange Opened

                            else if data.newPhase == BigInt.fromInt 3 then
                                StateChange Claimed

                            else
                                StateChange RedundantEvent

                newEvent =
                    { eventInfo = info
                    , blocknum = decodedEvent.blockNumber
                    , time = Nothing
                    }

                newModel =
                    { model
                        | history =
                            Array.append
                                model.history
                                (Array.fromList [ newEvent ])
                    }

                cmd =
                    let
                        userRole =
                            Maybe.map2
                                getUserRole
                                (case model.trade of
                                    Contracts.Types.Loaded tradeInfo ->
                                        Just tradeInfo

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
                    case ( newEvent.eventInfo, userRole ) of
                        ( Statement _, Just role ) ->
                            decryptNewMessagesCmd newModel role

                        _ ->
                            Cmd.none
            in
            ( newModel
            , cmd
            )


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
    let
        _ =
            Debug.log "decrypting" ""
    in
    model.history
        |> Array.toIndexedList
        |> Debug.log "indexed list"
        |> List.map
            (\( id, historyEvent ) ->
                case historyEvent.eventInfo of
                    Statement commMessage ->
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

                    _ ->
                        Cmd.none
            )
        |> Cmd.batch


getCommPubkeys : Contracts.Types.FullTradeInfo -> List String
getCommPubkeys tradeInfo =
    case tradeInfo.state.responderCommPubkey of
        Just responderCommPubkey ->
            [ tradeInfo.parameters.initiatorCommPubkey
            , responderCommPubkey
            ]

        Nothing ->
            let
                _ =
                    Debug.log "Trying to encrypt a message, but can't find the responderCommPubkey! Is the contract still in the Open phase?" ""
            in
            []


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
