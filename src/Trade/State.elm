port module Trade.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx exposing (CustomSend)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
import Network exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Result.Extra
import Time
import TokenValue
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.State as ChatHistory
import Trade.ChatHistory.Types as ChatHistory
import Trade.Types exposing (..)


init : EthHelpers.EthNode -> Maybe UserInfo -> Int -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode userInfo tradeId =
    let
        getCreationInfoCmd =
            getContractCreationInfoCmd ethNode tradeId

        ( eventSentry, eventSentryCmd ) =
            EventSentry.init EventSentryMsg ethNode.http
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , trade = CTypes.partialTradeInfo tradeId
      , stats = Waiting
      , expandedPhase = CTypes.Open
      , chatHistoryModel = Nothing
      , showChatHistory = False
      , eventsWaitingForChatHistory = []
      , secureCommInfo = partialCommInfo
      , eventSentry = eventSentry
      , allowance = Nothing
      , txChainStatus = NoTx
      }
    , Cmd.batch [ getCreationInfoCmd, eventSentryCmd ]
    , ChainCmd.none
    )


getContractCreationInfoCmd : EthHelpers.EthNode -> Int -> Cmd Msg
getContractCreationInfoCmd ethNode id =
    Contracts.Wrappers.getCreationInfoFromIdCmd ethNode (BigInt.fromInt id) CreationInfoFetched


updateUserInfo : Maybe UserInfo -> Model -> ( Model, Cmd Msg )
updateUserInfo userInfo model =
    ( { model | userInfo = userInfo }
    , case ( userInfo, model.trade ) of
        ( Just uInfo, CTypes.LoadedTrade trade ) ->
            Contracts.Wrappers.getAllowanceCmd
                model.ethNode
                uInfo.address
                trade.creationInfo.address
                AllowanceFetched

        _ ->
            Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg prevModel =
    case msg of
        Refresh time ->
            let
                fetchCreationInfoCmd =
                    case prevModel.trade of
                        CTypes.PartiallyLoadedTrade pInfo ->
                            case pInfo.creationInfo of
                                Nothing ->
                                    getContractCreationInfoCmd prevModel.ethNode pInfo.factoryID

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none

                ( newChatHistoryModel, shouldDecrypt ) =
                    case prevModel.chatHistoryModel of
                        Nothing ->
                            tryInitChatHistory prevModel.trade prevModel.userInfo prevModel.eventsWaitingForChatHistory

                        _ ->
                            ( prevModel.chatHistoryModel, False )

                decryptCmd =
                    if shouldDecrypt then
                        tryBuildDecryptCmd prevModel

                    else
                        Cmd.none

                fetchAllowanceCmd =
                    case ( prevModel.userInfo, prevModel.trade ) of
                        ( Just userInfo, CTypes.LoadedTrade trade ) ->
                            Contracts.Wrappers.getAllowanceCmd
                                prevModel.ethNode
                                userInfo.address
                                trade.creationInfo.address
                                AllowanceFetched

                        _ ->
                            Cmd.none

                newModel =
                    { prevModel | chatHistoryModel = newChatHistoryModel }
            in
            case prevModel.trade of
                CTypes.LoadedTrade tradeInfo ->
                    ( newModel
                    , Cmd.batch
                        [ Contracts.Wrappers.getStateCmd prevModel.ethNode tradeInfo.creationInfo.address StateFetched
                        , decryptCmd
                        , fetchCreationInfoCmd
                        , fetchAllowanceCmd
                        ]
                    , ChainCmd.none
                    )

                _ ->
                    ( newModel, Cmd.none, ChainCmd.none )

        AllowanceFetched fetchResult ->
            case fetchResult of
                Ok allowance ->
                    let
                        newModel =
                            { prevModel
                                | allowance = Just allowance
                            }
                    in
                    case ( newModel.txChainStatus, newModel.trade, newModel.userInfo ) of
                        ( ApproveMining _, CTypes.LoadedTrade trade, Just userInfo ) ->
                            if BigInt.compare allowance (CTypes.responderDeposit trade.parameters |> TokenValue.getBigInt) /= LT then
                                let
                                    ( txChainStatus, chainCmd ) =
                                        initiateCommitCall trade.creationInfo.address userInfo.commPubkey
                                in
                                ( { newModel | txChainStatus = txChainStatus }
                                , Cmd.none
                                , chainCmd
                                )

                            else
                                ( newModel
                                , Cmd.none
                                , ChainCmd.none
                                )

                        _ ->
                            ( newModel
                            , Cmd.none
                            , ChainCmd.none
                            )

                Err e ->
                    let
                        _ =
                            Debug.log "Error fecthing allowance" e
                    in
                    ( prevModel
                    , Cmd.none
                    , ChainCmd.none
                    )

        CreationInfoFetched fetchResult ->
            case fetchResult of
                Ok createdSell ->
                    let
                        newCreationInfo =
                            { address = createdSell.address_
                            , blocknum = BigIntHelpers.toIntWithWarning createdSell.blocknum
                            }

                        ( newSentry, sentryCmd, _ ) =
                            EventSentry.watch
                                EventLogFetched
                                prevModel.eventSentry
                                { address = newCreationInfo.address
                                , fromBlock = Eth.Types.BlockNum newCreationInfo.blocknum
                                , toBlock = Eth.Types.LatestBlock
                                , topics = []
                                }

                        newModel =
                            { prevModel
                                | trade = prevModel.trade |> CTypes.updateCreationInfo newCreationInfo
                                , eventSentry = newSentry
                            }

                        cmd =
                            Cmd.batch
                                [ sentryCmd
                                , Contracts.Wrappers.getParametersAndStateCmd newModel.ethNode newCreationInfo.address ParametersFetched StateFetched
                                ]
                    in
                    ( newModel
                    , cmd
                    , ChainCmd.none
                    )

                Err (Http.BadBody errstr) ->
                    Debug.todo "No contract at this id. Maybe should reload after some delay."

                Err otherErr ->
                    let
                        _ =
                            Debug.log "can't fetch full state: " otherErr
                    in
                    ( prevModel, Cmd.none, ChainCmd.none )

        StateFetched fetchResult ->
            case fetchResult of
                Ok (Just newState) ->
                    let
                        didPhaseChange =
                            case prevModel.trade of
                                CTypes.PartiallyLoadedTrade _ ->
                                    True

                                CTypes.LoadedTrade oldTradeInfo ->
                                    oldTradeInfo.state.phase /= newState.phase

                        newModel =
                            { prevModel
                                | trade = prevModel.trade |> CTypes.updateState newState
                                , expandedPhase =
                                    if didPhaseChange then
                                        newState.phase

                                    else
                                        prevModel.expandedPhase
                            }
                    in
                    ( newModel
                    , tryBuildDecryptCmd newModel
                    , ChainCmd.none
                    )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( prevModel, Cmd.none, ChainCmd.none )

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Ok parameters) ->
                    let
                        newModel =
                            { prevModel
                                | trade = prevModel.trade |> CTypes.updateParameters parameters
                            }
                    in
                    ( newModel
                    , tryBuildDecryptCmd newModel
                    , ChainCmd.none
                    )

                badResult ->
                    let
                        _ =
                            Debug.log "bad parametersFetched result" badResult
                    in
                    ( prevModel, Cmd.none, ChainCmd.none )

        EventLogFetched log ->
            let
                ( newModel, cmd ) =
                    handleNewLog log prevModel
            in
            ( newModel, cmd, ChainCmd.none )

        ExpandPhase phase ->
            ( { prevModel | expandedPhase = phase }
            , Cmd.none
            , ChainCmd.none
            )

        ToggleChat ->
            let
                showChat =
                    if prevModel.showChatHistory then
                        False

                    else
                        True
            in
            ( { prevModel | showChatHistory = showChat }
            , Cmd.none
            , ChainCmd.none
            )

        CommitClicked trade userInfo depositAmount ->
            ( { prevModel | txChainStatus = ConfirmingCommit trade userInfo depositAmount }
            , Cmd.none
            , ChainCmd.none
            )

        AbortCommit ->
            ( { prevModel | txChainStatus = NoTx }
            , Cmd.none
            , ChainCmd.none
            )

        ConfirmCommit trade userInfo depositAmount ->
            let
                ( txChainStatus, chainCmd ) =
                    let
                        approveChainCmd =
                            let
                                txParams =
                                    TokenContract.approve
                                        (daiAddress prevModel.ethNode.network)
                                        trade.creationInfo.address
                                        depositAmount
                                        |> Eth.toSend

                                customSend =
                                    { onMined = Nothing
                                    , onSign = Just ApproveSigned
                                    , onBroadcast = Nothing
                                    }
                            in
                            ChainCmd.custom customSend txParams
                    in
                    case prevModel.allowance of
                        Just allowance ->
                            if BigInt.compare allowance (CTypes.responderDeposit trade.parameters |> TokenValue.getBigInt) /= LT then
                                initiateCommitCall trade.creationInfo.address userInfo.commPubkey

                            else
                                ( ApproveNeedsSig, approveChainCmd )

                        _ ->
                            ( ApproveNeedsSig, approveChainCmd )
            in
            ( { prevModel | txChainStatus = txChainStatus }
            , Cmd.none
            , chainCmd
            )

        StartContractAction actionMsg ->
            let
                ( txChainStatus, chainCmd ) =
                    case prevModel.trade of
                        CTypes.LoadedTrade tradeInfo ->
                            case actionMsg of
                                Recall ->
                                    let
                                        txParams =
                                            DHT.recall tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Recall
                                    , ChainCmd.custom (contractActionSend Recall) txParams
                                    )

                                Claim ->
                                    let
                                        txParams =
                                            DHT.claim tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Claim
                                    , ChainCmd.custom (contractActionSend Claim) txParams
                                    )

                                Abort ->
                                    let
                                        txParams =
                                            DHT.abort tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Abort
                                    , ChainCmd.custom (contractActionSend Abort) txParams
                                    )

                                Release ->
                                    let
                                        txParams =
                                            DHT.release tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Release
                                    , ChainCmd.custom (contractActionSend Release) txParams
                                    )

                                Burn ->
                                    let
                                        txParams =
                                            DHT.burn tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Burn
                                    , ChainCmd.custom (contractActionSend Burn) txParams
                                    )

                                Poke ->
                                    let
                                        txParams =
                                            DHT.poke tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( ActionNeedsSig Poke
                                    , ChainCmd.custom (contractActionSend Poke) txParams
                                    )

                        tradeInfoNotYetLoaded ->
                            let
                                _ =
                                    Debug.log "Trying to handle StartContractAction msg, but contract info is not yet loaded :/" tradeInfoNotYetLoaded
                            in
                            ( prevModel.txChainStatus
                            , ChainCmd.none
                            )
            in
            ( { prevModel
                | txChainStatus = txChainStatus
              }
            , Cmd.none
            , chainCmd
            )

        ApproveSigned txHashResult ->
            case txHashResult of
                Ok txHash ->
                    ( { prevModel | txChainStatus = ApproveMining txHash }
                    , Cmd.none
                    , ChainCmd.none
                    )

                Err errstr ->
                    ( { prevModel | txChainStatus = TxError errstr }
                    , Cmd.none
                    , ChainCmd.none
                    )

        CommitSigned txHashResult ->
            case txHashResult of
                Ok txHash ->
                    ( { prevModel | txChainStatus = CommitMining txHash }
                    , Cmd.none
                    , ChainCmd.none
                    )

                Err errstr ->
                    ( { prevModel | txChainStatus = TxError errstr }
                    , Cmd.none
                    , ChainCmd.none
                    )

        CommitMined _ ->
            ( { prevModel | txChainStatus = NoTx }
            , Cmd.none
            , ChainCmd.none
            )

        ActionSigned action txHashResult ->
            case txHashResult of
                Ok txHash ->
                    ( { prevModel | txChainStatus = ActionMining action txHash }
                    , Cmd.none
                    , ChainCmd.none
                    )

                Err errstr ->
                    ( { prevModel | txChainStatus = TxError errstr }
                    , Cmd.none
                    , ChainCmd.none
                    )

        ActionMined action _ ->
            ( { prevModel | txChainStatus = NoTx }
            , Cmd.none
            , ChainCmd.none
            )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            , ChainCmd.none
            )

        ChatHistoryMsg chatHistoryMsg ->
            case prevModel.chatHistoryModel of
                Just prevChatHistoryModel ->
                    let
                        ( chatHistoryModel, shouldDecrypt, maybeMessageToSend ) =
                            ChatHistory.update chatHistoryMsg prevChatHistoryModel

                        encryptCmd =
                            case maybeMessageToSend of
                                Just chatMessage ->
                                    case prevModel.secureCommInfo of
                                        LoadedCommInfo commInfo ->
                                            encryptToPubkeys (encodeEncryptionArgs chatMessage commInfo)

                                        incomplete ->
                                            let
                                                _ =
                                                    Debug.log "Incomplete data found when trying to build encryption cmd" incomplete
                                            in
                                            Cmd.none

                                Nothing ->
                                    Cmd.none

                        model =
                            { prevModel | chatHistoryModel = Just chatHistoryModel }

                        decryptCmd =
                            if shouldDecrypt then
                                tryBuildDecryptCmd prevModel

                            else
                                Cmd.none
                    in
                    ( model
                    , Cmd.batch
                        [ decryptCmd
                        , encryptCmd
                        ]
                    , ChainCmd.none
                    )

                Nothing ->
                    let
                        _ =
                            Debug.log "Got a chat history message, but there is no chat history model!" ""
                    in
                    ( prevModel, Cmd.none, ChainCmd.none )

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
            case ( prevModel.userInfo, prevModel.trade, encodedEncryptionMessages ) of
                ( Just userInfo, CTypes.LoadedTrade tradeInfo, Ok ( Ok initiatorMessage, Ok responderMessage ) ) ->
                    case CTypes.getInitiatorOrResponder tradeInfo userInfo.address of
                        Nothing ->
                            let
                                _ =
                                    Debug.log "How did you click that button? You don't seem to be the Initiator or Responder..." ""
                            in
                            ( prevModel, Cmd.none, ChainCmd.none )

                        Just userRole ->
                            let
                                txParams =
                                    case userRole of
                                        Initiator ->
                                            DHT.initiatorStatement tradeInfo.creationInfo.address initiatorMessage responderMessage
                                                |> Eth.toSend

                                        Responder ->
                                            DHT.responderStatement tradeInfo.creationInfo.address initiatorMessage responderMessage
                                                |> Eth.toSend
                            in
                            ( prevModel
                            , Cmd.none
                            , ChainCmd.custom
                                { onMined = Nothing
                                , onSign = Nothing
                                , onBroadcast = Nothing
                                }
                                txParams
                            )

                problematicBullshit ->
                    let
                        _ =
                            Debug.log "MessageSubmit called, but something has gone terribly wrong" problematicBullshit
                    in
                    ( prevModel, Cmd.none, ChainCmd.none )

        MessageSubmitMined (Ok txReceipt) ->
            let
                _ =
                    Debug.log "Message submit mined!" ""
            in
            ( prevModel, Cmd.none, ChainCmd.none )

        MessageSubmitMined (Err errstr) ->
            let
                _ =
                    Debug.log "Error mining message submit" errstr
            in
            ( prevModel, Cmd.none, ChainCmd.none )


initiateCommitCall : Address -> String -> ( TxChainStatus, ChainCmd Msg )
initiateCommitCall tradeAddress commPubkey =
    let
        txParams =
            DHT.commit tradeAddress commPubkey
                |> Eth.toSend
    in
    ( CommitNeedsSig
    , ChainCmd.custom
        { onMined = Just ( CommitMined, Nothing )
        , onSign = Just CommitSigned
        , onBroadcast = Nothing
        }
        txParams
    )


handleNewLog : Eth.Types.Log -> Model -> ( Model, Cmd Msg )
handleNewLog log prevModel =
    let
        decodedEventLog =
            Eth.Decode.event CTypes.eventDecoder log
    in
    case decodedEventLog.returnData of
        Err err ->
            let
                _ =
                    Debug.log "Error decoding contract event" err
            in
            ( prevModel, Cmd.none )

        Ok event ->
            let
                newTrade =
                    case event of
                        CTypes.OpenedEvent data ->
                            case PaymentMethods.decodePaymentMethodList data.fiatTransferMethods of
                                Ok paymentMethods ->
                                    prevModel.trade
                                        |> CTypes.updatePaymentMethods
                                            paymentMethods

                                Err errStr ->
                                    let
                                        _ =
                                            Debug.log "Couldn't decode payment methods!" errStr
                                    in
                                    prevModel.trade

                        _ ->
                            prevModel.trade

                newSecureCommInfo =
                    case event of
                        CTypes.OpenedEvent data ->
                            prevModel.secureCommInfo
                                |> updateInitiatorPubkey data.commPubkey

                        CTypes.CommittedEvent data ->
                            prevModel.secureCommInfo
                                |> updateResponderPubkey data.commPubkey

                        _ ->
                            prevModel.secureCommInfo

                ( newChatHistoryModel, shouldDecrypt ) =
                    case prevModel.chatHistoryModel of
                        Just prevChatHistoryModel ->
                            ChatHistory.handleNewEvent
                                decodedEventLog.blockNumber
                                event
                                prevChatHistoryModel
                                |> Tuple.mapFirst Just

                        Nothing ->
                            -- chat is uninitialized; initialize if we can
                            tryInitChatHistory newTrade prevModel.userInfo prevModel.eventsWaitingForChatHistory

                eventsToSave =
                    case newChatHistoryModel of
                        Nothing ->
                            List.append
                                prevModel.eventsWaitingForChatHistory
                                [ ( decodedEventLog.blockNumber, event ) ]

                        Just _ ->
                            []

                newModel =
                    { prevModel
                        | trade = newTrade
                        , chatHistoryModel = newChatHistoryModel
                        , secureCommInfo = newSecureCommInfo
                        , eventsWaitingForChatHistory = eventsToSave
                    }

                cmd =
                    if shouldDecrypt then
                        tryBuildDecryptCmd newModel

                    else
                        Cmd.none
            in
            ( newModel
            , cmd
            )


tryInitChatHistory : CTypes.Trade -> Maybe UserInfo -> List ( Int, CTypes.DAIHardEvent ) -> ( Maybe ChatHistory.Model, Bool )
tryInitChatHistory maybeTrade maybeUserInfo pendingEvents =
    case ( maybeTrade, maybeUserInfo ) of
        ( CTypes.LoadedTrade tradeInfo, Just userInfo ) ->
            let
                maybeBuyerOrSeller =
                    CTypes.getBuyerOrSeller tradeInfo userInfo.address
            in
            case maybeBuyerOrSeller of
                Just buyerOrSeller ->
                    ChatHistory.init
                        userInfo
                        buyerOrSeller
                        tradeInfo.parameters.openMode
                        pendingEvents
                        |> Tuple.mapFirst Just

                Nothing ->
                    ( Nothing, False )

        _ ->
            ( Nothing, False )


tryBuildDecryptCmd : Model -> Cmd Msg
tryBuildDecryptCmd model =
    let
        userRole =
            Maybe.map2
                CTypes.getInitiatorOrResponder
                (case model.trade of
                    CTypes.LoadedTrade tradeInfo ->
                        Just tradeInfo

                    _ ->
                        Nothing
                )
                (model.userInfo
                    |> Maybe.map (\i -> i.address)
                )
                |> Maybe.Extra.join
    in
    case ( model.chatHistoryModel, userRole ) of
        ( Just chatHistoryModel, Just role ) ->
            decryptNewMessagesCmd chatHistoryModel role

        _ ->
            Cmd.none


decryptNewMessagesCmd : ChatHistory.Model -> InitiatorOrResponder -> Cmd Msg
decryptNewMessagesCmd model userRole =
    model.history
        |> Array.toIndexedList
        |> List.map
            (\( id, historyEvent ) ->
                case historyEvent.eventInfo of
                    ChatHistory.Statement commMessage ->
                        case commMessage.message of
                            ChatHistory.Encrypted messages ->
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


contractActionSend : ContractAction -> CustomSend Msg
contractActionSend action =
    { onMined = Just ( ActionMined action, Nothing )
    , onSign = Just <| ActionSigned action
    , onBroadcast = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 3000 Refresh
        , encryptionFinished EncryptionFinished
        , decryptionFinished <| \res -> ChatHistoryMsg (ChatHistory.DecryptionFinished res)
        ]


port encryptToPubkeys : Json.Encode.Value -> Cmd msg


port encryptionFinished : (Json.Decode.Value -> msg) -> Sub msg


port decryptMessage : Json.Encode.Value -> Cmd msg


port decryptionFinished : (Json.Decode.Value -> msg) -> Sub msg
