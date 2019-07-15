port module Trade.State exposing (init, subscriptions, update, updateUserInfo)

import AppCmd
import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.DAIHardNativeTrade as DHNT
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
import Helpers.BigInt as BigIntHelpers
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Process
import Result.Extra
import Routing
import Task
import Time
import TokenValue
import Trade.ChatHistory.SecureComm exposing (..)
import Trade.ChatHistory.State as ChatHistory
import Trade.ChatHistory.Types as ChatHistory
import Trade.Types exposing (..)
import UserNotice as UN


init : EthHelpers.Web3Context -> Maybe UserInfo -> Int -> UpdateResult
init web3Context userInfo tradeId =
    let
        getCreationInfoCmd =
            getContractCreationInfoCmd web3Context tradeId

        ( eventSentry, eventSentryCmd ) =
            EventSentry.init EventSentryMsg web3Context.httpProvider
    in
    UpdateResult
        { web3Context = web3Context
        , userInfo = userInfo
        , trade = CTypes.partialTradeInfo tradeId
        , expandedPhase = CTypes.Open
        , chatHistoryModel = Nothing
        , showChatHistory = False
        , showStatsModal = False
        , eventsWaitingForChatHistory = []
        , secureCommInfo = partialCommInfo
        , eventSentry = eventSentry
        , allowance = Nothing
        , txChainStatus = Nothing
        }
        (Cmd.batch [ getCreationInfoCmd, eventSentryCmd ])
        ChainCmd.none
        []


getContractCreationInfoCmd : EthHelpers.Web3Context -> Int -> Cmd Msg
getContractCreationInfoCmd web3Context id =
    Contracts.Wrappers.getCreationInfoFromIdCmd web3Context (BigInt.fromInt id) CreationInfoFetched


updateUserInfo : Maybe UserInfo -> Model -> ( Model, Cmd Msg )
updateUserInfo userInfo model =
    ( { model | userInfo = userInfo }
    , case ( userInfo, model.trade, model.web3Context.factoryType ) of
        ( Just uInfo, CTypes.LoadedTrade trade, Token tokenType ) ->
            Contracts.Wrappers.getAllowanceCmd
                model.web3Context
                tokenType
                uInfo.address
                trade.creationInfo.address
                AllowanceFetched

        _ ->
            Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh time ->
            let
                fetchCreationInfoCmd =
                    case prevModel.trade of
                        CTypes.PartiallyLoadedTrade pInfo ->
                            case pInfo.creationInfo of
                                Nothing ->
                                    getContractCreationInfoCmd prevModel.web3Context pInfo.id

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none

                ( newChatHistoryModel, shouldDecrypt ) =
                    case prevModel.chatHistoryModel of
                        Nothing ->
                            tryInitChatHistory prevModel.web3Context prevModel.trade prevModel.userInfo prevModel.eventsWaitingForChatHistory

                        _ ->
                            ( prevModel.chatHistoryModel, False )

                decryptCmd =
                    if shouldDecrypt then
                        tryBuildDecryptCmd prevModel

                    else
                        Cmd.none

                fetchAllowanceCmd =
                    case ( prevModel.userInfo, prevModel.trade, prevModel.web3Context.factoryType ) of
                        ( Just userInfo, CTypes.LoadedTrade trade, Token tokenType ) ->
                            Contracts.Wrappers.getAllowanceCmd
                                prevModel.web3Context
                                tokenType
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
                    UpdateResult
                        newModel
                        (Cmd.batch
                            [ Contracts.Wrappers.getStateCmd prevModel.web3Context tradeInfo.creationInfo.address StateFetched
                            , decryptCmd
                            , fetchCreationInfoCmd
                            , fetchAllowanceCmd
                            ]
                        )
                        ChainCmd.none
                        []

                _ ->
                    justModelUpdate newModel

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
                        ( Just (ApproveMining _), CTypes.LoadedTrade trade, Just userInfo ) ->
                            if BigInt.compare allowance (CTypes.responderDeposit trade.parameters |> TokenValue.getEvmValue) /= LT then
                                let
                                    ( txChainStatus, chainCmd ) =
                                        initiateCommitCall prevModel.web3Context trade userInfo.address userInfo.commPubkey
                                in
                                UpdateResult
                                    { newModel | txChainStatus = txChainStatus }
                                    Cmd.none
                                    chainCmd
                                    []

                            else
                                justModelUpdate newModel

                        _ ->
                            justModelUpdate newModel

                Err httpError ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.web3FetchError "allowance" httpError
                        ]

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
                                , Contracts.Wrappers.getParametersStateAndPhaseInfoCmd newModel.web3Context newCreationInfo.address ParametersFetched StateFetched PhaseInfoFetched
                                ]
                    in
                    UpdateResult
                        newModel
                        cmd
                        ChainCmd.none
                        []

                Err (Http.BadBody errstr) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.cantFindTradeWillRetry
                        ]

                Err httpError ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3FetchError "trade state" httpError ]

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
                    UpdateResult
                        newModel
                        (tryBuildDecryptCmd newModel)
                        ChainCmd.none
                        []

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.fromBadFetchResultMaybe "trade state" fetchResult
                        ]

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Ok parameters) ->
                    if CTypes.tradeHasDefaultParameters parameters then
                        let
                            newModel =
                                { prevModel
                                    | trade = prevModel.trade |> CTypes.updateParameters parameters
                                }
                        in
                        UpdateResult
                            newModel
                            (tryBuildDecryptCmd newModel)
                            ChainCmd.none
                            []

                    else
                        UpdateResult
                            prevModel
                            Cmd.none
                            ChainCmd.none
                            [ AppCmd.UserNotice UN.tradeParametersNotDefault ]

                Ok (Err s) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Error decoding fetched trade parameters" s
                        ]

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.web3FetchError "trade parameters" httpErr
                        ]

        PhaseInfoFetched fetchResult ->
            case fetchResult of
                Ok (Just phaseInfo) ->
                    let
                        newModel =
                            { prevModel
                                | trade = prevModel.trade |> CTypes.updatePhaseStartInfo phaseInfo
                            }
                    in
                    UpdateResult
                        newModel
                        (tryBuildDecryptCmd newModel)
                        ChainCmd.none
                        []

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.fromBadFetchResultMaybe "trade state" fetchResult
                        ]

        EventLogFetched log ->
            let
                decodedEventLog =
                    Eth.Decode.event CTypes.eventDecoder log
            in
            case decodedEventLog.returnData of
                Err err ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Error decoding contract event" err
                        ]

                Ok event ->
                    let
                        ( newTrade, maybeDecodeErrorNotice ) =
                            case event of
                                CTypes.InitiatedEvent data ->
                                    case CTypes.decodeTerms data.terms of
                                        Ok terms ->
                                            ( prevModel.trade
                                                |> CTypes.updateTerms terms
                                            , Nothing
                                            )

                                        Err s ->
                                            ( prevModel.trade
                                            , Just <| UN.unexpectedError "Couldn't decode payment methods!" s
                                            )

                                _ ->
                                    ( prevModel.trade
                                    , Nothing
                                    )

                        newSecureCommInfo =
                            case event of
                                CTypes.InitiatedEvent data ->
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
                                    tryInitChatHistory prevModel.web3Context newTrade prevModel.userInfo prevModel.eventsWaitingForChatHistory

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
                    UpdateResult
                        newModel
                        cmd
                        ChainCmd.none
                        ([ maybeDecodeErrorNotice ]
                            |> Maybe.Extra.values
                            |> List.map AppCmd.UserNotice
                        )

        ExpandPhase phase ->
            justModelUpdate { prevModel | expandedPhase = phase }

        ToggleChat ->
            let
                showChat =
                    if prevModel.showChatHistory then
                        False

                    else
                        True
            in
            justModelUpdate { prevModel | showChatHistory = showChat }

        ToggleStatsModal ->
            let
                showStatsModal =
                    if prevModel.showStatsModal then
                        False

                    else
                        True
            in
            justModelUpdate { prevModel | showStatsModal = showStatsModal }

        ViewUserHistory asRole ->
            case prevModel.trade of
                CTypes.LoadedTrade trade ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.GotoRoute (Routing.AgentHistory trade.parameters.initiatorAddress asRole) ]

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Trying to view a user's history for a not-yet-loaded Trade" prevModel.trade
                        ]

        CommitClicked trade userInfo depositAmount ->
            justModelUpdate { prevModel | txChainStatus = Just <| ConfirmingCommit trade userInfo depositAmount }

        AbortCommit ->
            justModelUpdate { prevModel | txChainStatus = Nothing }

        ConfirmCommit trade userInfo depositAmount ->
            let
                ( txChainStatus, chainCmd ) =
                    case prevModel.web3Context.factoryType of
                        Native _ ->
                            initiateCommitCall prevModel.web3Context trade userInfo.address userInfo.commPubkey

                        Token tokenType ->
                            let
                                approveChainCmd =
                                    let
                                        txParams =
                                            TokenContract.approve
                                                (Config.tokenContractAddress tokenType)
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
                                    if BigInt.compare allowance (CTypes.responderDeposit trade.parameters |> TokenValue.getEvmValue) /= LT then
                                        initiateCommitCall prevModel.web3Context trade userInfo.address userInfo.commPubkey

                                    else
                                        ( Just ApproveNeedsSig, approveChainCmd )

                                _ ->
                                    ( Just ApproveNeedsSig, approveChainCmd )
            in
            UpdateResult
                { prevModel | txChainStatus = txChainStatus }
                Cmd.none
                chainCmd
                []

        StartContractAction actionMsg ->
            let
                ( txChainStatus, chainCmd, appCmds ) =
                    case prevModel.trade of
                        CTypes.LoadedTrade tradeInfo ->
                            case actionMsg of
                                Recall ->
                                    let
                                        txParams =
                                            DHT.recall tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Recall
                                    , ChainCmd.custom (contractActionSend Recall) txParams
                                    , []
                                    )

                                Claim ->
                                    let
                                        txParams =
                                            DHT.claim tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Claim
                                    , ChainCmd.custom (contractActionSend Claim) txParams
                                    , []
                                    )

                                Abort ->
                                    let
                                        txParams =
                                            DHT.abort tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Abort
                                    , ChainCmd.custom (contractActionSend Abort) txParams
                                    , []
                                    )

                                Release ->
                                    let
                                        txParams =
                                            DHT.release tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Release
                                    , ChainCmd.custom (contractActionSend Release) txParams
                                    , []
                                    )

                                Burn ->
                                    let
                                        txParams =
                                            DHT.burn tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Burn
                                    , ChainCmd.custom (contractActionSend Burn) txParams
                                    , []
                                    )

                                Poke ->
                                    let
                                        txParams =
                                            DHT.poke tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ( Just <| ActionNeedsSig Poke
                                    , ChainCmd.custom (contractActionSend Poke) txParams
                                    , []
                                    )

                        tradeInfoNotYetLoaded ->
                            ( prevModel.txChainStatus
                            , ChainCmd.none
                            , [ AppCmd.UserNotice <|
                                    UN.unexpectedError "Trying to handle StartContractAction msg for a not-yet-loaded Trade" tradeInfoNotYetLoaded
                              ]
                            )
            in
            UpdateResult
                { prevModel
                    | txChainStatus = txChainStatus
                }
                Cmd.none
                chainCmd
                appCmds

        ApproveSigned txHashResult ->
            case txHashResult of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| ApproveMining txHash }

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.web3SigError "approve" s
                        ]

        CommitSigned txHashResult ->
            case txHashResult of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| CommitMining txHash }

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.web3SigError "commit" s
                        ]

        CommitMined _ ->
            justModelUpdate { prevModel | txChainStatus = Nothing }

        ActionSigned action txHashResult ->
            case txHashResult of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| ActionMining action txHash }

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.web3SigError (actionName action) s
                        ]

        ActionMined action _ ->
            justModelUpdate { prevModel | txChainStatus = Nothing }

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            UpdateResult
                { prevModel
                    | eventSentry =
                        newEventSentry
                }
                cmd
                ChainCmd.none
                []

        ChatHistoryMsg chatHistoryMsg ->
            case prevModel.chatHistoryModel of
                Just prevChatHistoryModel ->
                    let
                        updateResult =
                            ChatHistory.update chatHistoryMsg prevChatHistoryModel

                        ( encryptCmd, maybeUserNotice ) =
                            case updateResult.maybeMessageSubmit of
                                Just chatMessage ->
                                    case prevModel.secureCommInfo of
                                        LoadedCommInfo commInfo ->
                                            ( encryptToPubkeys (encodeEncryptionArgs chatMessage commInfo)
                                            , Nothing
                                            )

                                        incomplete ->
                                            ( Cmd.none
                                            , Just <|
                                                UN.unexpectedError "Trying to encrypt, but commInfo is not loaded" incomplete
                                            )

                                Nothing ->
                                    ( Cmd.none
                                    , Nothing
                                    )

                        model =
                            { prevModel | chatHistoryModel = Just updateResult.model }

                        decryptCmd =
                            if updateResult.shouldCallDecrypt then
                                tryBuildDecryptCmd prevModel

                            else
                                Cmd.none
                    in
                    UpdateResult
                        model
                        (Cmd.batch
                            [ decryptCmd
                            , encryptCmd
                            ]
                        )
                        ChainCmd.none
                        (AppCmd.mapList ChatHistoryMsg updateResult.appCmds
                            ++ (maybeUserNotice
                                    |> Maybe.map AppCmd.UserNotice
                                    |> Maybe.map List.singleton
                                    |> Maybe.withDefault []
                               )
                        )

                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Got a chat history message, but there is no chat history model!" chatHistoryMsg
                        ]

        EncryptionFinished encryptedMessagesValue ->
            let
                encodedEncryptionMessages =
                    decodeEncryptionResult encryptedMessagesValue
                        |> Result.andThen encodeEncryptedMessages
            in
            case ( prevModel.userInfo, prevModel.trade, encodedEncryptionMessages ) of
                ( Just userInfo, CTypes.LoadedTrade tradeInfo, Ok encodedEncryptedMessages ) ->
                    case CTypes.getInitiatorOrResponder tradeInfo userInfo.address of
                        Nothing ->
                            UpdateResult
                                prevModel
                                Cmd.none
                                ChainCmd.none
                                [ AppCmd.UserNotice <|
                                    UN.unexpectedError "Trying to encrypt, but the user is not involved in this trade." Nothing
                                ]

                        Just userRole ->
                            let
                                txParams =
                                    case userRole of
                                        Initiator ->
                                            DHT.initiatorStatement tradeInfo.creationInfo.address encodedEncryptedMessages
                                                |> Eth.toSend

                                        Responder ->
                                            DHT.responderStatement tradeInfo.creationInfo.address encodedEncryptedMessages
                                                |> Eth.toSend
                            in
                            UpdateResult
                                prevModel
                                Cmd.none
                                (ChainCmd.custom
                                    { onMined = Nothing
                                    , onSign = Nothing
                                    , onBroadcast = Nothing
                                    }
                                    txParams
                                )
                                []

                ( _, _, Err encodingErrStr ) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Error translating JS encryption result into Elm." encodingErrStr
                        ]

                ( maybeUserInfo, maybeTradeInfo, _ ) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Encryption successful, but the user or trade is no longer valid." ( maybeUserInfo, maybeTradeInfo )
                        ]

        MessageSubmitMined (Ok txReceipt) ->
            justModelUpdate prevModel

        MessageSubmitMined (Err s) ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.UserNotice <|
                    UN.web3MiningError "message" s
                ]


initiateCommitCall : EthHelpers.Web3Context -> CTypes.FullTradeInfo -> Address -> String -> ( Maybe TxChainStatus, ChainCmd Msg )
initiateCommitCall web3Context trade userAddress commPubkey =
    let
        commitConstructor =
            case web3Context.factoryType of
                Token _ ->
                    DHT.commit

                Native _ ->
                    DHNT.commit

        txParams =
            commitConstructor trade.creationInfo.address userAddress commPubkey
                |> (case web3Context.factoryType of
                        Token _ ->
                            identity

                        Native _ ->
                            EthHelpers.updateCallValue
                                (CTypes.responderDeposit trade.parameters |> TokenValue.getEvmValue)
                   )
                |> Eth.toSend
    in
    ( Just CommitNeedsSig
    , ChainCmd.custom
        { onMined = Just ( CommitMined, Nothing )
        , onSign = Just CommitSigned
        , onBroadcast = Nothing
        }
        txParams
    )


tryInitChatHistory : Web3Context -> CTypes.Trade -> Maybe UserInfo -> List ( Int, CTypes.DAIHardEvent ) -> ( Maybe ChatHistory.Model, Bool )
tryInitChatHistory web3Context maybeTrade maybeUserInfo pendingEvents =
    case ( maybeTrade, maybeUserInfo ) of
        ( CTypes.LoadedTrade tradeInfo, Just userInfo ) ->
            let
                maybeBuyerOrSeller =
                    CTypes.getBuyerOrSeller tradeInfo userInfo.address
            in
            case maybeBuyerOrSeller of
                Just buyerOrSeller ->
                    ChatHistory.init
                        web3Context
                        userInfo
                        buyerOrSeller
                        tradeInfo.parameters.initiatorRole
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
