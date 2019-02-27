module Interact.State exposing (init, subscriptions, update, updateUserInfo)

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
import Maybe.Extra
import RenderContract.Types
import Time
import TokenValue


init : EthHelpers.EthNode -> Address -> Address -> Int -> Maybe UserInfo -> BigInt -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode factoryAddress tokenAddress tokenDecimals userInfo ttId =
    let
        cmd =
            getContractCreationInfoCmd ethNode factoryAddress ttId
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , tokenAddress = tokenAddress
      , tokenDecimals = tokenDecimals
      , ttsInfo =
            { id = ttId
            , creationInfo = Nothing
            , parameters = Nothing
            , state = Nothing
            }
      , messages = []
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
            case ( model.ttsInfo.creationInfo, model.ttsInfo.parameters, model.eventSentries ) of
                ( Just creationInfo, Just state, Just sentries ) ->
                    ( model
                    , Cmd.batch
                        [ Contracts.Wrappers.getStateCmd model.ethNode model.tokenDecimals creationInfo.address StateFetched
                        , getSentryPollCmd sentries
                        ]
                    , ChainCmd.none
                    )

                ( _, _, _ ) ->
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
                                | ttsInfo =
                                    model.ttsInfo
                                        |> updateCreationInfo
                                            (Just newCreationInfo)
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
                    ( { model | ttsInfo = updateState model.ttsInfo (Just state) }, Cmd.none, ChainCmd.none )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    ( { model | ttsInfo = updateParameters model.ttsInfo (Just parameters) }, Cmd.none, ChainCmd.none )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        InitiatorStatementsFetched fetchResult ->
            let
                newModel =
                    case fetchResult of
                        Ok events ->
                            { model
                                | messages =
                                    addMessages
                                        (events
                                            |> List.map
                                                (\event ->
                                                    { who = Initiator
                                                    , message = "showing encrypted for initiator, for now: " ++ event.returnData.encryptedForInitiator
                                                    , blocknum = event.blockNumber
                                                    }
                                                )
                                        )
                                        model.messages
                            }

                        Err errstr ->
                            let
                                _ =
                                    Debug.log "error with initiator statement fetch" errstr
                            in
                            model
            in
            ( newModel
            , Cmd.none
            , ChainCmd.none
            )

        ResponderStatementsFetched fetchResult ->
            let
                newModel =
                    case fetchResult of
                        Ok events ->
                            { model
                                | messages =
                                    addMessages
                                        (events
                                            |> List.map
                                                (\event ->
                                                    { who = Responder
                                                    , message = "showing encrypted for initiator, for now: " ++ event.returnData.encryptedForInitiator
                                                    , blocknum = event.blockNumber
                                                    }
                                                )
                                        )
                                        model.messages
                            }

                        Err errstr ->
                            let
                                _ =
                                    Debug.log "error with initiator statement fetch" errstr
                            in
                            model
            in
            ( newModel
            , Cmd.none
            , ChainCmd.none
            )

        ContractAction actionMsg ->
            let
                chainCmd =
                    case ( model.ttsInfo.creationInfo, model.ttsInfo.parameters ) of
                        ( Nothing, _ ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but can't find the contract creationInfo :/" actionMsg
                            in
                            ChainCmd.none

                        ( _, Nothing ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but can't find the contract parameters :/" actionMsg
                            in
                            ChainCmd.none

                        ( Just creationInfo, Just parameters ) ->
                            case actionMsg of
                                RenderContract.Types.Recall ->
                                    let
                                        txParams =
                                            TTS.recall creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Commit ->
                                    let
                                        txParams =
                                            TokenContract.approve
                                                model.tokenAddress
                                                creationInfo.address
                                                (TokenValue.getBigInt parameters.responderDeposit)
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
                                            TTS.claim creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Release ->
                                    let
                                        txParams =
                                            TTS.release creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Burn ->
                                    let
                                        txParams =
                                            TTS.burn creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Poke ->
                                    let
                                        txParams =
                                            TTS.poke creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams
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
                    case ( model.ttsInfo.creationInfo, model.ttsInfo.parameters, model.userInfo ) of
                        ( Nothing, _, _ ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but can't find the contract creationInfo :/" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        ( _, Nothing, _ ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but can't find the contract parameters :/" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        ( _, _, Nothing ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but can't find userInfo :/" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        ( Just creationInfo, Just parameters, Just userInfo ) ->
                            let
                                txParams =
                                    TTS.commit creationInfo.address userInfo.commPubkey
                                        |> Eth.toSend
                            in
                            ( model, Cmd.none, ChainCmd.custom genericCustomSend txParams )

        MessageInputChanged newMessageStr ->
            ( { model | messageInput = newMessageStr }
            , Cmd.none
            , ChainCmd.none
            )

        MessageSubmit ->
            let
                userAddress =
                    model.userInfo
                        |> Maybe.map (\userInfo -> userInfo.address)

                userRole =
                    Maybe.map3 getUserRole model.ttsInfo.parameters model.ttsInfo.state userAddress
                        |> Maybe.Extra.join

                chainCmd =
                    case ( userRole, model.ttsInfo.creationInfo ) of
                        ( Just role, Just creationInfo ) ->
                            let
                                txParams =
                                    case role of
                                        Initiator ->
                                            TTS.initiatorStatement creationInfo.address model.messageInput "wait, this should be encrypted!!"
                                                |> Eth.toSend

                                        Responder ->
                                            TTS.responderStatement creationInfo.address model.messageInput "wait, this should be encrypted!!"
                                                |> Eth.toSend
                            in
                            ChainCmd.custom genericCustomSend txParams

                        _ ->
                            let
                                _ =
                                    Debug.log "MessageSubmit called, but we're missing some crucial info! HOW DID THIS HAPPEN" ( userRole, model.ttsInfo.creationInfo )
                            in
                            ChainCmd.none
            in
            ( model, Cmd.none, chainCmd )

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


getSentryPollCmd : ( EventSentry TTS.InitiatorStatementLog Msg, EventSentry TTS.ResponderStatementLog Msg ) -> Cmd Msg
getSentryPollCmd sentries =
    Cmd.batch
        [ EventSentryHack.pollForChanges (Tuple.first sentries)
        , EventSentryHack.pollForChanges (Tuple.second sentries)
        ]


addMessages : List CommMessage -> List CommMessage -> List CommMessage
addMessages newMessages messageList =
    List.append newMessages messageList
        |> List.sortBy .blocknum


genericCustomSend =
    { onMined = Just ( ContractActionMined, Nothing )
    , onSign = Nothing
    , onBroadcast = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 3000 Refresh
