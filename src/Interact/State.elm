module Interact.State exposing (init, subscriptions, update, updateWithUserAddress)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import EventHack
import Http
import Interact.Types exposing (..)
import RenderContract.Types
import Time
import TokenValue


init : EthHelpers.EthNode -> Address -> Address -> Int -> Maybe Address -> BigInt -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode factoryAddress tokenAddress tokenDecimals userAddress ttId =
    let
        cmd =
            getContractCreationInfoCmd ethNode factoryAddress ttId
    in
    ( { ethNode = ethNode
      , userAddress = userAddress
      , tokenAddress = tokenAddress
      , tokenDecimals = tokenDecimals
      , idInput = BigInt.toString ttId
      , ttsInfo =
            { id = ttId
            , address = Nothing
            , parameters = Nothing
            , state = Nothing
            }
      }
    , cmd
    , ChainCmd.none
    )


getContractCreationInfoCmd : EthHelpers.EthNode -> Address -> BigInt -> Cmd Msg
getContractCreationInfoCmd ethNode factoryAddress id =
    Contracts.Wrappers.getCreationInfoFromIdCmd ethNode factoryAddress id CreationInfoFetched


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        Refresh time ->
            case ( model.ttsInfo.address, model.ttsInfo.parameters ) of
                ( Just address, Just state ) ->
                    ( model
                    , Contracts.Wrappers.getStateCmd model.ethNode model.tokenDecimals address StateFetched
                    , ChainCmd.none
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, ChainCmd.none )

        TestResult fetchResult ->
            let
                _ =
                    Debug.log "chats" fetchResult
            in
            ( model, Cmd.none, ChainCmd.none )

        CreationInfoFetched fetchResult ->
            case fetchResult of
                Ok creationInfo ->
                    ( { model | ttsInfo = updateAddress model.ttsInfo (Just creationInfo.address_) }
                    , Cmd.batch
                        [ Contracts.Wrappers.getParametersAndStateCmd model.ethNode model.tokenDecimals creationInfo.address_ ParametersFetched StateFetched
                        , EventHack.fetchEventLogs
                            model.ethNode.http
                            creationInfo.address_
                            TTS.initiatorStatementLogEvent
                            ( Eth.Types.BlockNum 10406687, Eth.Types.BlockNum 10406687 )
                            TTS.initiatorStatementLogDecoder
                            TestResult
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

        ContractAction actionMsg ->
            let
                chainCmd =
                    case ( model.ttsInfo.address, model.ttsInfo.parameters ) of
                        ( Nothing, _ ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but can't find the contract address :/" actionMsg
                            in
                            ChainCmd.none

                        ( _, Nothing ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle ContractAction msg, but can't find the contract parameters :/" actionMsg
                            in
                            ChainCmd.none

                        ( Just ttsAddress, Just parameters ) ->
                            case actionMsg of
                                RenderContract.Types.Recall ->
                                    let
                                        txParams =
                                            TTS.recall ttsAddress
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Commit ->
                                    let
                                        txParams =
                                            TokenContract.approve
                                                model.tokenAddress
                                                ttsAddress
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
                                            TTS.claim ttsAddress ""
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Release ->
                                    let
                                        txParams =
                                            TTS.release ttsAddress
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Burn ->
                                    let
                                        txParams =
                                            TTS.burn ttsAddress ""
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                RenderContract.Types.Poke ->
                                    let
                                        txParams =
                                            TTS.poke ttsAddress
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
                    case ( model.ttsInfo.address, model.ttsInfo.parameters ) of
                        ( Nothing, _ ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but can't find the contract address :/" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        ( _, Nothing ) ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but can't find the contract parameters :/" ""
                            in
                            ( model, Cmd.none, ChainCmd.none )

                        ( Just ttsAddress, Just parameters ) ->
                            let
                                txParams =
                                    TTS.commit ttsAddress ""
                                        |> Eth.toSend
                            in
                            ( model, Cmd.none, ChainCmd.custom genericCustomSend txParams )


genericCustomSend =
    { onMined = Just ( ContractActionMined, Nothing )
    , onSign = Nothing
    , onBroadcast = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 3000 Refresh
