module Interact.State exposing (getContractInfoCmd, handleBadFetchResult, init, update, updateWithUserAddress)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Generated.ToastytradeSell as TTS
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Http
import Interact.Types exposing (..)
import RenderContract.Types
import TokenValue


init : EthHelpers.EthNode -> Address -> Address -> Int -> Maybe Address -> BigInt -> ( Model, Cmd Msg, ChainCmd Msg )
init ethNode factoryAddress tokenAddress tokenDecimals userAddress ttId =
    let
        cmd =
            getContractAddressCmd ethNode factoryAddress ttId
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


getContractAddressCmd : EthHelpers.EthNode -> Address -> BigInt -> Cmd Msg
getContractAddressCmd ethNode factoryAddress id =
    Contracts.Wrappers.getAddressFromIdCmd ethNode factoryAddress id AddressFetched


getContractInfoCmd : EthHelpers.EthNode -> Int -> Address -> Cmd Msg
getContractInfoCmd ethNode tokenDecimals address =
    Cmd.batch
        [ Contracts.Wrappers.getStateCmd ethNode tokenDecimals address StateFetched
        , Contracts.Wrappers.getParametersCmd ethNode tokenDecimals address ParametersFetched
        ]


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    { model | userAddress = userAddress }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        AddressFetched fetchResult ->
            case Debug.log "res" fetchResult of
                Ok address ->
                    ( { model | ttsInfo = updateAddress model.ttsInfo (Just address) }
                    , getContractInfoCmd model.ethNode model.tokenDecimals address
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
                    handleBadFetchResult model fetchResult

        ParametersFetched fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    ( { model | ttsInfo = updateParameters model.ttsInfo (Just parameters) }, Cmd.none, ChainCmd.none )

                _ ->
                    handleBadFetchResult model fetchResult

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


handleBadFetchResult : Model -> Result Http.Error (Maybe a) -> ( Model, Cmd Msg, ChainCmd Msg )
handleBadFetchResult model fetchResult =
    case fetchResult of
        Ok (Just a) ->
            let
                _ =
                    Debug.log "I'm confused about whether this is a bad fetch result or not!."
            in
            ( model, Cmd.none, ChainCmd.none )

        Ok Nothing ->
            let
                _ =
                    Debug.log "The data was fetched, but could not be decoded."
            in
            ( model, Cmd.none, ChainCmd.none )

        Err errstr ->
            let
                _ =
                    Debug.log "can't fetch full state: " errstr
            in
            ( model, Cmd.none, ChainCmd.none )


genericCustomSend =
    { onMined = Just ( ContractActionMined, Nothing )
    , onSign = Nothing
    , onBroadcast = Nothing
    }
