module Trade.State exposing (init, subscriptions, update, updateUserInfo)

import Array
import BigInt exposing (BigInt)
import BigIntHelpers
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Constants exposing (..)
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types
import Contracts.Wrappers
import Eth
import Eth.Decode
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Json.Decode
import Json.Encode
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Result.Extra
import Time
import TokenValue
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
      , trade = Contracts.Types.partialTradeInfo tradeId
      , stats = Waiting
      , eventSentry = eventSentry
      }
    , Cmd.batch [ getCreationInfoCmd, eventSentryCmd ]
    , ChainCmd.none
    )


getContractCreationInfoCmd : EthHelpers.EthNode -> Int -> Cmd Msg
getContractCreationInfoCmd ethNode id =
    Contracts.Wrappers.getCreationInfoFromIdCmd ethNode (BigInt.fromInt id) CreationInfoFetched


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


update : Msg -> Model -> ( Model, Cmd Msg, ChainCmd Msg )
update msg model =
    case msg of
        Refresh time ->
            case model.trade of
                Contracts.Types.LoadedTrade tradeInfo ->
                    ( model
                    , Cmd.batch
                        [ Contracts.Wrappers.getStateCmd model.ethNode tradeInfo.creationInfo.address StateFetched

                        --, tryBuildDecryptCmd model
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

                        ( newSentry, sentryCmd, _ ) =
                            EventSentry.watch
                                EventLogFetched
                                model.eventSentry
                                { address = newCreationInfo.address
                                , fromBlock = Eth.Types.BlockNum newCreationInfo.blocknum
                                , toBlock = Eth.Types.LatestBlock
                                , topics = []
                                }

                        newModel =
                            { model
                                | trade = model.trade |> Contracts.Types.updateCreationInfo newCreationInfo
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
                        newModel =
                            { model
                                | trade = model.trade |> Contracts.Types.updateState state
                            }
                    in
                    ( newModel
                    , Cmd.none
                      --, tryBuildDecryptCmd newModel
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
                        newModel =
                            { model
                                | trade = model.trade |> Contracts.Types.updateParameters parameters
                            }
                    in
                    ( newModel
                    , Cmd.none
                      --, tryBuildDecryptCmd newModel
                    , ChainCmd.none
                    )

                badResult ->
                    let
                        _ =
                            Debug.log "bad parametersFetched result" badResult
                    in
                    ( model, Cmd.none, ChainCmd.none )

        EventLogFetched log ->
            let
                ( newModel, cmd ) =
                    handleNewLog log model
            in
            ( newModel, cmd, ChainCmd.none )

        ContractAction actionMsg ->
            let
                chainCmd =
                    case model.trade of
                        Contracts.Types.LoadedTrade tradeInfo ->
                            case actionMsg of
                                Recall ->
                                    let
                                        txParams =
                                            DHT.recall tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                Commit ->
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
                                                daiAddress
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

                                Claim ->
                                    let
                                        txParams =
                                            DHT.claim tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                Abort ->
                                    let
                                        txParams =
                                            DHT.abort tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                Release ->
                                    let
                                        txParams =
                                            DHT.release tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                Burn ->
                                    let
                                        txParams =
                                            DHT.burn tradeInfo.creationInfo.address
                                                |> Eth.toSend
                                    in
                                    ChainCmd.custom genericCustomSend txParams

                                Poke ->
                                    let
                                        txParams =
                                            DHT.poke tradeInfo.creationInfo.address
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
                        ( Contracts.Types.LoadedTrade tradeInfo, Just userInfo ) ->
                            let
                                txParams =
                                    DHT.commit tradeInfo.creationInfo.address userInfo.commPubkey
                                        |> Eth.toSend
                            in
                            ( model, Cmd.none, ChainCmd.custom genericCustomSend txParams )

                        incomplete ->
                            let
                                _ =
                                    Debug.log "Trying to handle PreCommitApproveMined, but missing crucial info" incomplete
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
        decodedEventLog =
            Eth.Decode.event Contracts.Types.eventDecoder log
    in
    case decodedEventLog.returnData of
        Err err ->
            let
                _ =
                    Debug.log "Error decoding contract event" err
            in
            ( model, Cmd.none )

        Ok event ->
            let
                newModel =
                    { model
                        | trade =
                            case event of
                                Contracts.Types.OpenedEvent data ->
                                    model.trade
                                        |> Contracts.Types.updatePaymentMethods
                                            (PaymentMethods.decodePaymentMethodList data.fiatTransferMethods)

                                _ ->
                                    model.trade
                    }
            in
            ( newModel
            , Cmd.none
            )


genericCustomSend =
    { onMined = Just ( ContractActionMined, Nothing )
    , onSign = Nothing
    , onBroadcast = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 7000 Refresh
        ]
