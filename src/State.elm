port module State exposing (init, subscriptions, update)

import AgentHistory.State
import AppCmd exposing (AppCmd)
import BigInt
import Browser
import Browser.Dom
import Browser.Navigation
import CommonTypes exposing (..)
import Config
import Create.State
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Json.Decode
import Json.Encode
import Marketplace.State
import Routing
import Time
import Trade.State
import TradeCache.State as TradeCache
import TradeCache.Types exposing (TradeCache)
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    if flags.width < 1024 then
        ( Failed "Sorry, your screen size (< 1024 width) is not supported."
        , Cmd.none
        )

    else
        case EthHelpers.intToFactoryType flags.networkId of
            Nothing ->
                ( Failed "Your provider (Metamask?) is set to an unsupported network. Switch to Kovan, Mainnet, or XDAI and refresh."
                , Cmd.none
                )

            Just factoryType ->
                let
                    web3Context =
                        EthHelpers.web3Context factoryType

                    txSentry =
                        TxSentry.init ( txOut, txIn ) TxSentryMsg web3Context.httpProvider

                    ( tradeCache, tcCmd ) =
                        TradeCache.initAndStartCaching web3Context

                    ( model, fromUrlCmd ) =
                        { key = key
                        , time = Time.millisToPosix 0
                        , web3Context = web3Context
                        , txSentry = txSentry
                        , userAddress = Nothing
                        , userInfo = Nothing
                        , tradeCache = tradeCache
                        , submodel = BetaLandingPage
                        }
                            |> updateFromUrl url
                in
                ( model
                , Cmd.batch
                    [ Cmd.map TradeCacheMsg tcCmd
                    , fromUrlCmd
                    ]
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg maybeValidModel =
    case maybeValidModel of
        Running model ->
            updateValidModel msg model

        Failed _ ->
            ( maybeValidModel, Cmd.none )


type alias EncryptedMessage =
    { encapsulatedKey : String
    , iv : String
    , tag : String
    , message : String
    }


updateValidModel : Msg -> ValidModel -> ( Model, Cmd Msg )
updateValidModel msg model =
    case msg of
        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl model.key (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( Running model, cmd )

        UrlChanged url ->
            model |> updateFromUrl url

        GotoRoute route ->
            ( Running model
            , beginRouteChange model.key route
            )

        Tick newTime ->
            ( Running { model | time = newTime }, Cmd.none )

        NetworkUpdate newNetworkValue ->
            let
                _ =
                    Debug.log "hi!" ""

                newNetworkIdResult =
                    Json.Decode.decodeValue Json.Decode.int newNetworkValue
                        |> Result.map Eth.Net.toNetworkId

                maybeNewFactoryType =
                    newNetworkIdResult
                        |> Result.toMaybe
                        |> Maybe.andThen EthHelpers.networkIdToFactoryType
            in
            case ( newNetworkIdResult, maybeNewFactoryType ) of
                ( Ok newNetworkId, Just newFactoryType ) ->
                    if newNetworkId /= EthHelpers.factoryTypeToNetworkId model.web3Context.factoryType then
                        let
                            newWeb3Context =
                                EthHelpers.web3Context newFactoryType

                            ( submodel, submodelCmd, maybeRoute ) =
                                model.submodel |> updateSubmodelWeb3Context newWeb3Context

                            ( newTradeCache, tradeCacheCmd ) =
                                TradeCache.initAndStartCaching newWeb3Context
                        in
                        ( Running
                            { model
                                | submodel = submodel
                                , tradeCache = newTradeCache
                                , web3Context = newWeb3Context
                            }
                        , Cmd.batch
                            [ Cmd.map TradeCacheMsg tradeCacheCmd
                            , case maybeRoute of
                                Just newRoute ->
                                    beginRouteChange model.key newRoute

                                Nothing ->
                                    submodelCmd
                            ]
                        )

                    else
                        ( Running model
                        , Cmd.none
                        )

                _ ->
                    let
                        _ =
                            Debug.log "Error decoding networkID or determining FactoryType from JS value" ( newNetworkIdResult, maybeNewFactoryType )
                    in
                    ( Running model, Cmd.none )

        ConnectToWeb3 ->
            ( Running model
            , connectToWeb3 ()
            )

        WalletStatus walletSentry ->
            let
                genCommPubkeyCmd =
                    case walletSentry.account of
                        Nothing ->
                            Cmd.none

                        Just address ->
                            genPrivkey <|
                                encodeGenPrivkeyArgs
                                    address
                                    "Deriving keypair for encrypted communication on the DAIHard exchange. ONLY SIGN THIS on https://burnable-tech.github.io/DAIHard/. If you sign this elsewhere, you risk revealing any of your encrypted communication on DAIHard to an attacker."
            in
            ( Running
                { model
                    | userAddress = walletSentry.account
                }
            , genCommPubkeyCmd
            )

        UserPubkeySet commPubkeyValue ->
            case Json.Decode.decodeValue Json.Decode.string commPubkeyValue of
                Ok commPubkey ->
                    case model.userAddress of
                        Just userAddress ->
                            let
                                userInfo =
                                    Just
                                        { address = userAddress
                                        , commPubkey = commPubkey
                                        }

                                ( submodel, cmd ) =
                                    model.submodel |> updateSubmodelUserInfo userInfo
                            in
                            ( Running
                                { model
                                    | userInfo = userInfo
                                    , submodel = submodel
                                }
                            , cmd
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "User pubkey set, but I can no longer find the user address!" ""
                            in
                            ( Running model, Cmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "error decoding commPubkey from JS" errstr
                    in
                    ( Running model, Cmd.none )

        CreateMsg createMsg ->
            case model.submodel of
                CreateModel createModel ->
                    let
                        updateResult =
                            Create.State.update createMsg createModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map CreateMsg updateResult.chainCmd)
                    in
                    ( Running
                        { model
                            | submodel = CreateModel updateResult.model
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map CreateMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds updateResult.appCmds

                _ ->
                    ( Running model, Cmd.none )

        TradeMsg tradeMsg ->
            case model.submodel of
                TradeModel tradeModel ->
                    let
                        updateResult =
                            Trade.State.update tradeMsg tradeModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map TradeMsg updateResult.chainCmd)
                    in
                    ( Running
                        { model
                            | submodel = TradeModel updateResult.model
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map TradeMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds updateResult.appCmds

                _ ->
                    ( Running model, Cmd.none )

        MarketplaceMsg marketplaceMsg ->
            case model.submodel of
                MarketplaceModel marketplaceModel ->
                    let
                        updateResult =
                            Marketplace.State.update marketplaceMsg marketplaceModel
                    in
                    ( Running { model | submodel = MarketplaceModel updateResult.model }
                    , Cmd.map MarketplaceMsg updateResult.cmd
                    )
                        |> runAppCmds updateResult.appCmds

                _ ->
                    ( Running model, Cmd.none )

        AgentHistoryMsg agentHistoryMsg ->
            case model.submodel of
                AgentHistoryModel agentHistoryModel ->
                    let
                        updateResult =
                            AgentHistory.State.update agentHistoryMsg agentHistoryModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map AgentHistoryMsg updateResult.chainCmd)
                    in
                    ( Running
                        { model
                            | submodel = AgentHistoryModel updateResult.model
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map AgentHistoryMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds updateResult.appCmds

                _ ->
                    ( Running model, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( submodel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( Running { model | txSentry = submodel }, subCmd )

        TradeCacheMsg tradeCacheMsg ->
            let
                ( newTradeCache, tcCmd ) =
                    TradeCache.update
                        tradeCacheMsg
                        model.tradeCache
            in
            ( Running { model | tradeCache = newTradeCache }
            , tcCmd |> Cmd.map TradeCacheMsg
            )

        Fail str ->
            ( Failed str, Cmd.none )

        NoOp ->
            ( Running model, Cmd.none )


runAppCmds : List AppCmd -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runAppCmds appCmds ( maybeValidModel, prevCmd ) =
    List.foldl
        runAppCmd
        ( maybeValidModel, prevCmd )
        appCmds


runAppCmd : AppCmd -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runAppCmd appCmd ( maybeValidModel, prevCmd ) =
    case maybeValidModel of
        Failed _ ->
            ( maybeValidModel, prevCmd )

        Running prevModel ->
            case appCmd of
                AppCmd.Web3Connect ->
                    ( Running prevModel
                    , Cmd.batch
                        [ prevCmd
                        , connectToWeb3 ()
                        ]
                    )

                AppCmd.GotoRoute newRoute ->
                    ( Running prevModel
                    , beginRouteChange prevModel.key newRoute
                    )


encodeGenPrivkeyArgs : Address -> String -> Json.Decode.Value
encodeGenPrivkeyArgs address signMsg =
    Json.Encode.object
        [ ( "address", Json.Encode.string <| Eth.Utils.addressToString address )
        , ( "signSeedMsg", Json.Encode.string signMsg )
        ]


beginRouteChange : Browser.Navigation.Key -> Routing.Route -> Cmd Msg
beginRouteChange key route =
    Browser.Navigation.pushUrl key (Routing.routeToString route)


updateFromUrl : Url -> ValidModel -> ( Model, Cmd Msg )
updateFromUrl url model =
    gotoRoute model (Routing.urlToRoute url)


gotoRoute : ValidModel -> Routing.Route -> ( Model, Cmd Msg )
gotoRoute oldModel route =
    let
        newUrlString =
            Routing.routeToString route
    in
    case route of
        Routing.Home ->
            ( Running
                { oldModel
                    | submodel = BetaLandingPage
                }
            , Cmd.none
            )

        Routing.Create ->
            let
                updateResult =
                    Create.State.init oldModel.web3Context oldModel.userInfo

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute oldModel.txSentry (ChainCmd.map CreateMsg updateResult.chainCmd)
            in
            ( Running
                { oldModel
                    | submodel = CreateModel updateResult.model
                    , txSentry = newTxSentry
                }
            , Cmd.batch
                [ Cmd.map CreateMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runAppCmds updateResult.appCmds

        Routing.Trade id ->
            let
                updateResult =
                    Trade.State.init oldModel.web3Context oldModel.userInfo id

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute oldModel.txSentry (ChainCmd.map TradeMsg updateResult.chainCmd)
            in
            ( Running
                { oldModel
                    | submodel = TradeModel updateResult.model
                    , txSentry = newTxSentry
                }
            , Cmd.batch
                [ Cmd.map TradeMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runAppCmds updateResult.appCmds

        Routing.Marketplace browsingRole ->
            let
                ( marketplaceModel, marketplaceCmd ) =
                    Marketplace.State.init oldModel.web3Context browsingRole oldModel.userInfo
            in
            ( Running
                { oldModel
                    | submodel = MarketplaceModel marketplaceModel
                }
            , Cmd.batch
                [ Cmd.map MarketplaceMsg marketplaceCmd
                ]
            )

        Routing.AgentHistory address agentRole ->
            let
                ( agentHistoryModel, agentHistoryCmd ) =
                    AgentHistory.State.init oldModel.web3Context address agentRole oldModel.userInfo
            in
            ( Running
                { oldModel
                    | submodel = AgentHistoryModel agentHistoryModel
                }
            , Cmd.batch
                [ Cmd.map AgentHistoryMsg agentHistoryCmd
                ]
            )

        Routing.NotFound ->
            ( Failed "Don't understand that url...", Browser.Navigation.pushUrl oldModel.key newUrlString )


updateSubmodelUserInfo : Maybe UserInfo -> Submodel -> ( Submodel, Cmd Msg )
updateSubmodelUserInfo userInfo submodel =
    case submodel of
        BetaLandingPage ->
            ( submodel
            , Cmd.none
            )

        CreateModel createModel ->
            let
                ( newCreateModel, createCmd ) =
                    createModel |> Create.State.updateUserInfo userInfo
            in
            ( CreateModel newCreateModel
            , Cmd.map CreateMsg createCmd
            )

        TradeModel tradeModel ->
            let
                ( newTradeModel, tradeCmd ) =
                    tradeModel |> Trade.State.updateUserInfo userInfo
            in
            ( TradeModel newTradeModel
            , Cmd.map TradeMsg tradeCmd
            )

        MarketplaceModel marketplaceModel ->
            ( MarketplaceModel (marketplaceModel |> Marketplace.State.updateUserInfo userInfo)
            , Cmd.none
            )

        AgentHistoryModel agentHistoryModel ->
            ( AgentHistoryModel (agentHistoryModel |> AgentHistory.State.updateUserInfo userInfo)
            , Cmd.none
            )


updateSubmodelWeb3Context : EthHelpers.Web3Context -> Submodel -> ( Submodel, Cmd Msg, Maybe Routing.Route )
updateSubmodelWeb3Context newWeb3Context submodel =
    case submodel of
        BetaLandingPage ->
            ( submodel, Cmd.none, Nothing )

        CreateModel createModel ->
            ( CreateModel (createModel |> Create.State.updateWeb3Context newWeb3Context)
            , Cmd.none
            , Nothing
            )

        TradeModel tradeModel ->
            -- Doesn't make sense to look at the same trade on a new network
            -- so just redirect to marketplace
            ( submodel
            , Cmd.none
            , Just <| Routing.Marketplace Buyer
            )

        MarketplaceModel marketplaceModel ->
            ( MarketplaceModel (marketplaceModel |> Marketplace.State.updateWeb3Context newWeb3Context)
            , Cmd.none
            , Nothing
            )

        AgentHistoryModel agentHistoryModel ->
            ( AgentHistoryModel (agentHistoryModel |> AgentHistory.State.updateWeb3Context newWeb3Context)
            , Cmd.none
            , Nothing
            )


subscriptions : Model -> Sub Msg
subscriptions maybeValidModel =
    case maybeValidModel of
        Running model ->
            Sub.batch
                ([ Time.every 1000 Tick
                 , walletSentryPort (WalletSentry.decodeToMsg Fail WalletStatus)
                 , TxSentry.listen model.txSentry
                 , userPubkeyResult UserPubkeySet
                 , Sub.map TradeCacheMsg <| TradeCache.subscriptions model.tradeCache
                 , networkSentryPort NetworkUpdate
                 ]
                    ++ [ submodelSubscriptions model ]
                )

        Failed _ ->
            Sub.none


submodelSubscriptions : ValidModel -> Sub Msg
submodelSubscriptions model =
    case model.submodel of
        BetaLandingPage ->
            Sub.none

        CreateModel createModel ->
            Sub.map CreateMsg <| Create.State.subscriptions createModel

        TradeModel tradeModel ->
            Sub.map TradeMsg <| Trade.State.subscriptions tradeModel

        MarketplaceModel marketplaceModel ->
            Sub.map MarketplaceMsg <| Marketplace.State.subscriptions marketplaceModel

        AgentHistoryModel agentHistoryModel ->
            Sub.map AgentHistoryMsg <| AgentHistory.State.subscriptions agentHistoryModel


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port networkSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port genPrivkey : Json.Decode.Value -> Cmd msg


port userPubkeyResult : (Json.Decode.Value -> msg) -> Sub msg
