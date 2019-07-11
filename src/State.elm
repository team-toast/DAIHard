port module State exposing (init, subscriptions, update)

import AgentHistory.State
import AppCmd
import BigInt
import Browser
import Browser.Dom
import Browser.Navigation
import CommonTypes exposing (..)
import Config
import Create.State
import Element
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Json.Decode
import Json.Encode
import List.Extra
import Marketplace.State
import Maybe.Extra
import Routing
import Time
import Trade.State
import TradeCache.State as TradeCache
import TradeCache.Types exposing (TradeCache)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        tooSmallNotice =
            if flags.width < 1024 then
                Just UN.screenToSmall

            else
                Nothing

        ( factoryType, initialWeb3State ) =
            if flags.networkId == 0 then
                ( Native XDai
                , NoWeb3
                )

            else
                case EthHelpers.intToFactoryType flags.networkId of
                    Nothing ->
                        ( Native XDai
                        , WrongNetwork
                        )

                    Just factoryType_ ->
                        ( factoryType_
                        , AllGood
                        )

        providerNotice =
            case initialWeb3State of
                NoWeb3 ->
                    Just UN.noWeb3Provider

                WrongNetwork ->
                    Just UN.wrongWeb3Network

                AllGood ->
                    Nothing

        userNotices =
            Maybe.Extra.values
                [ tooSmallNotice, providerNotice ]

        web3Context =
            EthHelpers.web3Context factoryType

        txSentry =
            TxSentry.init ( txOut, txIn ) TxSentryMsg web3Context.httpProvider

        ( tradeCache, tcCmd ) =
            TradeCache.initAndStartCaching web3Context

        ( model, fromUrlCmd ) =
            { key = key
            , initialWeb3State = initialWeb3State
            , time = Time.millisToPosix 0
            , web3Context = web3Context
            , txSentry = txSentry
            , userAddress = Nothing
            , userInfo = Nothing
            , tradeCache = tradeCache
            , submodel = BetaLandingPage
            , userNotices = []
            , screenWidth = flags.width
            }
                |> updateFromUrl url
    in
    ( model |> addUserNotices userNotices
    , Cmd.batch
        [ Cmd.map TradeCacheMsg tcCmd
        , fromUrlCmd
        ]
    )


type alias EncryptedMessage =
    { encapsulatedKey : String
    , iv : String
    , tag : String
    , message : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppCmd appCmd ->
            case appCmd of
                AppCmd.Web3Connect ->
                    model
                        |> update ConnectToWeb3

                AppCmd.GotoRoute newRoute ->
                    ( model
                    , beginRouteChange model.key newRoute
                    )

                AppCmd.GTag gtag ->
                    ( model
                    , gTagOut (encodeGTag gtag)
                    )

                AppCmd.UserNotice userNotice ->
                    ( model |> addUserNotice userNotice
                    , Cmd.none
                    )

        DismissNotice id ->
            ( { model
                | userNotices =
                    model.userNotices |> List.Extra.removeAt id
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            let
                cmd =
                    case urlRequest of
                        Browser.Internal url ->
                            Browser.Navigation.pushUrl model.key (Url.toString url)

                        Browser.External href ->
                            Browser.Navigation.load href
            in
            ( model, cmd )

        UrlChanged url ->
            model |> updateFromUrl url

        GotoRoute route ->
            ( model
            , beginRouteChange model.key route
            )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        NetworkUpdate newNetworkValue ->
            let
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
                        ( { model
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
                        ( model
                        , Cmd.none
                        )

                ( Err jsonDecodeError, _ ) ->
                    ( model
                        |> (addUserNotice <| UN.unexpectedError "Can't decode networkID from Javascript" jsonDecodeError)
                    , Cmd.none
                    )

                ( _, Nothing ) ->
                    ( model
                        |> addUserNotice
                            UN.wrongWeb3Network
                    , Cmd.none
                    )

        ConnectToWeb3 ->
            case model.initialWeb3State of
                NoWeb3 ->
                    ( model |> addUserNotice UN.cantConnectNoWeb3
                    , Cmd.none
                    )

                _ ->
                    ( model
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
            ( { model
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
                            ( { model
                                | userInfo = userInfo
                                , submodel = submodel
                              }
                            , cmd
                            )

                        Nothing ->
                            ( model
                                |> addUserNotice
                                    (UN.unexpectedError
                                        "User pubkey set, but I can no longer find the user address!"
                                        Nothing
                                    )
                            , Cmd.none
                            )

                Err s ->
                    ( model |> (addUserNotice <| UN.unexpectedError "error decoding commPubkey from JS" s)
                    , Cmd.none
                    )

        CreateMsg createMsg ->
            case model.submodel of
                CreateModel createModel ->
                    let
                        updateResult =
                            Create.State.update createMsg createModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map CreateMsg updateResult.chainCmd)
                    in
                    ( { model
                        | submodel = CreateModel updateResult.model
                        , txSentry = newTxSentry
                      }
                    , Cmd.batch
                        [ Cmd.map CreateMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds
                            (AppCmd.mapList CreateMsg updateResult.appCmds)

                _ ->
                    ( model, Cmd.none )

        TradeMsg tradeMsg ->
            case model.submodel of
                TradeModel tradeModel ->
                    let
                        updateResult =
                            Trade.State.update tradeMsg tradeModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map TradeMsg updateResult.chainCmd)
                    in
                    ( { model
                        | submodel = TradeModel updateResult.model
                        , txSentry = newTxSentry
                      }
                    , Cmd.batch
                        [ Cmd.map TradeMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds
                            (AppCmd.mapList TradeMsg updateResult.appCmds)

                _ ->
                    ( model, Cmd.none )

        MarketplaceMsg marketplaceMsg ->
            case model.submodel of
                MarketplaceModel marketplaceModel ->
                    let
                        updateResult =
                            Marketplace.State.update marketplaceMsg marketplaceModel
                    in
                    ( { model | submodel = MarketplaceModel updateResult.model }
                    , Cmd.map MarketplaceMsg updateResult.cmd
                    )
                        |> runAppCmds
                            (AppCmd.mapList MarketplaceMsg updateResult.appCmds)

                _ ->
                    ( model, Cmd.none )

        AgentHistoryMsg agentHistoryMsg ->
            case model.submodel of
                AgentHistoryModel agentHistoryModel ->
                    let
                        updateResult =
                            AgentHistory.State.update agentHistoryMsg agentHistoryModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map AgentHistoryMsg updateResult.chainCmd)
                    in
                    ( { model
                        | submodel = AgentHistoryModel updateResult.model
                        , txSentry = newTxSentry
                      }
                    , Cmd.batch
                        [ Cmd.map AgentHistoryMsg updateResult.cmd
                        , chainCmd
                        ]
                    )
                        |> runAppCmds
                            (AppCmd.mapList AgentHistoryMsg updateResult.appCmds)

                _ ->
                    ( model, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( submodel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = submodel }, subCmd )

        TradeCacheMsg tradeCacheMsg ->
            let
                updateResult =
                    TradeCache.update
                        tradeCacheMsg
                        model.tradeCache
            in
            ( { model | tradeCache = updateResult.tradeCache }
            , updateResult.cmd |> Cmd.map TradeCacheMsg
            )

        NoOp ->
            ( model, Cmd.none )

        Test s ->
            let
                _ =
                    Debug.log "test" s
            in
            ( model, Cmd.none )


runAppCmds : List (AppCmd.AppCmd Msg) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runAppCmds appCmds ( model, prevCmd ) =
    List.foldl
        runAppCmd
        ( model, prevCmd )
        appCmds


runAppCmd : AppCmd.AppCmd Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runAppCmd appCmd ( model, prevCmd ) =
    let
        ( newModel, newCmd ) =
            update
                (AppCmd appCmd)
                model
    in
    ( newModel
    , Cmd.batch
        [ prevCmd
        , newCmd
        ]
    )


addUserNotices : List (UserNotice Msg) -> Model -> Model
addUserNotices userNotices prevModel =
    List.foldl
        addUserNotice
        prevModel
        userNotices


addUserNotice : UserNotice Msg -> Model -> Model
addUserNotice userNotice prevModel =
    { prevModel
        | userNotices =
            List.append
                prevModel.userNotices
                [ userNotice ]
    }


encodeGTag : AppCmd.GTagData -> Json.Decode.Value
encodeGTag gtag =
    Json.Encode.object
        [ ( "event", Json.Encode.string gtag.event )
        , ( "category", Json.Encode.string gtag.category )
        , ( "label", Json.Encode.string gtag.label )
        , ( "value", Json.Encode.int gtag.value )
        ]


encodeGenPrivkeyArgs : Address -> String -> Json.Decode.Value
encodeGenPrivkeyArgs address signMsg =
    Json.Encode.object
        [ ( "address", Json.Encode.string <| Eth.Utils.addressToString address )
        , ( "signSeedMsg", Json.Encode.string signMsg )
        ]


beginRouteChange : Browser.Navigation.Key -> Routing.Route -> Cmd Msg
beginRouteChange key route =
    Browser.Navigation.pushUrl key (Routing.routeToString route)


updateFromUrl : Url -> Model -> ( Model, Cmd Msg )
updateFromUrl url model =
    gotoRoute model (Routing.urlToRoute url)


gotoRoute : Model -> Routing.Route -> ( Model, Cmd Msg )
gotoRoute oldModel route =
    let
        newUrlString =
            Routing.routeToString route
    in
    case route of
        Routing.Home ->
            ( { oldModel
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
            ( { oldModel
                | submodel = CreateModel updateResult.model
                , txSentry = newTxSentry
              }
            , Cmd.batch
                [ Cmd.map CreateMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runAppCmds
                    (AppCmd.mapList CreateMsg updateResult.appCmds)

        Routing.Trade id ->
            let
                updateResult =
                    Trade.State.init oldModel.web3Context oldModel.userInfo id

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute oldModel.txSentry (ChainCmd.map TradeMsg updateResult.chainCmd)
            in
            ( { oldModel
                | submodel = TradeModel updateResult.model
                , txSentry = newTxSentry
              }
            , Cmd.batch
                [ Cmd.map TradeMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runAppCmds
                    (AppCmd.mapList TradeMsg updateResult.appCmds)

        Routing.Marketplace browsingRole ->
            let
                ( marketplaceModel, marketplaceCmd ) =
                    Marketplace.State.init oldModel.web3Context browsingRole oldModel.userInfo
            in
            ( { oldModel
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
            ( { oldModel
                | submodel = AgentHistoryModel agentHistoryModel
              }
            , Cmd.batch
                [ Cmd.map AgentHistoryMsg agentHistoryCmd
                ]
            )

        Routing.NotFound ->
            ( oldModel |> addUserNotice UN.invalidUrl
            , Cmd.none
            )


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
subscriptions model =
    let
        failedWalletDecodeToMsg : String -> Msg
        failedWalletDecodeToMsg =
            UN.walletError >> AppCmd.UserNotice >> AppCmd
    in
    Sub.batch
        ([ Time.every 1000 Tick
         , walletSentryPort (WalletSentry.decodeToMsg failedWalletDecodeToMsg WalletStatus)
         , TxSentry.listen model.txSentry
         , userPubkeyResult UserPubkeySet
         , Sub.map TradeCacheMsg <| TradeCache.subscriptions model.tradeCache
         , networkSentryPort NetworkUpdate
         ]
            ++ [ submodelSubscriptions model ]
        )


submodelSubscriptions : Model -> Sub Msg
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


port gTagOut : Json.Decode.Value -> Cmd msg
