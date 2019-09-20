port module State exposing (init, subscriptions, update)

import AgentHistory.State
import Array exposing (Array)
import BigInt
import Browser
import Browser.Dom
import Browser.Navigation
import ChainCmd exposing (ChainCmd)
import CmdDown
import CmdUp
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Create.State
import Create.Types as Create
import Element
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import Helpers.Eth as EthHelpers
import Helpers.Tuple
import Json.Decode
import Json.Encode
import List.Extra
import Marketplace.State
import Maybe.Extra
import Notifications
import Routing
import Time
import Trade.State
import TradeCache.State as TradeCache
import TradeCache.Types exposing (TradeCache)
import Types exposing (..)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        tooSmallNotice =
            if flags.width < 1024 then
                Just UN.screenToSmall

            else
                Nothing

        wallet =
            if flags.networkId == 0 then
                Wallet.NoneDetected

            else
                Wallet.OnlyNetwork <| Eth.Net.toNetworkId flags.networkId

        providerNotice =
            if wallet == Wallet.NoneDetected then
                Just UN.noWeb3Provider

            else
                case Wallet.factory wallet of
                    Nothing ->
                        Just UN.wrongWeb3Network

                    Just _ ->
                        Nothing

        userNotices =
            Maybe.Extra.values
                [ tooSmallNotice, providerNotice ]

        txSentry =
            Wallet.httpProvider wallet
                |> Maybe.map
                    (\httpProvider ->
                        TxSentry.init ( txOut, txIn ) TxSentryMsg httpProvider
                    )

        tcInitResults =
            Config.activeFactories
                |> List.map TradeCache.initAndStartCaching

        ( tradeCaches, tcCmds, tcCmdUpLists ) =
            ( List.map Helpers.Tuple.tuple3First tcInitResults
            , List.map Helpers.Tuple.tuple3Second tcInitResults
            , List.map Helpers.Tuple.tuple3Third tcInitResults
            )

        cmdUps =
            tcCmdUpLists
                |> List.indexedMap
                    (\tcId tcCmdUps ->
                        CmdUp.mapList (TradeCacheMsg tcId) tcCmdUps
                    )
                |> List.concat

        tcCmd =
            tcCmds
                |> List.indexedMap
                    (\tcId cmd ->
                        Cmd.map (TradeCacheMsg tcId) cmd
                    )
                |> Cmd.batch

        ( model, fromUrlCmd ) =
            { key = key
            , wallet = wallet
            , userAddress = Nothing
            , time = Time.millisToPosix 0
            , txSentry = txSentry
            , tradeCaches = tradeCaches
            , submodel = InitialBlank
            , currentRoute = Routing.InitialBlank
            , userNotices = []
            , screenWidth = flags.width
            }
                |> updateFromUrl url
                |> runCmdUps cmdUps
    in
    ( model
        |> addUserNotices userNotices
    , Cmd.batch
        [ tcCmd
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
        CmdUp cmdUp ->
            case cmdUp of
                CmdUp.Web3Connect ->
                    model
                        |> update ConnectToWeb3

                CmdUp.GotoRoute newRoute ->
                    model
                        |> update (GotoRoute newRoute)

                CmdUp.GTag gtag ->
                    ( model
                    , gTagOut (encodeGTag gtag)
                    )

                CmdUp.UserNotice userNotice ->
                    ( model |> addUserNotice userNotice
                    , gTagOut <|
                        encodeGTag <|
                            GTagData
                                "user notice"
                                "user notice"
                                userNotice.label
                                0
                    )

                CmdUp.BrowserNotification title maybeBody maybeImg ->
                    ( model
                    , Notifications.createNotification notifyPort title maybeBody maybeImg
                    )

                CmdUp.RequestBrowserNotificationPermission ->
                    ( model
                    , requestNotifyPermissionPort ()
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
            model
                |> gotoRoute route
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , gTagOut <|
                                encodeGTag <|
                                    GTagData
                                        "GotoRoute"
                                        "navigation"
                                        (Routing.routeToString route)
                                        0
                            , Browser.Navigation.pushUrl model.key (Routing.routeToString route)
                            ]
                    )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        ConnectToWeb3 ->
            case model.wallet of
                Wallet.NoneDetected ->
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
                , wallet = Wallet.OnlyNetwork walletSentry.networkId
              }
            , genCommPubkeyCmd
            )

        UserPubkeySet commPubkeyValue ->
            case Json.Decode.decodeValue Json.Decode.string commPubkeyValue of
                Ok commPubkey ->
                    case ( model.userAddress, model.wallet ) of
                        ( Just userAddress, Wallet.OnlyNetwork network ) ->
                            let
                                wallet =
                                    Wallet.Active <|
                                        UserInfo
                                            network
                                            userAddress
                                            commPubkey
                            in
                            { model | wallet = wallet }
                                |> runCmdDown (CmdDown.UpdateWallet wallet)

                        ( Nothing, _ ) ->
                            ( model
                                |> addUserNotice
                                    (UN.unexpectedError
                                        "User pubkey set, but I can no longer find the user address!"
                                        Nothing
                                    )
                            , Cmd.none
                            )

                        ( _, Wallet.Active userInfo ) ->
                            let
                                _ =
                                    Debug.log "User pubkey set, but there's already an active userInfo! Overwriting commpubkey." userInfo

                                wallet =
                                    Wallet.Active
                                        { userInfo | commPubkey = commPubkey }
                            in
                            { model | wallet = wallet }
                                |> runCmdDown (CmdDown.UpdateWallet wallet)

                        ( _, _ ) ->
                            ( model
                                |> addUserNotice
                                    (UN.unexpectedError
                                        "Unexpected wallet state encounted when setting commPubkey!"
                                        ( model.userAddress, model.wallet )
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

                        ( newTxSentry, chainCmd, userNotices ) =
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
                        |> runCmdUps
                            (CmdUp.mapList CreateMsg updateResult.cmdUps
                                ++ List.map CmdUp.UserNotice userNotices
                            )

                _ ->
                    ( model, Cmd.none )

        TradeMsg tradeMsg ->
            case model.submodel of
                TradeModel tradeModel ->
                    let
                        updateResult =
                            Trade.State.update tradeMsg tradeModel

                        ( newTxSentry, chainCmd, userNotices ) =
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
                        |> runCmdUps
                            (CmdUp.mapList TradeMsg updateResult.cmdUps
                                ++ List.map CmdUp.UserNotice userNotices
                            )

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
                        |> runCmdUps
                            (CmdUp.mapList MarketplaceMsg updateResult.cmdUps)

                _ ->
                    ( model, Cmd.none )

        AgentHistoryMsg agentHistoryMsg ->
            case model.submodel of
                AgentHistoryModel agentHistoryModel ->
                    let
                        updateResult =
                            AgentHistory.State.update agentHistoryMsg agentHistoryModel

                        ( newTxSentry, chainCmd, userNotices ) =
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
                        |> runCmdUps
                            (CmdUp.mapList AgentHistoryMsg updateResult.cmdUps
                                ++ List.map CmdUp.UserNotice userNotices
                            )

                _ ->
                    ( model, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( newTxSentry, subCmd ) =
                    case model.txSentry of
                        Just txSentry ->
                            TxSentry.update subMsg txSentry
                                |> Tuple.mapFirst Just

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | txSentry = newTxSentry }, subCmd )

        TradeCacheMsg tcId tradeCacheMsg ->
            case List.Extra.getAt tcId model.tradeCaches of
                Nothing ->
                    ( model, Cmd.none )
                        |> runCmdUp (CmdUp.UserNotice <| UN.unexpectedError "Encountered an out-of-range error when trying to route a TradeCacheMsg" Nothing)

                Just tradeCache ->
                    let
                        updateResult =
                            TradeCache.update
                                tradeCacheMsg
                                tradeCache
                    in
                    ( { model
                        | tradeCaches =
                            model.tradeCaches
                                |> List.Extra.setAt tcId updateResult.tradeCache
                      }
                    , updateResult.cmd |> Cmd.map (TradeCacheMsg tcId)
                    )

        ClickHappened ->
            model |> runCmdDown CmdDown.CloseAnyDropdownsOrModals

        NoOp ->
            ( model, Cmd.none )

        Test s ->
            let
                _ =
                    Debug.log "test" s
            in
            ( model, Cmd.none )


runCmdUps : List (CmdUp.CmdUp Msg) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runCmdUps cmdUps ( model, prevCmd ) =
    List.foldl
        runCmdUp
        ( model, prevCmd )
        cmdUps


runCmdUp : CmdUp.CmdUp Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runCmdUp cmdUp ( model, prevCmd ) =
    let
        ( newModel, newCmd ) =
            update
                (CmdUp cmdUp)
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
    if List.member userNotice prevModel.userNotices then
        prevModel

    else
        { prevModel
            | userNotices =
                List.append
                    prevModel.userNotices
                    [ userNotice ]
        }


encodeGTag : GTagData -> Json.Decode.Value
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


updateFromUrl : Url -> Model -> ( Model, Cmd Msg )
updateFromUrl url model =
    if Routing.routeToString model.currentRoute == url.path then
        ( model
        , Cmd.none
        )

    else
        gotoRoute (Routing.urlToRoute url) model


gotoRoute : Routing.Route -> Model -> ( Model, Cmd Msg )
gotoRoute route oldModel =
    let
        initCreate : Create.Mode -> ( Model, Cmd Msg )
        initCreate mode =
            let
                updateResult =
                    Create.State.init oldModel.wallet mode

                ( newTxSentry, chainCmd, userNotices ) =
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
                |> runCmdUps
                    (CmdUp.mapList CreateMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )
    in
    (case route of
        Routing.InitialBlank ->
            ( oldModel
            , Cmd.none
            )

        Routing.CreateFiat ->
            initCreate Create.OffRamp

        Routing.CreateCrypto ->
            initCreate (Create.CryptoSwap Seller)

        Routing.Trade factory id ->
            let
                updateResult =
                    case getTradeFromCaches factory id oldModel.tradeCaches of
                        Just (CTypes.LoadedTrade trade) ->
                            Trade.State.initFromCached oldModel.wallet trade

                        _ ->
                            Trade.State.init oldModel.wallet factory id

                ( newTxSentry, chainCmd, userNotices ) =
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
                |> runCmdUps
                    (CmdUp.mapList TradeMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )

        Routing.Marketplace ->
            let
                ( marketplaceModel, marketplaceCmd ) =
                    Marketplace.State.init oldModel.wallet
            in
            ( { oldModel
                | submodel = MarketplaceModel marketplaceModel
              }
            , Cmd.batch
                [ Cmd.map MarketplaceMsg marketplaceCmd
                ]
            )

        Routing.AgentHistory address ->
            let
                ( agentHistoryModel, agentHistoryCmd ) =
                    AgentHistory.State.init oldModel.wallet address
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
    )
        |> Tuple.mapFirst
            (\model -> { model | currentRoute = route })


getTradeFromCaches : FactoryType -> Int -> List TradeCache -> Maybe CTypes.Trade
getTradeFromCaches factory id tradeCaches =
    tradeCaches
        |> List.Extra.find (\tc -> tc.factory == factory)
        |> Maybe.map .trades
        |> Maybe.andThen (Array.get id)


runCmdDown : CmdDown.CmdDown -> Model -> ( Model, Cmd Msg )
runCmdDown cmdDown oldModel =
    case oldModel.submodel of
        InitialBlank ->
            ( oldModel, Cmd.none )

        CreateModel createModel ->
            let
                updateResult =
                    createModel |> Create.State.runCmdDown cmdDown

                ( newTxSentry, chainCmd, userNotices ) =
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
                |> runCmdUps
                    (CmdUp.mapList CreateMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )

        TradeModel tradeModel ->
            let
                updateResult =
                    tradeModel |> Trade.State.runCmdDown cmdDown

                ( newTxSentry, chainCmd, userNotices ) =
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
                |> runCmdUps
                    (CmdUp.mapList TradeMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )

        MarketplaceModel marketplaceModel ->
            let
                updateResult =
                    marketplaceModel |> Marketplace.State.runCmdDown cmdDown

                ( newTxSentry, chainCmd, userNotices ) =
                    ChainCmd.execute oldModel.txSentry (ChainCmd.map MarketplaceMsg updateResult.chainCmd)
            in
            ( { oldModel
                | submodel = MarketplaceModel updateResult.model
                , txSentry = newTxSentry
              }
            , Cmd.batch
                [ Cmd.map MarketplaceMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runCmdUps
                    (CmdUp.mapList MarketplaceMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )

        AgentHistoryModel agentHistoryModel ->
            let
                updateResult =
                    agentHistoryModel |> AgentHistory.State.runCmdDown cmdDown

                ( newTxSentry, chainCmd, userNotices ) =
                    ChainCmd.execute oldModel.txSentry (ChainCmd.map AgentHistoryMsg updateResult.chainCmd)
            in
            ( { oldModel
                | submodel = AgentHistoryModel updateResult.model
                , txSentry = newTxSentry
              }
            , Cmd.batch
                [ Cmd.map AgentHistoryMsg updateResult.cmd
                , chainCmd
                ]
            )
                |> runCmdUps
                    (CmdUp.mapList AgentHistoryMsg updateResult.cmdUps
                        ++ List.map CmdUp.UserNotice userNotices
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        failedWalletDecodeToMsg : String -> Msg
        failedWalletDecodeToMsg =
            UN.walletError >> CmdUp.UserNotice >> CmdUp
    in
    Sub.batch
        ([ Time.every 1000 Tick
         , walletSentryPort (WalletSentry.decodeToMsg failedWalletDecodeToMsg WalletStatus)
         , Maybe.map TxSentry.listen model.txSentry
            |> Maybe.withDefault Sub.none
         , userPubkeyResult UserPubkeySet
         , model.tradeCaches
            |> List.map TradeCache.subscriptions
            |> List.indexedMap
                (\tcId sub -> Sub.map (TradeCacheMsg tcId) sub)
            |> Sub.batch
         ]
            ++ [ submodelSubscriptions model ]
        )


submodelSubscriptions : Model -> Sub Msg
submodelSubscriptions model =
    case model.submodel of
        InitialBlank ->
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


port connectToWeb3 : () -> Cmd msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port genPrivkey : Json.Decode.Value -> Cmd msg


port userPubkeyResult : (Json.Decode.Value -> msg) -> Sub msg


port gTagOut : Json.Decode.Value -> Cmd msg


port requestNotifyPermissionPort : () -> Cmd msg


port notifyPort : Notifications.NotifyPort msg
