port module State exposing (init, subscriptions, update)

import BigInt
import Browser
import Browser.Dom
import Browser.Navigation
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Create.State
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers exposing (EthNode)
import Json.Decode
import Json.Encode
import Marketplace.State
import MyTrades.State
import Network exposing (..)
import Routing
import Time
import Trade.State
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    if flags.width < 1024 then
        ( Failed "Sorry, your screen size (< 1024 width) is not supported."
        , Cmd.none
        )

    else
        case EthHelpers.intToNetwork flags.networkId of
            Nothing ->
                ( Failed "Your provider (Metamask?) is set to an unsupported network. Switch to mainnet (or Kovan for testing) and refresh."
                , Cmd.none
                )

            Just network ->
                let
                    node =
                        EthHelpers.ethNode network

                    txSentry =
                        TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
                in
                { key = key
                , time = Time.millisToPosix 0
                , node = node
                , txSentry = txSentry
                , userAddress = Nothing
                , userInfo = Nothing
                , submodel = BetaLandingPage
                }
                    |> updateFromUrl url


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

        WalletStatus walletSentry ->
            ( Running
                { model
                    | userAddress = walletSentry.account
                }
            , case walletSentry.account of
                Nothing ->
                    Cmd.none

                Just address ->
                    genPrivkey <|
                        encodeGenPrivkeyArgs
                            address
                            "Deriving keypair for encrypted communication on DAIHARD. Never sign this on any other site!"
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
                    case updateResult.newRoute of
                        Nothing ->
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

                        Just route ->
                            ( Running model
                            , beginRouteChange model.key route
                            )

                _ ->
                    ( Running model, Cmd.none )

        TradeMsg tradeMsg ->
            case model.submodel of
                TradeModel tradeModel ->
                    let
                        ( newTradeModel, tradeCmd, chainCmdOrder ) =
                            Trade.State.update tradeMsg tradeModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map TradeMsg chainCmdOrder)
                    in
                    ( Running
                        { model
                            | submodel = TradeModel newTradeModel
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map TradeMsg tradeCmd
                        , chainCmd
                        ]
                    )

                _ ->
                    ( Running model, Cmd.none )

        MarketplaceMsg marketplaceMsg ->
            case model.submodel of
                MarketplaceModel marketplaceModel ->
                    let
                        ( newMarketplaceModel, marketplaceCmd, newRoute ) =
                            Marketplace.State.update marketplaceMsg marketplaceModel
                    in
                    case newRoute of
                        Nothing ->
                            ( Running { model | submodel = MarketplaceModel newMarketplaceModel }
                            , Cmd.map MarketplaceMsg marketplaceCmd
                            )

                        Just route ->
                            ( Running model
                            , beginRouteChange model.key route
                            )

                _ ->
                    ( Running model, Cmd.none )

        MyTradesMsg myTradesMsg ->
            case model.submodel of
                MyTradesModel myTradesModel ->
                    let
                        updateResult =
                            MyTrades.State.update myTradesMsg myTradesModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map MyTradesMsg updateResult.chainCmd)
                    in
                    case updateResult.newRoute of
                        Nothing ->
                            ( Running
                                { model
                                    | submodel = MyTradesModel updateResult.model
                                    , txSentry = newTxSentry
                                }
                            , Cmd.batch
                                [ Cmd.map MyTradesMsg updateResult.cmd
                                , chainCmd
                                ]
                            )

                        Just route ->
                            ( Running model
                            , beginRouteChange model.key route
                            )

                _ ->
                    ( Running model, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( submodel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( Running { model | txSentry = submodel }, subCmd )

        Fail str ->
            ( Failed str, Cmd.none )

        NoOp ->
            ( Running model, Cmd.none )


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
gotoRoute model route =
    let
        newUrlString =
            Routing.routeToString route
    in
    case route of
        Routing.Home ->
            ( Running
                { model
                    | submodel = BetaLandingPage
                }
            , Cmd.none
            )

        Routing.Create ->
            let
                ( createModel, createCmd, chainCmdOrder ) =
                    Create.State.init model.node model.userInfo

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute model.txSentry (ChainCmd.map CreateMsg chainCmdOrder)
            in
            ( Running
                { model
                    | submodel = CreateModel createModel
                    , txSentry = newTxSentry
                }
            , Cmd.batch
                [ Cmd.map CreateMsg createCmd
                , chainCmd
                ]
            )

        Routing.Trade id ->
            let
                ( tradeModel, tradeCmd, chainCmdOrder ) =
                    Trade.State.init model.node model.userInfo id

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute model.txSentry (ChainCmd.map TradeMsg chainCmdOrder)
            in
            ( Running
                { model
                    | submodel = TradeModel tradeModel
                    , txSentry = newTxSentry
                }
            , Cmd.batch
                [ Cmd.map TradeMsg tradeCmd
                , chainCmd
                ]
            )

        Routing.Marketplace openMode ->
            let
                ( marketplaceModel, marketplaceCmd ) =
                    Marketplace.State.init model.node model.userInfo openMode
            in
            ( Running
                { model
                    | submodel = MarketplaceModel marketplaceModel
                }
            , Cmd.batch
                [ Cmd.map MarketplaceMsg marketplaceCmd
                ]
            )

        Routing.MyTrades ->
            let
                ( myTradesModel, myTradesCmd ) =
                    MyTrades.State.init model.node model.userInfo
            in
            ( Running
                { model
                    | submodel = MyTradesModel myTradesModel
                }
            , Cmd.batch
                [ Cmd.map MyTradesMsg myTradesCmd
                ]
            )

        Routing.NotFound ->
            ( Failed "Don't understand that url...", Browser.Navigation.pushUrl model.key newUrlString )


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

        MyTradesModel myTradesModel ->
            ( MyTradesModel (myTradesModel |> MyTrades.State.updateUserInfo userInfo)
            , Cmd.none
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

        MyTradesModel myTradesModel ->
            Sub.map MyTradesMsg <| MyTrades.State.subscriptions myTradesModel


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port genPrivkey : Json.Decode.Value -> Cmd msg


port userPubkeyResult : (Json.Decode.Value -> msg) -> Sub msg
