port module State exposing (init, subscriptions, update)

import BigInt
import Browser
import Browser.Navigation
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Constants exposing (..)
import Create.State
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers exposing (EthNode)
import Json.Decode
import Json.Encode
import Routing
import Search.State
import Time
import Trade.State
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        node =
            Eth.Net.toNetworkId flags.networkId
                |> EthHelpers.ethNode

        txSentry =
            TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
    in
    updateFromUrl
        { key = key
        , time = Time.millisToPosix 0
        , node = node
        , txSentry = txSentry
        , userAddress = Nothing
        , userInfo = Nothing
        , submodel = HomeModel
        }
        url


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
            ( Running model, Cmd.none )

        GotoRoute route ->
            gotoRoute model route

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
                            in
                            ( Running
                                { model
                                    | userInfo = userInfo
                                    , submodel = model.submodel |> updateSubmodelUserInfo userInfo
                                }
                            , Cmd.none
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
                            gotoRoute model route

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

        SearchMsg searchMsg ->
            case model.submodel of
                SearchModel searchModel ->
                    let
                        ( newSearchModel, searchCmd, newRoute ) =
                            Search.State.update searchMsg searchModel
                    in
                    case newRoute of
                        Nothing ->
                            ( Running { model | submodel = SearchModel newSearchModel }
                            , Cmd.map SearchMsg searchCmd
                            )

                        Just route ->
                            gotoRoute model route

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


updateFromUrl : ValidModel -> Url -> ( Model, Cmd Msg )
updateFromUrl model url =
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
                    | submodel = HomeModel
                }
            , Browser.Navigation.pushUrl model.key newUrlString
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
                , Browser.Navigation.pushUrl model.key newUrlString
                ]
            )

        Routing.Trade maybeID ->
            case maybeID of
                Nothing ->
                    ( Failed "Error interpreting url", Browser.Navigation.pushUrl model.key newUrlString )

                Just id ->
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
                        , Browser.Navigation.pushUrl model.key newUrlString
                        ]
                    )

        Routing.Search openMode ->
            let
                ( searchModel, searchCmd ) =
                    Search.State.init model.node openMode model.userInfo
            in
            ( Running
                { model
                    | submodel = SearchModel searchModel
                }
            , Cmd.batch
                [ Cmd.map SearchMsg searchCmd
                , Browser.Navigation.pushUrl model.key newUrlString
                ]
            )

        Routing.NotFound ->
            ( Failed "Don't understand that url...", Browser.Navigation.pushUrl model.key newUrlString )


updateSubmodelUserInfo : Maybe UserInfo -> Submodel -> Submodel
updateSubmodelUserInfo userInfo submodel =
    case submodel of
        HomeModel ->
            submodel

        CreateModel createModel ->
            CreateModel (createModel |> Create.State.updateUserInfo userInfo)

        TradeModel tradeModel ->
            TradeModel (tradeModel |> Trade.State.updateUserInfo userInfo)

        SearchModel searchModel ->
            SearchModel (searchModel |> Search.State.updateUserInfo userInfo)


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
        HomeModel ->
            Sub.none

        CreateModel createModel ->
            Sub.map CreateMsg <| Create.State.subscriptions createModel

        TradeModel tradeModel ->
            Sub.map TradeMsg <| Trade.State.subscriptions tradeModel

        SearchModel searchModel ->
            Sub.map SearchMsg <| Search.State.subscriptions searchModel


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port genPrivkey : Json.Decode.Value -> Cmd msg


port userPubkeyResult : (Json.Decode.Value -> msg) -> Sub msg
