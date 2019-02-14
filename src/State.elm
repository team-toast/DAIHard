port module State exposing (init, subscriptions, update)

import BigInt
import Browse.State
import Browser
import Browser.Navigation
import ChainCmd exposing (ChainCmd)
import Create.State
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Interact.State
import Json.Decode
import Routing
import Time
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    case ( Eth.Utils.toAddress flags.tokenContractAddressString, Eth.Utils.toAddress flags.factoryAddressString ) of
        ( Ok tokenContractAddress, Ok factoryAddress ) ->
            let
                node =
                    Eth.Net.toNetworkId flags.networkId
                        |> EthHelpers.ethNode

                txSentry =
                    TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
            in
            updateFromUrl
                { key = key
                , tokenContractAddress = tokenContractAddress
                , factoryAddress = factoryAddress
                , time = Time.millisToPosix 0
                , node = node
                , txSentry = txSentry
                , userAddress = Nothing
                , tokenContractDecimals = flags.tokenContractDecimals
                , submodel = HomeModel
                }
                url

        ( Err errstr, _ ) ->
            ( Failed ("error interpreting token contract address: " ++ errstr), Cmd.none )

        ( _, Err errstr ) ->
            ( Failed ("error interpreting factory contract address: " ++ errstr), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg maybeValidModel =
    case maybeValidModel of
        Running model ->
            updateValidModel msg model

        Failed _ ->
            ( maybeValidModel, Cmd.none )


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
                    , submodel = updateSubmodelWithUserAddress model.submodel walletSentry.account
                }
            , Cmd.none
            )

        CreateMsg createMsg ->
            case model.submodel of
                CreateModel createModel ->
                    let
                        ( newCreateModel, createCmd, chainCmdOrder ) =
                            Create.State.update createMsg createModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map CreateMsg chainCmdOrder)
                    in
                    ( Running
                        { model
                            | submodel = CreateModel newCreateModel
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map CreateMsg createCmd
                        , chainCmd
                        ]
                    )

                _ ->
                    ( Running model, Cmd.none )

        InteractMsg interactMsg ->
            case model.submodel of
                InteractModel interactModel ->
                    let
                        ( newInteractModel, interactCmd, chainCmdOrder ) =
                            Interact.State.update interactMsg interactModel

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map InteractMsg chainCmdOrder)
                    in
                    ( Running
                        { model
                            | submodel = InteractModel newInteractModel
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map InteractMsg interactCmd
                        , chainCmd
                        ]
                    )

                _ ->
                    ( Running model, Cmd.none )

        BrowseMsg browseMsg ->
            case model.submodel of
                BrowseModel browseModel ->
                    let
                        ( newBrowseModel, browseCmd ) =
                            Browse.State.update browseMsg browseModel
                    in
                    ( Running
                        { model | submodel = BrowseModel newBrowseModel }
                    , Cmd.map BrowseMsg browseCmd
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


updateFromUrl : ValidModel -> Url -> ( Model, Cmd Msg )
updateFromUrl model url =
    gotoRoute model (Routing.urlToRoute url)


gotoRoute : ValidModel -> Route -> ( Model, Cmd Msg )
gotoRoute model route =
    let
        newUrlString =
            Routing.routeToString route
    in
    case route of
        Home ->
            ( Running
                { model
                    | submodel = HomeModel
                }
            , Browser.Navigation.pushUrl model.key newUrlString
            )

        Create ->
            let
                ( createModel, createCmd, chainCmdOrder ) =
                    Create.State.init model.tokenContractAddress model.tokenContractDecimals model.factoryAddress model.userAddress

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

        Interact id ->
            case Maybe.map BigInt.fromInt id of
                Nothing ->
                    ( Failed "Error interpreting url", Browser.Navigation.pushUrl model.key newUrlString )

                Just bigIntId ->
                    let
                        ( interactModel, interactCmd, chainCmdOrder ) =
                            Interact.State.init model.node model.factoryAddress model.tokenContractAddress model.tokenContractDecimals model.userAddress bigIntId

                        ( newTxSentry, chainCmd ) =
                            ChainCmd.execute model.txSentry (ChainCmd.map InteractMsg chainCmdOrder)
                    in
                    ( Running
                        { model
                            | submodel = InteractModel interactModel
                            , txSentry = newTxSentry
                        }
                    , Cmd.batch
                        [ Cmd.map InteractMsg interactCmd
                        , chainCmd
                        , Browser.Navigation.pushUrl model.key newUrlString
                        ]
                    )

        Browse ->
            let
                ( browseModel, browseCmd ) =
                    Browse.State.init model.node model.factoryAddress model.tokenContractDecimals model.userAddress
            in
            ( Running
                { model
                    | submodel = BrowseModel browseModel
                }
            , Cmd.batch
                [ Cmd.map BrowseMsg browseCmd
                , Browser.Navigation.pushUrl model.key newUrlString
                ]
            )

        NotFound ->
            ( Failed "Don't understand that url...", Browser.Navigation.pushUrl model.key newUrlString )


updateSubmodelWithUserAddress : Submodel -> Maybe Address -> Submodel
updateSubmodelWithUserAddress submodel userAddress =
    case submodel of
        HomeModel ->
            submodel

        CreateModel createModel ->
            CreateModel (Create.State.updateWithUserAddress createModel userAddress)

        InteractModel interactModel ->
            InteractModel (Interact.State.updateWithUserAddress interactModel userAddress)

        BrowseModel browseModel ->
            BrowseModel (Browse.State.updateWithUserAddress browseModel userAddress)

        None ->
            None


subscriptions : Model -> Sub Msg
subscriptions maybeValidModel =
    case maybeValidModel of
        Running model ->
            Sub.batch
                [ Time.every 1000 Tick
                , walletSentryPort (WalletSentry.decodeToMsg Fail WalletStatus)
                , TxSentry.listen model.txSentry
                ]

        Failed _ ->
            Sub.none


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg
