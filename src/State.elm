port module State exposing (init, subscriptions, update)

import BigInt
import Browse.State
import Browser
import Browser.Navigation
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (UserInfo)
import Create.State
import Eth.Net
import Eth.Sentry.Tx as TxSentry
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Interact.State
import Json.Decode
import Json.Encode
import Routing
import Time
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        _ =
            genPrivkey

        -- Hack because Elm 0.19 will discard code that isn't used elsewhere in Elm code, event outgoing ports. >:(
    in
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
                , userInfo = Nothing
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

                Just _ ->
                    genPrivkey "Deriving keypair for encrypted communication on TOASTYTRADE. Never sign this on any other site!"
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

        -- ( Running model
        -- , encryptToPubkeys
        --     (Json.Encode.object
        --         [ ( "message", Json.Encode.string "hiiii" )
        --         , ( "pubkeyHexStrings", Json.Encode.list Json.Encode.string [ pubkeyHexString ] )
        --         ]
        --     )
        -- )
        MessagesEncrypted encryptedMessages ->
            let
                objectDecoder =
                    Json.Decode.map4 EncryptedMessage
                        (Json.Decode.field "encapsulated" Json.Decode.string)
                        (Json.Decode.field "iv" Json.Decode.string)
                        (Json.Decode.field "tag" Json.Decode.string)
                        (Json.Decode.field "encrypted" Json.Decode.string)

                decoder =
                    Json.Decode.list objectDecoder

                _ =
                    Debug.log "decoded"
                        (Json.Decode.decodeValue decoder encryptedMessages)
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
                        ( newBrowseModel, browseCmd, newRoute ) =
                            Browse.State.update browseMsg browseModel
                    in
                    case newRoute of
                        Nothing ->
                            ( Running { model | submodel = BrowseModel newBrowseModel }
                            , Cmd.map BrowseMsg browseCmd
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
                    Create.State.init model.tokenContractAddress model.tokenContractDecimals model.factoryAddress model.userInfo

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

        Routing.Interact id ->
            case Maybe.map BigInt.fromInt id of
                Nothing ->
                    ( Failed "Error interpreting url", Browser.Navigation.pushUrl model.key newUrlString )

                Just bigIntId ->
                    let
                        ( interactModel, interactCmd, chainCmdOrder ) =
                            Interact.State.init model.node model.factoryAddress model.tokenContractAddress model.tokenContractDecimals model.userInfo bigIntId

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

        Routing.Browse ->
            let
                ( browseModel, browseCmd ) =
                    Browse.State.init model.node model.factoryAddress model.tokenContractDecimals model.userInfo
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

        Routing.NotFound ->
            ( Failed "Don't understand that url...", Browser.Navigation.pushUrl model.key newUrlString )


updateSubmodelUserInfo : Maybe UserInfo -> Submodel -> Submodel
updateSubmodelUserInfo userInfo submodel =
    case submodel of
        HomeModel ->
            submodel

        CreateModel createModel ->
            CreateModel (createModel |> Create.State.updateUserInfo userInfo)

        InteractModel interactModel ->
            InteractModel (interactModel |> Interact.State.updateUserInfo userInfo)

        BrowseModel browseModel ->
            BrowseModel (browseModel |> Browse.State.updateUserInfo userInfo)


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

        InteractModel interactModel ->
            Sub.map InteractMsg <| Interact.State.subscriptions interactModel

        BrowseModel browseModel ->
            Sub.map BrowseMsg <| Browse.State.subscriptions browseModel


port walletSentryPort : (Json.Decode.Value -> msg) -> Sub msg


port txOut : Json.Decode.Value -> Cmd msg


port txIn : (Json.Decode.Value -> msg) -> Sub msg


port genPrivkey : String -> Cmd msg


port userPubkeyResult : (Json.Decode.Value -> msg) -> Sub msg
