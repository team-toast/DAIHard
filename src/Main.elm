port module Main exposing (Model, Msg(..), Submodel(..), init, main, subscriptions, txIn, txOut, update, view, walletSentryPort)

import Browser
import ChainCmd exposing (ChainCmdOrder)
import Create
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Net
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import Html
import Interact
import Json.Decode
import Time


type alias Flags =
    { networkId : Int
    , tokenContractDecimals : Int
    , tokenContractAddressString : String
    , factoryAddressString : String
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Running ValidModel
    | Failed String


type alias ValidModel =
    { tokenContractAddress : Address
    , factoryAddress : Address
    , time : Time.Posix
    , node : EthHelpers.EthNode
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , tokenContractDecimals : Int
    , submodel : Submodel
    }


type Submodel
    = Home
    | CreateModel Create.Model
    | InteractModel Interact.Model
    | None


type Msg
    = GotoCreate
    | GotoInteract
    | Tick Time.Posix
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | CreateMsg Create.Msg
    | InteractMsg Interact.Msg
    | Fail String


init : Flags -> ( Model, Cmd Msg )
init flags =
    case ( Eth.Utils.toAddress flags.tokenContractAddressString, Eth.Utils.toAddress flags.factoryAddressString ) of
        ( Ok tokenContractAddress, Ok factoryAddress ) ->
            let
                node =
                    Eth.Net.toNetworkId flags.networkId
                        |> EthHelpers.ethNode

                txSentry =
                    TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
            in
            ( Running
                { tokenContractAddress = tokenContractAddress
                , factoryAddress = factoryAddress
                , time = Time.millisToPosix 0
                , node = node
                , txSentry = txSentry
                , userAddress = Nothing
                , tokenContractDecimals = flags.tokenContractDecimals
                , submodel = Home
                }
            , Cmd.none
            )

        ( Err errstr, _ ) ->
            ( Failed ("error interpreting token contract address: " ++ errstr), Cmd.none )

        ( _, Err errstr ) ->
            ( Failed ("error interpreting factory contract address: " ++ errstr), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg maybeValidModel =
    case maybeValidModel of
        Running model ->
            updateValidModel msg model

        Failed str ->
            ( maybeValidModel, Cmd.none )


updateValidModel : Msg -> ValidModel -> ( Model, Cmd Msg )
updateValidModel msg model =
    case msg of
        GotoCreate ->
            let
                ( createModel, createCmd, chainCmdOrder ) =
                    Create.init model.tokenContractAddress model.tokenContractDecimals model.factoryAddress model.userAddress

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

        GotoInteract ->
            let
                ( interactModel, interactCmd, chainCmdOrder ) =
                    Interact.init model.node model.tokenContractAddress model.tokenContractDecimals model.userAddress Nothing

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
                ]
            )

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
                            Create.update createMsg createModel

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
                            Interact.update interactMsg interactModel

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

        TxSentryMsg subMsg ->
            let
                ( submodel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( Running { model | txSentry = submodel }, subCmd )

        Fail str ->
            ( Failed str, Cmd.none )


updateSubmodelWithUserAddress : Submodel -> Maybe Address -> Submodel
updateSubmodelWithUserAddress submodel userAddress =
    case submodel of
        Home ->
            submodel

        CreateModel createModel ->
            CreateModel (Create.updateWithUserAddress createModel userAddress)

        InteractModel interactModel ->
            InteractModel (Interact.updateWithUserAddress interactModel userAddress)

        None ->
            None


view : Model -> Html.Html Msg
view maybeValidModel =
    case maybeValidModel of
        Running model ->
            let
                mainElementAttributes =
                    [ Element.width Element.fill
                    , Element.Background.color EH.pageBackgroundColor
                    ]

                mainColumnAttributes =
                    [ Element.width Element.fill
                    ]
            in
            Element.layout mainElementAttributes
                (Element.column mainColumnAttributes
                    [ headerElement model
                    , bodyElement model
                    ]
                )

        Failed str ->
            Element.layout []
                (Element.text ("ERROR: " ++ str))


headerElement : ValidModel -> Element.Element Msg
headerElement model =
    Element.row
        [ Element.Background.color EH.headerBackgroundColor
        , Element.width Element.fill
        , Element.padding 15
        , Element.spacing 30
        , Element.Font.size 24
        ]
        [ Element.Input.button [ Element.centerX ]
            { onPress = Just GotoCreate
            , label = Element.text "Create"
            }
        , Element.Input.button [ Element.centerX ]
            { onPress = Just GotoInteract
            , label = Element.text "Interact"
            }
        ]


bodyElement : ValidModel -> Element.Element Msg
bodyElement model =
    Element.el [ Element.paddingXY 100 25, Element.width Element.fill ]
        (subModelElement model)


subModelElement : ValidModel -> Element.Element Msg
subModelElement model =
    let
        subModelStyles =
            [ Element.width Element.fill
            , Element.spacing 20
            ]

        bodyStyles =
            [ Element.Border.rounded 15
            , Element.Background.color EH.subpageBackgroundColor
            , Element.padding 20
            , Element.spacing 50
            , Element.width Element.fill
            ]
    in
    (\( title, element ) ->
        Element.column subModelStyles
            [ EH.pageTitle title
            , Element.el bodyStyles element
            ]
    )
        (case model.submodel of
            Home ->
                ( "Home", Element.none )

            CreateModel createModel ->
                ( "Create", Element.map CreateMsg (Create.view createModel) )

            InteractModel interactModel ->
                ( "Interact", Element.map InteractMsg (Interact.view interactModel model.time) )

            None ->
                ( "none", Element.none )
        )


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
