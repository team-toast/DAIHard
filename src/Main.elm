port module Main exposing (Model, Msg(..), Submodel(..), init, main, subscriptions, txIn, txOut, update, view, walletSentryPort)

import Browser
import ChainCmd exposing (ChainCmdOrder)
import Create
import Element
import Element.Input as Input
import ElementHelpers exposing (..)
import Eth.Net as Net
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import Eth.Utils as EthUtils
import EthHelpers
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import Interact
import Json.Decode as Decode exposing (Value)
import Time


main : Program ( Int, Int, String ) Model Msg
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
    { factoryAddress : Address
    , time : Time.Posix
    , node : EthHelpers.EthNode
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , tokenContractDecimals : Int
    , submodel : Submodel
    }


type Submodel
    = CreateModel Create.Model
    | InteractModel Interact.Model
    | None


type Msg
    = GotoCreate
    | Tick Time.Posix
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | CreateMsg Create.Msg
    | InteractMsg Interact.Msg
    | Fail String


init : ( Int, Int, String ) -> ( Model, Cmd Msg )
init ( networkId, tokenContractDecimals, factoryAddressString ) =
    case EthUtils.toAddress factoryAddressString of
        Ok factoryAddress ->
            let
                node =
                    Net.toNetworkId networkId
                        |> EthHelpers.ethNode

                txSentry =
                    TxSentry.init ( txOut, txIn ) TxSentryMsg node.http

                ( createModel, createCmd, chainCmdOrder ) =
                    Create.init factoryAddress Nothing tokenContractDecimals

                ( newTxSentry, chainCmd ) =
                    ChainCmd.execute txSentry (ChainCmd.map CreateMsg chainCmdOrder)

                cmdBatch =
                    Cmd.batch
                        [ Cmd.map CreateMsg createCmd
                        , chainCmd
                        ]
            in
            ( Running
                { factoryAddress = factoryAddress
                , time = Time.millisToPosix 0
                , node = node
                , txSentry = newTxSentry
                , userAddress = Nothing
                , tokenContractDecimals = tokenContractDecimals
                , submodel = CreateModel createModel
                }
            , cmdBatch
            )

        Err errstr ->
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
                    Create.init model.factoryAddress model.userAddress model.tokenContractDecimals

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
                        ( newInteractModel, interactCmd ) =
                            Interact.update interactMsg interactModel
                    in
                    ( Running { model | submodel = InteractModel newInteractModel }
                    , Cmd.map InteractMsg interactCmd
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
            Element.layout []
                (Element.column
                    []
                    [ headerElement model
                    , subModelElement model
                    ]
                )

        Failed str ->
            Element.layout []
                (Element.text ("ERROR: " ++ str))


headerElement : ValidModel -> Element.Element Msg
headerElement model =
    Element.row []
        [ Input.button []
            { onPress = Just GotoCreate
            , label = Element.text "Create!"
            }
        ]


subModelElement : ValidModel -> Element.Element Msg
subModelElement model =
    case model.submodel of
        CreateModel createModel ->
            Element.map CreateMsg (Create.viewElement createModel)

        InteractModel interactModel ->
            Element.none

        None ->
            Element.none



-- Html.map InteractMsg (Interact.viewElement interactModel model.time)


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


port walletSentryPort : (Value -> msg) -> Sub msg


port txOut : Value -> Cmd msg


port txIn : (Value -> msg) -> Sub msg
