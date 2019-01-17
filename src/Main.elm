port module Main exposing (Model, Msg(..), SubModel(..), headerView, init, main, subModelView, subscriptions, txIn, txOut, update, view, walletSentry)

import Create
import Eth.Net as Net
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (..)
import EthHelpers
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick)
import HtmlElements exposing (..)
import Interact
import Json.Decode as Decode exposing (Value)
import Time exposing (Time)


main : Program ( Int, Int ) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { time : Time
    , node : EthHelpers.EthNode
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , tokenContractDecimals : Int
    , subModel : SubModel
    }


type SubModel
    = CreateModel Create.Model
    | InteractModel Interact.Model


type Msg
    = GotoCreate
    | Tick Time
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | CreateMsg Create.Msg
    | InteractMsg Interact.Msg
    | Fail String


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( networkId, tokenContractDecimals ) =
    let
        node =
            Net.toNetworkId networkId
                |> EthHelpers.ethNode

        createModel =
            Tuple.first (Create.init tokenContractDecimals)
    in
    ( { time = 0
      , node = node
      , txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
      , userAddress = Nothing
      , tokenContractDecimals = tokenContractDecimals
      , subModel = CreateModel createModel
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotoCreate ->
            let
                ( createModel, createCmd ) =
                    Create.init model.tokenContractDecimals
            in
            ( { model | subModel = CreateModel createModel }
            , Cmd.map CreateMsg createCmd
            )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        WalletStatus walletSentry ->
            ( { model
                | userAddress = walletSentry.account
              }
            , Cmd.none
            )

        CreateMsg createMsg ->
            case model.subModel of
                CreateModel createModel ->
                    let
                        ( newCreateModel, createCmd ) =
                            Create.update createMsg createModel
                    in
                    ( { model | subModel = CreateModel newCreateModel }
                    , Cmd.map CreateMsg createCmd
                    )

                _ ->
                    ( model, Cmd.none )

        InteractMsg interactMsg ->
            case model.subModel of
                InteractModel interactModel ->
                    let
                        ( newInteractModel, interactCmd ) =
                            Interact.update interactMsg interactModel
                    in
                    ( { model | subModel = InteractModel newInteractModel }
                    , Cmd.map InteractMsg interactCmd
                    )

                _ ->
                    ( model, Cmd.none )

        TxSentryMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = subModel }, subCmd )

        Fail str ->
            let
                _ =
                    Debug.log str
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ headerView model
        , subModelView model
        ]


headerView : Model -> Html Msg
headerView model =
    div [ Html.Attributes.style [ ( "height", "30px" ) ] ]
        [ button [ onClick GotoCreate ] [ text "Create" ]
        ]


subModelView : Model -> Html Msg
subModelView model =
    case model.subModel of
        CreateModel createModel ->
            Html.map CreateMsg (Create.view createModel)

        InteractModel interactModel ->
            Html.map InteractMsg (Interact.view interactModel model.time)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , walletSentry (WalletSentry.decodeToMsg Fail WalletStatus)
        , TxSentry.listen model.txSentry
        ]


port walletSentry : (Value -> msg) -> Sub msg


port txOut : Value -> Cmd msg


port txIn : (Value -> msg) -> Sub msg



-- ttAddress =
--   let
--     ttAddressResult = EthUtils.toAddress ttAddressString
--   in
--     case ttAddressResult of
--       Err _ ->
--         let _ = Debug.crash "Address '" ++ ttAddressString ++ "' passed to Elm could not be converted!"
--         in EthUtils.unsafeToAddress "0x0"
--       Ok tta -> tta
-- and then
-- Eth.call node.http ( ToastytradeSell.getFullState ttAddress )
--              |> Task.attempt FullStateFetched
