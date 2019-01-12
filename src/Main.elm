port module Main exposing (..)

-- Library

import Eth
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Types exposing (..)
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Units exposing (gwei)
import Eth.Utils as EthUtils
import BigInt exposing (BigInt)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Process
import Task
import Time exposing (Time)

-- Internal

import Contracts.ToastytradeSell as ToastytradeSell exposing (..)

main : Program (Int, String) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { node : EthNode
  , txSentry : TxSentry Msg
  , userAddress : Maybe Address
  , ttAddress : Address
  , ttModel : Maybe ToastytradeSellModel
  }

init : (Int, String) -> (Model, Cmd Msg)
init (networkId, ttAddressString) =
  let
    node =
      Net.toNetworkId networkId
        |> ethNode
    ttAddress =
      let
        ttAddressResult = EthUtils.toAddress ttAddressString
      in
        case ttAddressResult of
          Err _ ->
            let _ = Debug.crash "Address '" ++ ttAddressString ++ "' passed to Elm could not be converted!"
            in EthUtils.unsafeToAddress "0x0"
          Ok tta -> tta
  in
    (
      { node = node
      , txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
      , userAddress = Nothing
      , ttAddress = ttAddress
      , ttModel = Nothing
      }
    , Cmd.batch [ Eth.call node.http ( ToastytradeSell.getFullState ttAddress )
                    |> Task.attempt FullStateFetched
                ]
    )

type alias ToastytradeSellModel =
  { balance : BigInt
  , phase : Phase
  , phaseStartTime : Time
  , seller : Address
  , buyer : Maybe Address
  , buyerDeposit : BigInt
  , autorecallInterval : Time
  , depositDeadlineInterval : Time
  , autoreleaseInterval : Time
  }

toastytradeModelFromGetFullState : ToastytradeSell.GetFullState -> Result String ToastytradeSellModel
toastytradeModelFromGetFullState fullState =
  let
    phaseResult = bigIntToPhase fullState.phase
    phaseStartTimeResult = bigIntToTime fullState.phaseStartTimestamp
    autorecallIntervalResult = bigIntToTime fullState.autorecallInterval
    depositDeadlineIntervalResult = bigIntToTime fullState.depositDeadlineInterval
    autoreleaseIntervalResult = bigIntToTime fullState.autoreleaseInterval
  in
    case (phaseResult, phaseStartTimeResult, autorecallIntervalResult, depositDeadlineIntervalResult, autoreleaseIntervalResult) of
      (Err e, _, _, _, _) -> Err ( "Error interpreting phase: " ++ e )
      (_, Err e, _, _, _) -> Err ( "Error interpreting phaseStartTimestamp: " ++ e )
      (_, _, Err e, _, _) -> Err ( "Error interpreting autorecallInterval: " ++ e )
      (_, _, _, Err e, _) -> Err ( "Error interpreting depositDeadlineInterval: " ++ e )
      (_, _, _, _, Err e) -> Err ( "Error interpreting autoreleaseInterval: " ++ e )
      (Ok phase, Ok phaseStartTime, Ok autorecallInterval, Ok depositDeadlineInterval, Ok autoreleaseInterval) ->
        Ok
        { balance = fullState.balance
        , phase = phase
        , phaseStartTime = phaseStartTime
        , seller = fullState.seller
        , buyer = addressIfNot0x0 fullState.buyer
        , buyerDeposit = fullState.buyerDeposit
        , autorecallInterval = autorecallInterval
        , depositDeadlineInterval = depositDeadlineInterval
        , autoreleaseInterval = autoreleaseInterval
        }

bigIntToPhase : BigInt -> Result String Phase
bigIntToPhase phase =
  let
    phaseInt = Result.withDefault 99 ( BigInt.toString phase |> String.toInt )
  in
    case phaseInt of
      0 -> Ok Open
      1 -> Ok Committed
      2 -> Ok Claimed
      3 -> Ok Closed
      _ -> Err "Invalid Toastytrade.Phase enum! How did this happen? Did you forget to update the ABI with a new phase?"

bigIntToTime : BigInt -> Result String Time
bigIntToTime time =
  let
    maybeTimeInt = (BigInt.toString time |> String.toInt)
  in
    case maybeTimeInt of
      Err _ ->
        Err "Can't convert the BigInt to a time!"
      Ok timeInt ->
        Ok (toFloat timeInt * Time.second)

addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
  if addr == EthUtils.unsafeToAddress("0x0000000000000000000000000000000000000000") then
    Nothing
  else
    Just addr

type Phase
  = Open
  | Committed
  | Claimed
  | Closed


type alias EthNode =
    { http : HttpProvider
    , ws : WebsocketProvider
    }


ethNode : NetworkId -> EthNode
ethNode networkId =
  case networkId of
    Mainnet ->
      EthNode "https://mainnet.infura.io/" "wss://mainnet.infura.io/ws"

    Ropsten ->
      EthNode "https://ropsten.infura.io/" "wss://ropsten.infura.io/ws"

    Kovan ->
      EthNode "https://kovan.infura.io/" "wss://kovan.infura.io/ws"

    Rinkeby ->
      EthNode "https://rinkeby.infura.io/" "wss://rinkeby.infura.io/ws"

    _ ->
      EthNode "UnknownEthNetwork" "UnknownEthNetwork"


type Msg
  = WalletStatus WalletSentry
  | TxSentryMsg TxSentry.Msg
  | FullStateFetched (Result Http.Error GetFullState)
  | Fail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    WalletStatus walletSentry ->
      ({ model
       | userAddress = walletSentry.account
       }
      , Cmd.none
      )

    FullStateFetched (Err e) ->
      let _ = Debug.log "can't fetch full state: " e
      in ( model, Cmd.none )

    FullStateFetched (Ok getFullState) ->
      let
        ttModelResult = toastytradeModelFromGetFullState getFullState
      in
        case ttModelResult of
          Err e ->
            let _ = Debug.log e
            in (model, Cmd.none)
          Ok ttModel ->
            ({ model
              | ttModel = Just ttModel
            }, Cmd.none)

    TxSentryMsg subMsg ->
      let
        ( subModel, subCmd ) =
          TxSentry.update subMsg model.txSentry
      in
        ( { model | txSentry = subModel }, subCmd )

    Fail str ->
      let _ = Debug.log str
      in (model, Cmd.none)


view : Model -> Html Msg
view model =
  case model.ttModel of
    Nothing ->
      div [] [ text "Model loading... " ]
    Just ttModel ->
      div []
          [ div []
                [ text "Toastytrade Sell at address "
                , text (EthUtils.addressToString model.ttAddress)
                ]
          , text (toString ttModel.phase)
          , case ttModel.phase of
              Open ->
                div []
                    [ text (EthUtils.addressToString ttModel.seller)
                    , text " selling."
                    ]
              _ ->
                div []
                    [ text (EthUtils.addressToString ttModel.seller)
                    , text " selling to "
                    , text (Maybe.withDefault "???" (Maybe.map EthUtils.addressToString ttModel.buyer))
                    ]
          ]

countdownView : ToastytradeSellModel -> Time.Time -> Html Msg
countdownView ttModel currentTime =
  let
    phaseInterval =
      case ttModel.phase of
        Open -> autorecallInterval
        Committed -> depositDeadlineInterval
        Claimed -> autoreleaseInterval
        Closed -> 0

    countdownPassed = ttModel.phaseStartTime + phaseInterval > currentTime
    descriptionDiv =
      case ttModel.

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ walletSentry (WalletSentry.decodeToMsg Fail WalletStatus)
        , TxSentry.listen model.txSentry
        ]


port walletSentry : (Value -> msg) -> Sub msg

port txOut : Value -> Cmd msg

port txIn : (Value -> msg) -> Sub msg
