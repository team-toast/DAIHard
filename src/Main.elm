port module Main exposing (..)

-- Library

import Eth
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Types exposing (..)
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Units exposing (gwei)
import Eth.Utils as EthUtils
import Array
import BigInt exposing (BigInt)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
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
  { time : Time
  , node : EthNode
  , txSentry : TxSentry Msg
  , userAddress : Maybe Address
  , page : Page
  }

type Page
  = CreateTT
  | ViewTT
    { ttAddress : Address
    , ttModel : Maybe ToastytradeSellModel
    }

type alias TokenAmount =
  { evmValue : BigInt
  , numDecimals : Int
  }

tokenAmountToString : TokenAmount -> Int -> String
tokenAmountToString tokenAmount displayPrecision =
  let
    firstTruncateAmount = tokenAmount.numDecimals - displayPrecision
    divisor = BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt firstTruncateAmount)
    (truncatedAmount, remainder) = Maybe.withDefault (tokenAmount.evmValue, BigInt.fromInt 0 ) ( BigInt.divmod tokenAmount.evmValue divisor )
    firstDigitOfRemainder =
      BigInt.toString remainder
      |> String.slice 0 1
      |> String.toInt
      |> Result.withDefault 0
    truncatedAmountRounded =
      if firstDigitOfRemainder > 5 then
        truncatedAmount
      else
        truncatedAmount
    amountStringNoDecimal = BigInt.toString truncatedAmount
  in
    if displayPrecision == 0 then
      amountStringNoDecimal
    else
      (  String.dropRight displayPrecision amountStringNoDecimal
      ++ "."
      ++ String.right displayPrecision amountStringNoDecimal
      )
      |> removeExtraZeros

removeExtraZeros : String -> String
removeExtraZeros numString =
  if (numString |> String.endsWith "0") || (numString |> String.endsWith ".") then
    removeExtraZeros (String.slice 0 -1 numString)
  else
    numString

stringToTokenAmount : String -> Int -> Result String TokenAmount
stringToTokenAmount amountString numDecimals =
  let
    ( newString, numDigitsMoved ) = pullAnyFirstDecimalOffToRight amountString
    numDigitsLeftToMove = numDecimals - numDigitsMoved
    maybeBigIntAmount = BigInt.fromString newString
  in
    if numDigitsLeftToMove < 0 then
      Err "Number is too precise!"
    else
      case maybeBigIntAmount of
        Nothing ->
          Err "Can't interpret that number!"
        Just bigIntAmount ->
          let
            evmValue = (BigInt.mul bigIntAmount ( BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt numDigitsLeftToMove) ) )
          in
            Ok (TokenAmount evmValue numDecimals)


pullAnyFirstDecimalOffToRight : String -> (String, Int)
pullAnyFirstDecimalOffToRight numString =
  let
    maybeDecimalPosition = List.head (String.indexes "." numString)
  in
    case maybeDecimalPosition of
      Nothing ->
        ( numString, 0 )
      Just decimalPos ->
        let
          numDigitsMoved = ( ( String.length numString) - 1 ) - decimalPos
          newString =
            (  String.left decimalPos numString
            ++ String.dropLeft (decimalPos + 1) numString
            )
        in
          ( newString, numDigitsMoved )

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
      { time = 0
      , node = node
      , txSentry = TxSentry.init ( txOut, txIn ) TxSentryMsg node.http
      , userAddress = Nothing
      , page = ViewTT
        { ttAddress = ttAddress
        , ttModel = Nothing
        }
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
  = GotoCreate
  | Tick Time
  | WalletStatus WalletSentry
  | TxSentryMsg TxSentry.Msg
  | FullStateFetched (Result Http.Error GetFullState)
  | Fail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotoCreate ->
      ( { model | page = CreateTT }, Cmd.none )

    Tick newTime ->
      ( { model | time = newTime }, Cmd.none )

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
      case model.page of
        ViewTT viewPage ->
          let
            ttModelResult = toastytradeModelFromGetFullState getFullState
          in
            case ttModelResult of
              Err e ->
                let _ = Debug.log e
                in (model, Cmd.none)
              Ok ttModel ->
                let
                  newPage = ViewTT { viewPage | ttModel = Just ttModel }
                in
                  ({ model
                    | page = newPage
                  } , Cmd.none)
        CreateTT ->
          -- ignore the update
          ( model, Cmd.none )

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
  div []
      [ headerView model
      , pageView model.page model.time
      ]

headerView : Model -> Html Msg
headerView model =
  div [Html.Attributes.style [("height", "30px")] ]
      [ button [ onClick GotoCreate ] [ text "Create" ]
      ]

pageView : Page -> Time.Time -> Html Msg
pageView page currentTime =
  case page of
    CreateTT ->
      div []
          [ text "Sell "
          --, input [ ]
          ]


    ViewTT viewPage ->
      case viewPage.ttModel of
        Nothing ->
          div [] [ text "Model loading... " ]
        Just ttModel ->
          div []
              [ div []
                    [ text "Toastytrade Sell at address "
                    , text (EthUtils.addressToString viewPage.ttAddress)
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
              , countdownView ttModel currentTime
              ]

countdownView : ToastytradeSellModel -> Time.Time -> Html Msg
countdownView ttModel currentTime =
  let
    phaseInterval =
      case ttModel.phase of
        Open -> ttModel.autorecallInterval
        Committed -> ttModel.depositDeadlineInterval
        Claimed -> ttModel.autoreleaseInterval
        Closed -> 0

    timeLeft = (ttModel.phaseStartTime + phaseInterval) - currentTime
    countdownHasPassed = timeLeft < 0
    textElement =
      text
        (
        case (ttModel.phase, countdownHasPassed) of
          (Open, False) ->
            "This offer autorecalls in "
          (Open, True) ->
            "Autorecall available. Poke to autorecall."
          (Committed, False) ->
            "Time left until deposit deadline: "
          (Committed, True) ->
            "Deposit deadline passed. Poke to return Seller's ether and burn Buyer's deposit."
          (Claimed, False) ->
            "Autorelease to Buyer in "
          (Claimed, True) ->
            "Autorelease available. Poke to autorelease."
          (Closed, _) ->
            "This offer is now closed."
        )
    dynamicElement =
      if ttModel.phase == Closed then
        div [] []
      else if countdownHasPassed then
        div [] [ text "POKEBUTTON" ]
      else
        div [] [ text (toString timeLeft) ]
  in
    div [] [ textElement, dynamicElement ]

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
