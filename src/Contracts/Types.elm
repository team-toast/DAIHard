module Contracts.Types exposing (CreateParameters, OpenMode(..), Phase(..), State, UserParameters, bigIntToPhase, buildCreateParameters, decodeState, initiatorIsBuyerToOpenMode, openModeToInitiatorIsBuyer, phaseToString, txReceiptToCreatedToastytradeSellAddress, txReceiptToCreatedToastytradeSellId)

import Abi.Decode
import BigInt exposing (BigInt)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.Toastytrade as TT
import Eth.Types exposing (Address)
import EthHelpers
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type OpenMode
    = BuyerOpened
    | SellerOpened


openModeToInitiatorIsBuyer : OpenMode -> Bool
openModeToInitiatorIsBuyer openMode =
    case openMode of
        BuyerOpened ->
            True

        SellerOpened ->
            False


initiatorIsBuyerToOpenMode : Bool -> OpenMode
initiatorIsBuyerToOpenMode initiatorIsBuyer =
    if initiatorIsBuyer then
        BuyerOpened

    else
        SellerOpened


type Phase
    = Created
    | Open
    | Committed
    | Claimed
    | Closed


type alias UserParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , totalPriceCurrency : String
    , totalPriceValue : TokenValue
    , transferMethods : String
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias CreateParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , totalPriceString : String
    , transferMethods : String
    , initiatorCommPubkey : String
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    , initiatorAddress : Address
    , buyerDeposit : TokenValue
    , pokeReward : TokenValue
    }


type alias State =
    { balance : TokenValue
    , phase : Phase
    , phaseStartTime : Time.Posix
    , responder : Maybe Address
    , responderCommPubkey : Maybe String
    }


txReceiptToCreatedToastytradeSellAddress : Address -> Eth.Types.TxReceipt -> Result String Address
txReceiptToCreatedToastytradeSellAddress factoryAddress txReceipt =
    let
        maybeCreateEventData =
            txReceipt.logs
                |> List.filter (\log -> log.address == factoryAddress)
                |> List.head
                |> Maybe.map (\log -> log.data)
    in
    case maybeCreateEventData of
        Just createEventData ->
            createEventData
                |> String.dropLeft (2 + 64)
                |> String.left 64
                |> Abi.Decode.fromString Abi.Decode.address

        Nothing ->
            Err "Can't find a log generated from the given factoryAddress. Are you looking at the wrong transaction?"


txReceiptToCreatedToastytradeSellId : Address -> Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedToastytradeSellId factoryAddress txReceipt =
    let
        maybeCreateEventData =
            txReceipt.logs
                |> List.filter (\log -> log.address == factoryAddress)
                |> List.head
                |> Maybe.map (\log -> log.data)
    in
    case maybeCreateEventData of
        Just createEventData ->
            createEventData
                -- Remove the '0x' at the beginning
                |> String.dropLeft 2
                -- Take the first 64-char argument
                |> String.left 64
                |> Abi.Decode.fromString Abi.Decode.uint

        Nothing ->
            Err "Can't find a log generated from the given factoryAddress. Are you looking at the wrong transaction?"


bigIntToPhase : BigInt -> Maybe Phase
bigIntToPhase phase =
    let
        phaseInt =
            Maybe.withDefault 99 (BigInt.toString phase |> String.toInt)
    in
    case phaseInt of
        0 ->
            Just Created

        1 ->
            Just Open

        2 ->
            Just Committed

        3 ->
            Just Claimed

        4 ->
            Just Closed

        _ ->
            Nothing


phaseToString : Phase -> String
phaseToString phase =
    case phase of
        Created ->
            "Created"

        Open ->
            "Open"

        Committed ->
            "Committed"

        Claimed ->
            "Claimed"

        Closed ->
            "Closed"


buildCreateParameters : UserInfo -> UserParameters -> CreateParameters
buildCreateParameters initiatorInfo userParameters =
    let
        buyerDeposit =
            TokenValue.divByInt userParameters.tradeAmount 3

        totalPriceString =
            userParameters.totalPriceCurrency
                ++ (userParameters.totalPriceValue |> TokenValue.renderToString Nothing)

        pokeReward =
            TokenValue.updateValue
                userParameters.tradeAmount
                (BigInt.fromInt 2500000000000000)
    in
    { openMode = userParameters.openMode
    , tradeAmount = userParameters.tradeAmount
    , totalPriceString = totalPriceString
    , autorecallInterval = userParameters.autorecallInterval
    , autoabortInterval = userParameters.autoabortInterval
    , autoreleaseInterval = userParameters.autoreleaseInterval
    , transferMethods = userParameters.transferMethods
    , initiatorAddress = initiatorInfo.address
    , initiatorCommPubkey = initiatorInfo.commPubkey
    , buyerDeposit = buyerDeposit
    , pokeReward = pokeReward
    }


decodeState : Int -> TT.GetState -> Maybe State
decodeState numDecimals encodedState =
    let
        maybePhase =
            bigIntToPhase encodedState.phase

        maybePhaseStartTime =
            TimeHelpers.secondsBigIntToMaybePosix encodedState.phaseStartTimestamp
    in
    Maybe.map2
        (\phase phaseStartTime ->
            { balance = TokenValue.tokenValue numDecimals encodedState.balance
            , phase = phase
            , phaseStartTime = phaseStartTime
            , responder = EthHelpers.addressIfNot0x0 encodedState.responder
            , responderCommPubkey =
                case encodedState.responderCommPubkey of
                    "" ->
                        Nothing

                    s ->
                        Just s
            }
        )
        maybePhase
        maybePhaseStartTime
