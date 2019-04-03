module Contracts.Types exposing (CreateParameters, DAIHardEvent(..), FullTradeInfo, OpenMode(..), PartialTradeInfo, Phase(..), State, Trade(..), TradeCreationInfo, TradeParameters, UserParameters, bigIntToPhase, buildCreateParameters, decodeState, eventDecoder, getBuyerOrSeller, getInitiatorOrResponder, initiatorIsBuyerToOpenMode, initiatorOrResponderToBuyerOrSeller, openModeToInitiatorIsBuyer, partialTradeInfo, phaseToInt, phaseToString, txReceiptToCreatedToastytradeSellId, updateCreationInfo, updateParameters, updatePaymentMethods, updateState)

import Abi.Decode
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Constants exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Eth.Decode
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import FiatValue exposing (FiatValue)
import Json.Decode
import Margin
import PaymentMethods exposing (PaymentMethod)
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
    = Open
    | Committed
    | Claimed
    | Closed


type alias UserParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , fiatPrice : FiatValue
    , paymentMethods : List PaymentMethod
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias TradeParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , fiatPrice : FiatValue
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    , initiatorAddress : Address
    , buyerDeposit : TokenValue
    , pokeReward : TokenValue
    }


type alias CreateParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , fiatPrice : FiatValue
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    , initiatorAddress : Address
    , pokeReward : TokenValue
    , initiatorCommPubkey : String
    , paymentMethods : List PaymentMethod
    }


type alias State =
    { balance : TokenValue
    , phase : Phase
    , phaseStartTime : Time.Posix
    , responder : Maybe Address
    }


type DAIHardEvent
    = OpenedEvent DHT.Opened
    | CommittedEvent DHT.Committed
    | RecalledEvent
    | ClaimedEvent
    | AbortedEvent
    | ReleasedEvent
    | BurnedEvent
    | PokeEvent
    | InitiatorStatementLogEvent DHT.InitiatorStatementLog
    | ResponderStatementLogEvent DHT.ResponderStatementLog


type Trade
    = PartiallyLoadedTrade PartialTradeInfo
    | LoadedTrade FullTradeInfo


type alias PartialTradeInfo =
    { factoryID : Int
    , creationInfo : Maybe TradeCreationInfo
    , parameters : Maybe TradeParameters
    , state : Maybe State
    , paymentMethods : Maybe (List PaymentMethod)
    }


type alias TradeCreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias FullTradeInfo =
    { factoryID : Int
    , creationInfo : TradeCreationInfo
    , parameters : TradeParameters
    , state : State
    , derived : DerivedValues
    , paymentMethods : List PaymentMethod
    }


type alias DerivedValues =
    { phaseEndTime : Time.Posix
    , margin : Maybe Float
    }


partialTradeInfo : Int -> Trade
partialTradeInfo factoryID =
    PartiallyLoadedTrade (PartialTradeInfo factoryID Nothing Nothing Nothing Nothing)


updateCreationInfo : TradeCreationInfo -> Trade -> Trade
updateCreationInfo creationInfo trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | creationInfo = Just creationInfo }
                |> checkIfTradeLoaded

        LoadedTrade _ ->
            let
                _ =
                    Debug.log "Trying to update creation info on a trade that's already fully loaded!" ""
            in
            trade


updateParameters : TradeParameters -> Trade -> Trade
updateParameters parameters trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | parameters = Just parameters }
                |> checkIfTradeLoaded

        LoadedTrade info ->
            let
                _ =
                    Debug.log "Trying to update parameters on a trade that's already fully loaded!" ""
            in
            trade


updateState : State -> Trade -> Trade
updateState state trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | state = Just state }
                |> checkIfTradeLoaded

        LoadedTrade info ->
            LoadedTrade { info | state = state }


updatePaymentMethods : List PaymentMethod -> Trade -> Trade
updatePaymentMethods paymentMethods trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | paymentMethods = Just paymentMethods }
                |> checkIfTradeLoaded

        LoadedTrade info ->
            let
                _ =
                    Debug.log "Trying to update payment methods on a trade that's already fully loaded!" ""
            in
            trade


checkIfTradeLoaded : PartialTradeInfo -> Trade
checkIfTradeLoaded pInfo =
    case ( ( pInfo.creationInfo, pInfo.parameters ), ( pInfo.state, pInfo.paymentMethods ) ) of
        ( ( Just creationInfo, Just parameters ), ( Just state, Just paymentMethods ) ) ->
            LoadedTrade
                (FullTradeInfo
                    pInfo.factoryID
                    creationInfo
                    parameters
                    state
                    (deriveValues parameters state)
                    paymentMethods
                )

        _ ->
            PartiallyLoadedTrade pInfo


deriveValues : TradeParameters -> State -> DerivedValues
deriveValues parameters state =
    let
        currentPhaseInterval =
            case state.phase of
                Open ->
                    parameters.autorecallInterval

                Committed ->
                    parameters.autoabortInterval

                Claimed ->
                    parameters.autoreleaseInterval

                Closed ->
                    Time.millisToPosix 0
    in
    { phaseEndTime =
        TimeHelpers.add
            state.phaseStartTime
            currentPhaseInterval
    , margin =
        Margin.margin parameters.tradeAmount parameters.fiatPrice
    }


getInitiatorOrResponder : FullTradeInfo -> Address -> Maybe InitiatorOrResponder
getInitiatorOrResponder tradeInfo userAddress =
    if userAddress == tradeInfo.parameters.initiatorAddress then
        Just Initiator

    else
        tradeInfo.state.responder
            |> Maybe.andThen
                (\responder ->
                    if userAddress == responder then
                        Just Responder

                    else
                        Nothing
                )


getBuyerOrSeller : FullTradeInfo -> Address -> Maybe BuyerOrSeller
getBuyerOrSeller tradeInfo userAddress =
    getInitiatorOrResponder tradeInfo userAddress
        |> Maybe.map
            (\initiatorOrResponder ->
                case ( initiatorOrResponder, tradeInfo.parameters.openMode ) of
                    ( Initiator, SellerOpened ) ->
                        Seller

                    ( Initiator, BuyerOpened ) ->
                        Buyer

                    ( Responder, SellerOpened ) ->
                        Buyer

                    ( Responder, BuyerOpened ) ->
                        Seller
            )


eventDecoder : Json.Decode.Decoder DAIHardEvent
eventDecoder =
    eventSigDecoder
        |> Json.Decode.andThen
            (\hashedSig ->
                if hashedSig == Eth.Utils.keccak256 "Opened(string,string)" then
                    Json.Decode.map OpenedEvent DHT.openedDecoder

                else if hashedSig == Eth.Utils.keccak256 "Committed(address,string)" then
                    Json.Decode.map CommittedEvent DHT.committedDecoder

                else if hashedSig == Eth.Utils.keccak256 "Recalled()" then
                    Json.Decode.succeed RecalledEvent

                else if hashedSig == Eth.Utils.keccak256 "Claimed()" then
                    Json.Decode.succeed ClaimedEvent

                else if hashedSig == Eth.Utils.keccak256 "Aborted()" then
                    Json.Decode.succeed AbortedEvent

                else if hashedSig == Eth.Utils.keccak256 "Released()" then
                    Json.Decode.succeed ReleasedEvent

                else if hashedSig == Eth.Utils.keccak256 "Burned()" then
                    Json.Decode.succeed BurnedEvent

                else if hashedSig == Eth.Utils.keccak256 "InitiatorStatementLog(string,string)" then
                    Json.Decode.map InitiatorStatementLogEvent DHT.initiatorStatementLogDecoder

                else if hashedSig == Eth.Utils.keccak256 "ResponderStatementLog(string,string)" then
                    Json.Decode.map ResponderStatementLogEvent DHT.responderStatementLogDecoder

                else if hashedSig == Eth.Utils.keccak256 "Poke()" then
                    Json.Decode.succeed PokeEvent

                else
                    Json.Decode.fail "Unrecognized topic hash"
            )


eventSigDecoder : Json.Decode.Decoder Eth.Types.Hex
eventSigDecoder =
    Json.Decode.field "topics" (Json.Decode.index 0 Eth.Decode.hex)


txReceiptToCreatedToastytradeSellId : Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedToastytradeSellId txReceipt =
    txReceipt.logs
        |> List.filter
            (\log ->
                (Eth.Utils.addressToString >> String.toLower) log.address
                    == (Eth.Utils.addressToString >> String.toLower) factoryAddress
            )
        |> List.head
        |> Result.fromMaybe "No log found from that factoryAddress in that txReceipt"
        |> Result.andThen
            (\log ->
                (Eth.Decode.event DHF.newTradeDecoder log).returnData
                    |> Result.mapError Json.Decode.errorToString
            )
        |> Result.map .id


bigIntToPhase : BigInt -> Maybe Phase
bigIntToPhase phase =
    let
        phaseInt =
            Maybe.withDefault 99 (BigInt.toString phase |> String.toInt)
    in
    case phaseInt of
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


phaseToInt : Phase -> Int
phaseToInt phase =
    case phase of
        Open ->
            1

        Committed ->
            2

        Claimed ->
            3

        Closed ->
            4


phaseToString : Phase -> String
phaseToString phase =
    case phase of
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
        pokeReward =
            TokenValue.updateValue
                userParameters.tradeAmount
                (BigInt.fromInt 0)
    in
    { openMode = userParameters.openMode
    , tradeAmount = userParameters.tradeAmount
    , fiatPrice = userParameters.fiatPrice
    , autorecallInterval = userParameters.autorecallInterval
    , autoabortInterval = userParameters.autoabortInterval
    , autoreleaseInterval = userParameters.autoreleaseInterval
    , initiatorAddress = initiatorInfo.address
    , pokeReward = pokeReward
    , initiatorCommPubkey = initiatorInfo.commPubkey
    , paymentMethods = userParameters.paymentMethods
    }


decodeState : Int -> DHT.GetState -> Maybe State
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
            }
        )
        maybePhase
        maybePhaseStartTime


initiatorOrResponderToBuyerOrSeller : OpenMode -> InitiatorOrResponder -> BuyerOrSeller
initiatorOrResponderToBuyerOrSeller openMode initiatorOrResponder =
    case ( initiatorOrResponder, openMode ) of
        ( Initiator, BuyerOpened ) ->
            Buyer

        ( Initiator, SellerOpened ) ->
            Seller

        ( Responder, BuyerOpened ) ->
            Seller

        ( Responder, SellerOpened ) ->
            Buyer
