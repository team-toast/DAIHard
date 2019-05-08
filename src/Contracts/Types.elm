module Contracts.Types exposing (ClosedReason(..), CreateParameters, DAIHardEvent(..), FullTradeInfo, OpenMode(..), PartialTradeInfo, Phase(..), PhaseStartInfo, State, TimeoutInfo(..), Trade(..), TradeCreationInfo, TradeParameters, UserParameters, bigIntToPhase, buildCreateParameters, decodePhaseStartInfo, decodeState, eventDecoder, getBuyerOrSeller, getCurrentPhaseTimeoutInfo, getInitiatorOrResponder, getPhaseInterval, getPokeText, getResponderRole, initiatorIsBuyerToOpenMode, initiatorOrResponderToBuyerOrSeller, openModeToInitiatorIsBuyer, partialTradeInfo, phaseIcon, phaseToInt, phaseToString, responderDeposit, txReceiptToCreatedTradeSellId, updateCreationInfo, updateParameters, updatePaymentMethods, updatePhaseStartInfo, updateState)

import Abi.Decode
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Eth.Decode
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import FiatValue exposing (FiatValue)
import Images exposing (Image)
import Json.Decode
import Margin
import Network exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type Trade
    = PartiallyLoadedTrade PartialTradeInfo
    | LoadedTrade FullTradeInfo


type alias PartialTradeInfo =
    { factoryID : Int
    , creationInfo : Maybe TradeCreationInfo
    , parameters : Maybe TradeParameters
    , state : Maybe State
    , phaseStartInfo : Maybe PhaseStartInfo
    , paymentMethods : Maybe (List PaymentMethod)
    }


type alias FullTradeInfo =
    { factoryID : Int
    , creationInfo : TradeCreationInfo
    , parameters : TradeParameters
    , state : State
    , phaseStartInfo : PhaseStartInfo
    , derived : DerivedValues
    , paymentMethods : List PaymentMethod
    }


type alias TradeCreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias DerivedValues =
    { phaseEndTime : Time.Posix
    , margin : Maybe Float
    }


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


type OpenMode
    = BuyerOpened
    | SellerOpened


type alias State =
    { balance : TokenValue
    , phase : Phase
    , phaseStartTime : Time.Posix
    , responder : Maybe Address
    , closedReason : ClosedReason
    }


type Phase
    = Open
    | Committed
    | Claimed
    | Closed


type ClosedReason
    = NotClosed
    | Recalled
    | Aborted
    | Released
    | Burned


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


type alias PhaseStartInfo =
    { openedBlock : BigInt
    , committedBlock : BigInt
    , claimedBlock : BigInt
    , closedBlock : BigInt
    , openedTime : Time.Posix
    , committedTime : Time.Posix
    , claimedTime : Time.Posix
    , closedTime : Time.Posix
    }


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


getResponderRole : TradeParameters -> BuyerOrSeller
getResponderRole parameters =
    case parameters.openMode of
        BuyerOpened ->
            Seller

        SellerOpened ->
            Buyer


responderDeposit : TradeParameters -> TokenValue
responderDeposit parameters =
    case parameters.openMode of
        BuyerOpened ->
            parameters.tradeAmount

        SellerOpened ->
            parameters.buyerDeposit


partialTradeInfo : Int -> Trade
partialTradeInfo factoryID =
    PartiallyLoadedTrade (PartialTradeInfo factoryID Nothing Nothing Nothing Nothing Nothing)


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


updatePhaseStartInfo : PhaseStartInfo -> Trade -> Trade
updatePhaseStartInfo phaseStartInfo trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | phaseStartInfo = Just phaseStartInfo }
                |> checkIfTradeLoaded

        LoadedTrade info ->
            let
                _ =
                    Debug.log "Trying to update phaseStartInfo on a trade that's already fully loaded!" ""
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
    case ( ( pInfo.creationInfo, pInfo.parameters ), ( pInfo.state, pInfo.paymentMethods ), pInfo.phaseStartInfo ) of
        ( ( Just creationInfo, Just parameters ), ( Just state, Just paymentMethods ), Just phaseStartInfo ) ->
            LoadedTrade
                (FullTradeInfo
                    pInfo.factoryID
                    creationInfo
                    parameters
                    state
                    phaseStartInfo
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


type TimeoutInfo
    = TimeUp Time.Posix
    | TimeLeft ( Time.Posix, Time.Posix )


getCurrentPhaseTimeoutInfo : Time.Posix -> FullTradeInfo -> TimeoutInfo
getCurrentPhaseTimeoutInfo currentTime tradeInfo =
    let
        elapsed =
            TimeHelpers.sub
                currentTime
                tradeInfo.state.phaseStartTime

        totalInterval =
            getPhaseInterval tradeInfo.state.phase tradeInfo

        timeLeft =
            TimeHelpers.sub
                totalInterval
                elapsed
    in
    if Time.posixToMillis timeLeft > 0 then
        TimeLeft ( timeLeft, totalInterval )

    else
        TimeUp totalInterval


getPhaseInterval : Phase -> FullTradeInfo -> Time.Posix
getPhaseInterval phase tradeInfo =
    case phase of
        Open ->
            tradeInfo.parameters.autorecallInterval

        Committed ->
            tradeInfo.parameters.autoabortInterval

        Claimed ->
            tradeInfo.parameters.autoreleaseInterval

        Closed ->
            Time.millisToPosix 0


getPokeText : Phase -> String
getPokeText phase =
    case phase of
        Open ->
            "Expiring..."

        Committed ->
            "Aborting..."

        Claimed ->
            "Releasing..."

        Closed ->
            ""


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


txReceiptToCreatedTradeSellId : Network -> Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedTradeSellId network txReceipt =
    txReceipt.logs
        |> List.filter
            (\log ->
                (Eth.Utils.addressToString >> String.toLower) log.address
                    == (Eth.Utils.addressToString >> String.toLower) (factoryAddress network)
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


bigIntToClosedReason : BigInt -> Maybe ClosedReason
bigIntToClosedReason i =
    let
        reasonInt =
            Maybe.withDefault 99 (BigInt.toString i |> String.toInt)
    in
    case reasonInt of
        0 ->
            Just NotClosed

        1 ->
            Just Recalled

        2 ->
            Just Aborted

        3 ->
            Just Released

        4 ->
            Just Burned

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

        maybeClosedReason =
            bigIntToClosedReason encodedState.closedReason
    in
    Maybe.map3
        (\phase phaseStartTime closedReason ->
            { balance = TokenValue.tokenValue numDecimals encodedState.balance
            , phase = phase
            , phaseStartTime = phaseStartTime
            , responder = EthHelpers.addressIfNot0x0 encodedState.responder
            , closedReason = closedReason
            }
        )
        maybePhase
        maybePhaseStartTime
        maybeClosedReason


decodePhaseStartInfo : DHT.GetPhaseStartInfo -> Maybe PhaseStartInfo
decodePhaseStartInfo encoded =
    Maybe.map4
        (\openedTimeBigInt committedTimeBigInt claimedTimeBigInt closedTimeBigInt ->
            { openedTime = openedTimeBigInt
            , committedTime = committedTimeBigInt
            , claimedTime = claimedTimeBigInt
            , closedTime = closedTimeBigInt
            , openedBlock = encoded.v1
            , committedBlock = encoded.v2
            , claimedBlock = encoded.v3
            , closedBlock = encoded.v4
            }
        )
        (TimeHelpers.secondsBigIntToMaybePosix encoded.v6)
        (TimeHelpers.secondsBigIntToMaybePosix encoded.v7)
        (TimeHelpers.secondsBigIntToMaybePosix encoded.v8)
        (TimeHelpers.secondsBigIntToMaybePosix encoded.v9)


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


phaseIcon : Phase -> Image
phaseIcon phase =
    case phase of
        Open ->
            Images.openPhase

        Committed ->
            Images.committedPhase

        Claimed ->
            Images.claimedPhase

        Closed ->
            Images.none
