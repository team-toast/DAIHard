module Contracts.Types exposing (ClosedReason(..), CreateParameters, DAIHardEvent(..), FullTradeInfo, PartialTradeInfo, Phase(..), PhaseStartInfo, State, Terms, TimeoutInfo(..), Trade(..), TradeCreationInfo, TradeParameters, UserParameters, bigIntToPhase, buildCreateParameters, decodeParameters, decodePhaseStartInfo, decodeState, decodeTerms, defaultAbortPunishment, defaultBuyerDeposit, encodeTerms, eventDecoder, getBuyerOrSeller, getCurrentPhaseTimeoutInfo, getDevFee, getInitiatorOrResponder, getPhaseInterval, getPokeText, getResponderRole, initiatorOrResponderToBuyerOrSeller, partialTradeInfo, phaseIcon, phaseToInt, phaseToString, responderDeposit, txReceiptToCreatedTradeSellId, updateCreationInfo, updateParameters, updatePhaseStartInfo, updateState, updateTerms)

import Abi.Decode
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Eth.Decode
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import FiatValue exposing (FiatValue)
import Images exposing (Image)
import Json.Decode
import Json.Encode
import Margin
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
    , terms : Maybe Terms
    , phaseStartInfo : Maybe PhaseStartInfo
    }


type alias FullTradeInfo =
    { factoryID : Int
    , creationInfo : TradeCreationInfo
    , parameters : TradeParameters
    , state : State
    , terms : Terms
    , phaseStartInfo : PhaseStartInfo
    , derived : DerivedValues
    }


type alias TradeCreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias DerivedValues =
    { phaseEndTime : Time.Posix
    , margin : Maybe Float
    }


type alias Terms =
    { price : FiatValue
    , paymentMethods : List PaymentMethod
    }


type alias UserParameters =
    { initiatingParty : BuyerOrSeller
    , tradeAmount : TokenValue
    , price : FiatValue
    , paymentMethods : List PaymentMethod
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias TradeParameters =
    { initiatingParty : BuyerOrSeller
    , tradeAmount : TokenValue
    , buyerDeposit : TokenValue
    , abortPunishment : TokenValue
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    , initiatorAddress : Address
    , pokeReward : TokenValue
    }


type alias CreateParameters =
    { initiatingParty : BuyerOrSeller
    , tradeAmount : TokenValue
    , price : FiatValue
    , buyerDeposit : TokenValue
    , abortPunishment : TokenValue
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
    = InitiatedEvent DHT.Initiated
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


defaultBuyerDeposit : TokenValue -> TokenValue
defaultBuyerDeposit tradeAmount =
    TokenValue.div tradeAmount 3


defaultAbortPunishment : TokenValue -> TokenValue
defaultAbortPunishment tradeAmount =
    TokenValue.div tradeAmount 12


getDevFee : TokenValue -> TokenValue
getDevFee tradeAmount =
    TokenValue.div tradeAmount 200


getResponderRole : TradeParameters -> BuyerOrSeller
getResponderRole parameters =
    case parameters.initiatingParty of
        Buyer ->
            Seller

        Seller ->
            Buyer


responderDeposit : TradeParameters -> TokenValue
responderDeposit parameters =
    case parameters.initiatingParty of
        Buyer ->
            parameters.tradeAmount

        Seller ->
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


updateTerms : Terms -> Trade -> Trade
updateTerms terms trade =
    case trade of
        PartiallyLoadedTrade pInfo ->
            { pInfo | terms = Just terms }
                |> checkIfTradeLoaded

        LoadedTrade info ->
            let
                _ =
                    Debug.log "Trying to update terms on a trade that's already fully loaded!" ""
            in
            trade


checkIfTradeLoaded : PartialTradeInfo -> Trade
checkIfTradeLoaded pInfo =
    case ( ( pInfo.creationInfo, pInfo.parameters ), ( pInfo.state, pInfo.terms ), pInfo.phaseStartInfo ) of
        ( ( Just creationInfo, Just parameters ), ( Just state, Just terms ), Just phaseStartInfo ) ->
            LoadedTrade
                (FullTradeInfo
                    pInfo.factoryID
                    creationInfo
                    parameters
                    state
                    terms
                    phaseStartInfo
                    (deriveValues parameters state terms)
                )

        _ ->
            PartiallyLoadedTrade pInfo


deriveValues : TradeParameters -> State -> Terms -> DerivedValues
deriveValues parameters state terms =
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
        Margin.margin parameters.tradeAmount terms.price
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
                case ( initiatorOrResponder, tradeInfo.parameters.initiatingParty ) of
                    ( Initiator, Seller ) ->
                        Seller

                    ( Initiator, Buyer ) ->
                        Buyer

                    ( Responder, Seller ) ->
                        Buyer

                    ( Responder, Buyer ) ->
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
                if hashedSig == Eth.Utils.keccak256 "Initiated(string,string)" then
                    Json.Decode.map InitiatedEvent DHT.initiatedDecoder

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


decodeParameters : DHT.GetParameters -> Result String TradeParameters
decodeParameters encodedParameters =
    let
        autorecallIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autorecallInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"

        depositDeadlineIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoabortInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"

        autoreleaseIntervalResult =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoreleaseInterval
                |> Result.fromMaybe "error converting BigInt to Time.Posix"
    in
    Result.map3
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval ->
            { initiatingParty =
                if encodedParameters.initiatorIsCustodian then
                    Seller

                else
                    Buyer
            , tradeAmount = TokenValue.tokenValue encodedParameters.tradeAmount
            , buyerDeposit = TokenValue.tokenValue encodedParameters.beneficiaryDeposit
            , abortPunishment = TokenValue.tokenValue encodedParameters.abortPunishment
            , autorecallInterval = autorecallInterval
            , autoabortInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , pokeReward = TokenValue.tokenValue encodedParameters.pokeReward
            }
        )
        autorecallIntervalResult
        depositDeadlineIntervalResult
        autoreleaseIntervalResult


eventSigDecoder : Json.Decode.Decoder Eth.Types.Hex
eventSigDecoder =
    Json.Decode.field "topics" (Json.Decode.index 0 Eth.Decode.hex)


txReceiptToCreatedTradeSellId : Network -> Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedTradeSellId network txReceipt =
    txReceipt.logs
        |> List.filter
            (\log ->
                (Eth.Utils.addressToString >> String.toLower) log.address
                    == (Eth.Utils.addressToString >> String.toLower) (Config.factoryAddress network)
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
            TokenValue.zero
    in
    { initiatingParty = userParameters.initiatingParty
    , tradeAmount = userParameters.tradeAmount
    , buyerDeposit = defaultBuyerDeposit userParameters.tradeAmount
    , abortPunishment = defaultAbortPunishment userParameters.tradeAmount
    , price = userParameters.price
    , autorecallInterval = userParameters.autorecallInterval
    , autoabortInterval = userParameters.autoabortInterval
    , autoreleaseInterval = userParameters.autoreleaseInterval
    , initiatorAddress = initiatorInfo.address
    , pokeReward = pokeReward
    , initiatorCommPubkey = initiatorInfo.commPubkey
    , paymentMethods = userParameters.paymentMethods
    }


decodeState : DHT.GetState -> Maybe State
decodeState encodedState =
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
            { balance = TokenValue.tokenValue encodedState.balance
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


initiatorOrResponderToBuyerOrSeller : BuyerOrSeller -> InitiatorOrResponder -> BuyerOrSeller
initiatorOrResponderToBuyerOrSeller initiatingParty initiatorOrResponder =
    case ( initiatorOrResponder, initiatingParty ) of
        ( Initiator, Buyer ) ->
            Buyer

        ( Initiator, Seller ) ->
            Seller

        ( Responder, Buyer ) ->
            Seller

        ( Responder, Seller ) ->
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


encodeTerms : Terms -> String
encodeTerms terms =
    let
        encodedPaymentMethods =
            Json.Encode.list PaymentMethods.encode
                terms.paymentMethods

        encodedPrice =
            FiatValue.encode terms.price
    in
    Json.Encode.object
        [ ( "paymentmethods", encodedPaymentMethods )
        , ( "price", encodedPrice )
        ]
        |> Json.Encode.encode 0


decodeTerms : String -> Result Json.Decode.Error Terms
decodeTerms encodedTerms =
    let
        decoder =
            Json.Decode.map2
                Terms
                (Json.Decode.field "price" FiatValue.decoder)
                (Json.Decode.field "paymentmethods"
                    (Json.Decode.list PaymentMethods.decoder)
                )
    in
    Json.Decode.decodeString decoder encodedTerms
