module Contracts.Types exposing (CreateParameters, FullTradeInfo, OpenMode(..), PartialTradeInfo, Phase(..), State, ToastytradeEvent(..), Trade(..), TradeCreationInfo, UserParameters, bigIntToPhase, buildCreateParameters, decodeState, eventDecoder, initiatorIsBuyerToOpenMode, openModeToInitiatorIsBuyer, partialTradeInfo, phaseToString, txReceiptToCreatedToastytradeSellId, updateCreationInfo, updateParameters, updateState)

import Abi.Decode
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.Toastytrade as TT
import Contracts.Generated.ToastytradeFactory as TTF
import Eth.Decode
import Eth.Types exposing (Address)
import Eth.Utils
import EthHelpers
import FiatValue exposing (FiatValue)
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import Utils


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
    , fiatPrice : FiatValue
    , paymentMethods : List PaymentMethod
    , autorecallInterval : Time.Posix
    , autoabortInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias CreateParameters =
    { openMode : OpenMode
    , tradeAmount : TokenValue
    , fiatPrice : FiatValue
    , paymentMethods : List PaymentMethod
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


type alias DerivedValues =
    { phaseEndTime : Time.Posix
    , margin : Maybe Float
    }


type ToastytradeEvent
    = CommittedEvent TT.Committed
    | InitiatorStatementLogEvent TT.InitiatorStatementLog
    | ResponderStatementLogEvent TT.ResponderStatementLog
    | PhaseChangeEvent TT.PhaseChange
    | RecalledEvent
    | AbortedEvent
    | ReleasedEvent
    | BurnedEvent


type Trade
    = PartiallyLoaded PartialTradeInfo
    | Loaded FullTradeInfo


type alias PartialTradeInfo =
    { factoryID : Int
    , creationInfo : Maybe TradeCreationInfo
    , parameters : Maybe CreateParameters
    , state : Maybe State
    }


type alias TradeCreationInfo =
    { address : Address
    , blocknum : Int
    }


type alias FullTradeInfo =
    { factoryID : Int
    , creationInfo : TradeCreationInfo
    , parameters : CreateParameters
    , state : State
    , derived : DerivedValues
    }


partialTradeInfo : Int -> Trade
partialTradeInfo factoryID =
    PartiallyLoaded (PartialTradeInfo factoryID Nothing Nothing Nothing)


updateCreationInfo : TradeCreationInfo -> Trade -> Trade
updateCreationInfo creationInfo trade =
    case trade of
        PartiallyLoaded pInfo ->
            { pInfo | creationInfo = Just creationInfo }
                |> checkIfLoaded

        Loaded _ ->
            let
                _ =
                    Debug.log "Trying to update creation info on a trade that's already been loaded!" ""
            in
            trade


updateParameters : CreateParameters -> Trade -> Trade
updateParameters parameters trade =
    case trade of
        PartiallyLoaded pInfo ->
            { pInfo | parameters = Just parameters }
                |> checkIfLoaded

        Loaded info ->
            let
                _ =
                    Debug.log "Trying to update parameters on a trade that's already been loaded!" ""
            in
            trade


updateState : State -> Trade -> Trade
updateState state trade =
    case trade of
        PartiallyLoaded pInfo ->
            { pInfo | state = Just state }
                |> checkIfLoaded

        Loaded info ->
            Loaded { info | state = state }


checkIfLoaded : PartialTradeInfo -> Trade
checkIfLoaded pInfo =
    case ( pInfo.creationInfo, pInfo.parameters, pInfo.state ) of
        ( Just creationInfo, Just parameters, Just state ) ->
            Loaded
                (FullTradeInfo
                    pInfo.factoryID
                    creationInfo
                    parameters
                    state
                    (deriveValues parameters state)
                )

        _ ->
            PartiallyLoaded pInfo


deriveValues : CreateParameters -> State -> DerivedValues
deriveValues parameters state =
    let
        currentPhaseInterval =
            case state.phase of
                Created ->
                    Time.millisToPosix 0

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
        Utils.margin parameters.tradeAmount parameters.fiatPrice
    }


eventDecoder : Json.Decode.Decoder ToastytradeEvent
eventDecoder =
    topicDecoder 0
        |> Json.Decode.andThen
            (\hashedSig ->
                if hashedSig == Eth.Utils.keccak256 "Committed(address)" then
                    Json.Decode.map CommittedEvent TT.committedDecoder

                else if hashedSig == Eth.Utils.keccak256 "InitiatorStatementLog(string,string)" then
                    Json.Decode.map InitiatorStatementLogEvent TT.initiatorStatementLogDecoder

                else if hashedSig == Eth.Utils.keccak256 "ResponderStatementLog(string,string)" then
                    Json.Decode.map ResponderStatementLogEvent TT.responderStatementLogDecoder

                else if hashedSig == Eth.Utils.keccak256 "PhaseChange(uint8)" then
                    Json.Decode.map PhaseChangeEvent TT.phaseChangeDecoder

                else if hashedSig == Eth.Utils.keccak256 "Recalled()" then
                    Json.Decode.succeed RecalledEvent

                else if hashedSig == Eth.Utils.keccak256 "Aborted()" then
                    Json.Decode.succeed AbortedEvent

                else if hashedSig == Eth.Utils.keccak256 "Released()" then
                    Json.Decode.succeed ReleasedEvent

                else if hashedSig == Eth.Utils.keccak256 "Burned()" then
                    Json.Decode.succeed BurnedEvent

                else
                    Json.Decode.fail "Unrecognized topic hash"
            )


topicDecoder : Int -> Json.Decode.Decoder Eth.Types.Hex
topicDecoder index =
    Json.Decode.field "topics" (Json.Decode.index 0 Eth.Decode.hex)


txReceiptToCreatedToastytradeSellId : Address -> Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedToastytradeSellId factoryAddress txReceipt =
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
                (Eth.Decode.event TTF.newToastytradeDecoder log).returnData
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

        pokeReward =
            TokenValue.updateValue
                userParameters.tradeAmount
                (BigInt.fromInt 2500000000000000)
    in
    { openMode = userParameters.openMode
    , tradeAmount = userParameters.tradeAmount
    , fiatPrice = userParameters.fiatPrice
    , autorecallInterval = userParameters.autorecallInterval
    , autoabortInterval = userParameters.autoabortInterval
    , autoreleaseInterval = userParameters.autoreleaseInterval
    , paymentMethods = userParameters.paymentMethods
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
