module Components exposing (EthNode, TokenAmount, addressIfNot0x0, bigIntToTime, ethNode, pullAnyFirstDecimalOffToRight, removeTrailingZeros, stringToTokenAmount, tokenAmountToString, zeroTokens)

-- Library

import BigInt exposing (BigInt)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Types exposing (..)
import Eth.Utils as EthUtils
import Time exposing (Time)


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


type alias TokenAmount =
    { numDecimals : Int
    , evmValue : BigInt
    }


zeroTokens : Int -> TokenAmount
zeroTokens numDecimals =
    TokenAmount numDecimals (BigInt.fromInt 0)


tokenAmountToString : TokenAmount -> Int -> String
tokenAmountToString tokenAmount displayPrecision =
    let
        firstTruncateAmount =
            tokenAmount.numDecimals - displayPrecision

        divisor =
            BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt firstTruncateAmount)

        ( truncatedAmount, remainder ) =
            Maybe.withDefault ( tokenAmount.evmValue, BigInt.fromInt 0 ) (BigInt.divmod tokenAmount.evmValue divisor)

        firstDigitOfRemainder =
            remainder
                |> BigInt.toString
                |> String.slice 0 1
                |> String.toInt
                |> Result.withDefault 0

        truncatedAmountRounded =
            if firstDigitOfRemainder > 5 then
                BigInt.add truncatedAmount (BigInt.fromInt 1)

            else
                truncatedAmount

        amountStringNoDecimal =
            BigInt.toString truncatedAmount
    in
    if displayPrecision == 0 then
        amountStringNoDecimal

    else
        (String.dropRight displayPrecision amountStringNoDecimal
            ++ "."
            ++ String.right displayPrecision amountStringNoDecimal
        )
            |> removeTrailingZeros


removeTrailingZeros : String -> String
removeTrailingZeros numString =
    if (numString |> String.endsWith "0") || (numString |> String.endsWith ".") then
        removeTrailingZeros (String.slice 0 -1 numString)

    else
        numString


stringToTokenAmount : String -> Int -> Result String TokenAmount
stringToTokenAmount amountString numDecimals =
    let
        ( newString, numDigitsMoved ) =
            pullAnyFirstDecimalOffToRight amountString

        numDigitsLeftToMove =
            numDecimals - numDigitsMoved

        maybeBigIntAmount =
            BigInt.fromString newString
    in
    if numDigitsLeftToMove < 0 then
        Err "Number is too precise!"

    else
        case maybeBigIntAmount of
            Nothing ->
                Err "Can't interpret that number!"

            Just bigIntAmount ->
                let
                    evmValue =
                        BigInt.mul bigIntAmount (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt numDigitsLeftToMove))
                in
                Ok (TokenAmount numDecimals evmValue)


pullAnyFirstDecimalOffToRight : String -> ( String, Int )
pullAnyFirstDecimalOffToRight numString =
    let
        maybeDecimalPosition =
            List.head (String.indexes "." numString)
    in
    case maybeDecimalPosition of
        Nothing ->
            ( numString, 0 )

        Just decimalPos ->
            let
                numDigitsMoved =
                    (String.length numString - 1) - decimalPos

                newString =
                    String.left decimalPos numString
                        ++ String.dropLeft (decimalPos + 1) numString
            in
            ( newString, numDigitsMoved )


bigIntToTime : BigInt -> Result String Time
bigIntToTime time =
    BigInt.toString time
        |> String.toInt
        |> Result.map (\t -> toFloat t * Time.second)


addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
    if addr == EthUtils.unsafeToAddress "0x0000000000000000000000000000000000000000" then
        Nothing

    else
        Just addr
