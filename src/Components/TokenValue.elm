module TokenValue exposing (TokenValue, empty, getString, numDecimals, renderToString, toBigInt, updateViaBigInt, updateViaString)

import BigInt exposing (BigInt)



-- Exposed


type TokenValue
    = TokenValue
        { numDecimals : Int
        , string : String
        }


numDecimals : TokenValue -> Int
numDecimals tokens =
    case tokens of
        TokenValue tokens ->
            tokens.numDecimals


getString : TokenValue -> String
getString tokens =
    case tokens of
        TokenValue tokens ->
            tokens.string


toBigInt : TokenValue -> Maybe BigInt
toBigInt tokens =
    stringToEvmValue (numDecimals tokens) (getString tokens)


empty : Int -> TokenValue
empty numDecimals =
    TokenValue
        { numDecimals = numDecimals
        , string = ""
        }


renderToString : Maybe Int -> TokenValue -> Maybe String
renderToString maxDigitsAfterDecimal tokens =
    case toBigInt tokens of
        Nothing ->
            Nothing

        Just evmValue ->
            Just
                (case maxDigitsAfterDecimal of
                    Nothing ->
                        evmValueToString (numDecimals tokens) evmValue

                    Just maxDigits ->
                        evmValueToTruncatedString (numDecimals tokens) maxDigits evmValue
                )


updateViaString : TokenValue -> String -> TokenValue
updateViaString (TokenValue originalTokens) newString =
    TokenValue
        { numDecimals = originalTokens.numDecimals
        , string = newString
        }


updateViaBigInt : TokenValue -> Maybe BigInt -> TokenValue
updateViaBigInt (TokenValue tokens) newBigIntValue =
    let
        newString =
            case newBigIntValue of
                Nothing ->
                    ""

                Just bigIntValue ->
                    evmValueToString tokens.numDecimals bigIntValue
    in
    TokenValue { tokens | string = newString }



-- Internal


stringToEvmValue : Int -> String -> Maybe BigInt
stringToEvmValue numDecimals amountString =
    let
        ( newString, numDigitsMoved ) =
            pullAnyFirstDecimalOffToRight amountString

        numDigitsLeftToMove =
            numDecimals - numDigitsMoved

        maybeBigIntAmount =
            BigInt.fromString newString
    in
    if numDigitsLeftToMove < 0 then
        Nothing

    else
        case maybeBigIntAmount of
            Nothing ->
                Nothing

            Just bigIntAmount ->
                let
                    evmValue =
                        BigInt.mul bigIntAmount (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt numDigitsLeftToMove))
                in
                Just evmValue


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


evmValueToTruncatedString : Int -> Int -> BigInt -> String
evmValueToTruncatedString numDecimals maxDigitsAfterDecimal evmValue =
    let
        divisor =
            BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt numDecimals)

        ( truncatedAmount, remainder ) =
            BigInt.divmod evmValue divisor
                -- will return Nothing if divisor is zero. This should never happen.
                |> Maybe.withDefault ( evmValue, BigInt.fromInt 0 )

        zeroPaddedRemainder =
            BigInt.toString remainder
                |> String.padLeft numDecimals '0'

        preDecimalString =
            BigInt.toString truncatedAmount

        postDecimalString =
            zeroPaddedRemainder
                |> String.left maxDigitsAfterDecimal
    in
    preDecimalString
        ++ "."
        ++ postDecimalString
        |> removeTrailingZerosAndDots


evmValueToString : Int -> BigInt -> String
evmValueToString numDecimals evmValue =
    evmValueToTruncatedString numDecimals numDecimals evmValue


removeTrailingZerosAndDots : String -> String
removeTrailingZerosAndDots numString =
    if String.length numString == 0 then
        "0"

    else if (numString |> String.endsWith "0") || (numString |> String.endsWith ".") then
        removeTrailingZerosAndDots (String.slice 0 -1 numString)

    else
        numString
