module TokenValue exposing (TokenValue, add, decoder, div, divByInt, encode, fromString, getBigInt, getFloatValueWithWarning, isZero, mul, numDecimals, renderToString, sub, tokenValue, updateValue, updateViaString, zero)

import BigInt exposing (BigInt)
import BigIntHelpers
import Json.Decode
import Json.Encode


type TokenValue
    = TokenValue
        { numDecimals : Int
        , value : BigInt
        }


tokenValue : Int -> BigInt -> TokenValue
tokenValue numDecimals_ value_ =
    TokenValue
        { numDecimals = numDecimals_
        , value = value_
        }


fromString : Int -> String -> Maybe TokenValue
fromString numDecimals_ s =
    updateViaString (zero numDecimals_) s


zero : Int -> TokenValue
zero numDecimals_ =
    TokenValue
        { numDecimals = numDecimals_
        , value = BigInt.fromInt 0
        }


isZero : TokenValue -> Bool
isZero tv =
    BigInt.fromInt 0 == getBigInt tv


updateValue : TokenValue -> BigInt -> TokenValue
updateValue (TokenValue tokens) newValue =
    TokenValue { tokens | value = newValue }


updateViaString : TokenValue -> String -> Maybe TokenValue
updateViaString (TokenValue originalTokens) s =
    case stringToEvmValue originalTokens.numDecimals s of
        Just newValue ->
            Just (updateValue (TokenValue originalTokens) newValue)

        Nothing ->
            Nothing


numDecimals : TokenValue -> Int
numDecimals tokens_ =
    case tokens_ of
        TokenValue tokens ->
            tokens.numDecimals


getBigInt : TokenValue -> BigInt
getBigInt (TokenValue tokens) =
    tokens.value


getFloatValueWithWarning : TokenValue -> Float
getFloatValueWithWarning tokens =
    let
        toFloat =
            tokens
                |> renderToString Nothing
                |> String.toFloat
    in
    case toFloat of
        Just f ->
            f

        Nothing ->
            let
                _ =
                    Debug.log "Error converting tokenValue to float--string -> float failed!" tokens
            in
            0


renderToString : Maybe Int -> TokenValue -> String
renderToString maxDigitsAfterDecimal tokens =
    case maxDigitsAfterDecimal of
        Nothing ->
            evmValueToString (numDecimals tokens) (getBigInt tokens)

        Just maxDigits ->
            evmValueToTruncatedString (numDecimals tokens) maxDigits (getBigInt tokens)


add : TokenValue -> TokenValue -> TokenValue
add t1 t2 =
    BigInt.add
        (getBigInt t1)
        (getBigInt t2)
        |> updateValue t1


sub : TokenValue -> TokenValue -> TokenValue
sub t1 t2 =
    BigInt.sub
        (getBigInt t1)
        (getBigInt t2)
        |> updateValue t1


mul : TokenValue -> TokenValue -> TokenValue
mul t1 t2 =
    BigInt.mul
        (getBigInt t1)
        (getBigInt t2)
        |> updateValue t1


div : TokenValue -> TokenValue -> TokenValue
div t1 t2 =
    BigInt.div
        (getBigInt t1)
        (getBigInt t2)
        |> updateValue t1


divByInt : TokenValue -> Int -> TokenValue
divByInt t i =
    BigInt.div
        (getBigInt t)
        (BigInt.fromInt i)
        |> updateValue t


encode : TokenValue -> Json.Encode.Value
encode tv =
    tv
        |> getBigInt
        |> BigIntHelpers.encode


decoder : Int -> Json.Decode.Decoder TokenValue
decoder decimals =
    Json.Decode.map (tokenValue decimals) BigIntHelpers.decoder



-- Internal


stringToEvmValue : Int -> String -> Maybe BigInt
stringToEvmValue numDecimals_ amountString =
    if amountString == "" then
        Nothing

    else
        let
            ( newString, numDigitsMoved ) =
                pullAnyFirstDecimalOffToRight amountString

            numDigitsLeftToMove =
                numDecimals_ - numDigitsMoved

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
evmValueToTruncatedString numDecimals_ maxDigitsAfterDecimal evmValue =
    let
        untruncatedString =
            evmValueToString numDecimals_ evmValue

        maybeDecimalPos =
            List.head (String.indexes "." untruncatedString)
    in
    case maybeDecimalPos of
        Nothing ->
            untruncatedString

        Just decimalPos ->
            if maxDigitsAfterDecimal == 0 then
                String.left decimalPos untruncatedString

            else
                String.left (decimalPos + 1 + maxDigitsAfterDecimal) untruncatedString


evmValueToString : Int -> BigInt -> String
evmValueToString numDecimals_ evmValue =
    let
        zeroPaddedString =
            evmValue
                |> BigInt.toString
                |> String.padLeft numDecimals_ '0'

        withDecimalString =
            String.dropRight numDecimals_ zeroPaddedString
                ++ "."
                ++ String.right numDecimals_ zeroPaddedString
    in
    removeUnnecessaryZerosAndDots withDecimalString


removeUnnecessaryZerosAndDots : String -> String
removeUnnecessaryZerosAndDots numString =
    if String.endsWith "." numString then
        String.slice 0 -1 numString

    else if String.endsWith "0" numString then
        removeUnnecessaryZerosAndDots (String.slice 0 -1 numString)

    else if numString == "" then
        "0"

    else
        numString
