module TokenValue exposing (TokenValue(..), add, compare, decoder, div, divFloatWithWarning, encode, evmValueToTruncatedUserFloatString, evmValueToUserFloatString, fromFloatWithWarning, fromIntTokenValue, fromString, getEvmValue, getFloatValueWithWarning, isZero, mul, mulFloatWithWarning, negate, pullAnyFirstDecimalOffToRight, removeUnnecessaryZerosAndDots, sub, toConciseString, toFloatString, tokenValue, userStringToEvmValue, zero)

import BigInt exposing (BigInt)
import Config
import FormatFloat exposing (..)
import Helpers.BigInt as BigIntHelpers
import Json.Decode
import Json.Encode


type TokenValue
    = TokenValue BigInt


tokenValue : BigInt -> TokenValue
tokenValue evmValue =
    TokenValue evmValue


fromIntTokenValue : Int -> TokenValue
fromIntTokenValue val =
    BigInt.fromInt val
        |> BigInt.mul
            (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt Config.tokenDecimals))
        |> tokenValue


fromFloatWithWarning : Float -> TokenValue
fromFloatWithWarning val =
    case userStringToEvmValue (String.fromFloat val) of
        Just bigint ->
            tokenValue bigint

        Nothing ->
            let
                _ =
                    Debug.log "Error converting float to tokenValue" ""
            in
            tokenValue (BigInt.fromInt 0)


fromString : String -> Maybe TokenValue
fromString s =
    Maybe.map
        TokenValue
        (userStringToEvmValue s)


zero : TokenValue
zero =
    TokenValue <| BigInt.fromInt 0


isZero : TokenValue -> Bool
isZero tv =
    getEvmValue tv == BigInt.fromInt 0


getEvmValue : TokenValue -> BigInt
getEvmValue (TokenValue tokens) =
    tokens


getFloatValueWithWarning : TokenValue -> Float
getFloatValueWithWarning tokens =
    case tokens |> toFloatString Nothing |> String.toFloat of
        Just f ->
            f

        Nothing ->
            let
                _ =
                    Debug.log "Error converting tokenValue to float: string -> float failed" tokens
            in
            0


toFloatString : Maybe Int -> TokenValue -> String
toFloatString maxDigitsAfterDecimal tokens =
    case maxDigitsAfterDecimal of
        Nothing ->
            evmValueToUserFloatString (getEvmValue tokens)

        Just maxDigits ->
            evmValueToTruncatedUserFloatString maxDigits (getEvmValue tokens)


toConciseString : TokenValue -> String
toConciseString tv =
    getFloatValueWithWarning tv
        |> autoFormatFloat



-- oldToConciseString tv =
--     if BigInt.compare (getEvmValue tv) (BigInt.fromInt 0) == LT then
--         "-" ++ toConciseString (negate tv)
--     else
--         let
--             hackyRoundFloatString s =
--                 String.toFloat s
--                     |> Maybe.map ((*) 100.0)
--                     |> Maybe.map round
--                     |> Maybe.map toFloat
--                     |> Maybe.map (\f -> f / 100.0)
--                     |> Maybe.map String.fromFloat
--                     |> Maybe.withDefault s
--                     |> String.left 4
--             floatStr =
--                 evmValueToUserFloatString (getEvmValue tv)
--         in
--         case String.indexes "." floatStr of
--             [] ->
--                 floatStr
--             [ 0 ] ->
--                 "0"
--                     ++ floatStr
--                     |> hackyRoundFloatString
--             [ 1 ] ->
--                 hackyRoundFloatString floatStr
--             [ i ] ->
--                 String.toFloat floatStr
--                     |> Maybe.map round
--                     |> Maybe.map String.fromInt
--                     |> Maybe.withDefault (String.left i floatStr)
--             _ ->
--                 let
--                     _ =
--                         Debug.log "Error interpreting evmValueToString result. More than one decimal??"
--                 in
--                 "???"


negate : TokenValue -> TokenValue
negate t =
    getEvmValue t
        |> BigInt.negate
        |> tokenValue


add : TokenValue -> TokenValue -> TokenValue
add t1 t2 =
    BigInt.add
        (getEvmValue t1)
        (getEvmValue t2)
        |> TokenValue


sub : TokenValue -> TokenValue -> TokenValue
sub t1 t2 =
    BigInt.sub
        (getEvmValue t1)
        (getEvmValue t2)
        |> TokenValue


mul : TokenValue -> Int -> TokenValue
mul t i =
    BigInt.mul
        (getEvmValue t)
        (BigInt.fromInt i)
        |> TokenValue


mulFloatWithWarning : TokenValue -> Float -> TokenValue
mulFloatWithWarning t f =
    getFloatValueWithWarning t
        * f
        |> fromFloatWithWarning


div : TokenValue -> Int -> TokenValue
div t i =
    BigInt.div
        (getEvmValue t)
        (BigInt.fromInt i)
        |> TokenValue


divFloatWithWarning : TokenValue -> Float -> TokenValue
divFloatWithWarning t f =
    getFloatValueWithWarning t
        / f
        |> fromFloatWithWarning


compare : TokenValue -> TokenValue -> Order
compare t1 t2 =
    BigInt.compare
        (getEvmValue t1)
        (getEvmValue t2)


encode : TokenValue -> Json.Encode.Value
encode tv =
    tv
        |> getEvmValue
        |> BigIntHelpers.encode


decoder : Json.Decode.Decoder TokenValue
decoder =
    Json.Decode.map
        tokenValue
        BigIntHelpers.decoder



-- Internal


userStringToEvmValue : String -> Maybe BigInt
userStringToEvmValue amountString =
    if amountString == "" then
        Nothing

    else
        let
            ( newString, numDigitsMoved ) =
                pullAnyFirstDecimalOffToRight amountString

            numDigitsLeftToMove =
                Config.tokenDecimals - numDigitsMoved

            maybeBigIntAmount =
                if numDigitsLeftToMove < 0 then
                    -- indicates there is too much precision; we must cut some off the end
                    BigInt.fromString (String.dropRight (Basics.negate numDigitsLeftToMove) newString)

                else
                    BigInt.fromString newString
        in
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


evmValueToTruncatedUserFloatString : Int -> BigInt -> String
evmValueToTruncatedUserFloatString maxDigitsAfterDecimal evmValue =
    let
        untruncatedString =
            evmValueToUserFloatString evmValue

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


evmValueToUserFloatString : BigInt -> String
evmValueToUserFloatString evmValue =
    if BigInt.compare evmValue (BigInt.fromInt 0) == LT then
        "-" ++ evmValueToUserFloatString (BigInt.negate evmValue)

    else
        let
            zeroPaddedString =
                evmValue
                    |> BigInt.toString
                    |> String.padLeft Config.tokenDecimals '0'

            withDecimalString =
                String.dropRight Config.tokenDecimals zeroPaddedString
                    ++ "."
                    ++ String.right Config.tokenDecimals zeroPaddedString
        in
        removeUnnecessaryZerosAndDots withDecimalString
            |> (\s ->
                    if s == "" then
                        "0"

                    else
                        s
               )


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
