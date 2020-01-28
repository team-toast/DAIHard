module Config exposing (activeFactories, bucketSaleAddress, bucketSaleBucketInterval, bucketSaleQueryAddress, bucketSaleTokensPerBucket, bucketTokenSymbol, devFeeAddress, factoryAddress, tokenContractAddress)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils
import Time
import TokenValue exposing (TokenValue)


activeFactories : Bool -> List FactoryType
activeFactories testMode =
    if testMode then
        [ Token KovanDai ]

    else
        [ Token EthDai
        , Native XDai
        ]


tokenContractAddress : TokenFactoryType -> Address
tokenContractAddress tokenFactoryType =
    case tokenFactoryType of
        EthDai ->
            Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"

        KovanDai ->
            Eth.Utils.unsafeToAddress "0x4F96Fe3b7A6Cf9725f59d353F723c1bDb64CA6Aa"


factoryAddress : FactoryType -> Address
factoryAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x3BCd42008DF139Bac1b710605bB7b7839d52672B"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0x780cE226c3bB57C61BaeF6e4D1Bab5Ff89c7684D"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x716C806eC6f4703B35a5F613519C607c72c60408"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xE3C549cCE69CB9d19d50C57743b1C5d2D84841e0"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x237CFfc6c21DF0c58a0b4CcD1d5fe3d43818A27B"


devFeeAddress : FactoryType -> Address
devFeeAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x61F399ED1D5AEC3Bc9d4B026352d5764181d6b35"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0xF59ed429f9753B0498436DE1a3559AEC7a0c2a21"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x61F399ED1D5AEC3Bc9d4B026352d5764181d6b35"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xF59ed429f9753B0498436DE1a3559AEC7a0c2a21"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x092110996699c3E06e998d89F0f4586026e44F0F"


fryAddress : Bool -> Address
fryAddress testMode =
    if testMode then
        Eth.Utils.unsafeToAddress "0x4F1bee416CEcB7Bc3d4A0b94F78e401fb664F4eF"

    else
        Debug.todo "no address for non-testmode FRY"


bucketSaleAddress : Bool -> Address
bucketSaleAddress testMode =
    if testMode then
        Eth.Utils.unsafeToAddress "0xe0d2136293F7c924d6d8a41D58601c43256CC2b7"

    else
        Debug.todo "No address for non-testMode bucketSale"


bucketSaleQueryAddress : Bool -> Address
bucketSaleQueryAddress testMode =
    if testMode then
        Eth.Utils.unsafeToAddress "0x49C6B6892c8229c00D537Bed5d7e60B1E9B4Fd83"

    else
        Debug.todo ""


bucketSaleBucketInterval : Bool -> Time.Posix
bucketSaleBucketInterval testMode =
    if testMode then
        Time.millisToPosix <| 1000 * 60 * 2

    else
        Debug.todo "blocks per bucket in non-test-mode"


bucketSaleTokensPerBucket : Bool -> TokenValue
bucketSaleTokensPerBucket testMode =
    TokenValue.fromIntTokenValue <|
        if testMode then
            150

        else
            Debug.todo "tokens per bucket in non-test mode"


bucketTokenSymbol : String
bucketTokenSymbol =
    "FRY"
