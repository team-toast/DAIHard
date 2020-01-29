module Contracts.BucketSale.Wrappers exposing (ExitInfo, enter, exit, getFryBalance, getSaleStartTimestampCmd, getTotalExitedTokens, getTotalValueEnteredForBucket, getUserBuyForBucket, getUserExitInfo, httpProvider, queryBigIntListToMaybExitInfo, unlockDai)

import BigInt exposing (BigInt)
import CommonTypes
import Config
import Contracts.BucketSale.Generated.BucketSale as BucketSaleBindings
import Contracts.BucketSale.Generated.BucketSaleQuery as BucketSaleBindings
import Contracts.Generated.ERC20Token as Token
import Eth
import Eth.Types exposing (Address, Call, HttpProvider)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Http
import List.Extra
import Task
import TokenValue exposing (TokenValue)


httpProvider : Bool -> HttpProvider
httpProvider test =
    if test then
        EthHelpers.httpProviderForFactory <| CommonTypes.Token CommonTypes.KovanDai

    else
        Debug.todo "non-test mode not yet defined for httpProvider"


getSaleStartTimestampCmd : Bool -> (Result Http.Error BigInt -> msg) -> Cmd msg
getSaleStartTimestampCmd testMode msgConstructor =
    BucketSaleBindings.startOfSale (Config.bucketSaleAddress testMode)
        |> Eth.call (httpProvider testMode)
        |> Task.attempt msgConstructor


getTotalValueEnteredForBucket : Bool -> Int -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getTotalValueEnteredForBucket testMode bucketId msgConstructor =
    BucketSaleBindings.buckets (Config.bucketSaleAddress testMode) (BigInt.fromInt bucketId)
        |> Eth.call (httpProvider testMode)
        |> Task.map TokenValue.tokenValue
        |> Task.attempt msgConstructor


getUserBuyForBucket : Bool -> Address -> Int -> (Result Http.Error BucketSaleBindings.Buy -> msg) -> Cmd msg
getUserBuyForBucket testMode userAddress bucketId msgConstructor =
    BucketSaleBindings.buys (Config.bucketSaleAddress testMode) (BigInt.fromInt bucketId) userAddress
        |> Eth.call (httpProvider testMode)
        |> Task.attempt msgConstructor


getTotalExitedTokens : Bool -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getTotalExitedTokens testMode msgConstructor =
    BucketSaleBindings.totalExitedTokens (Config.bucketSaleAddress testMode)
        |> Eth.call (httpProvider testMode)
        |> Task.map TokenValue.tokenValue
        |> Task.attempt msgConstructor


getFryBalance : Bool -> Address -> (Result Http.Error TokenValue -> msg) -> Cmd msg
getFryBalance testMode userAddress msgConstructor =
    Token.balanceOf
        (Config.fryAddress testMode)
        userAddress
        |> Eth.call (httpProvider testMode)
        |> Task.map TokenValue.tokenValue
        |> Task.attempt msgConstructor


type alias ExitInfo =
    { totalExitable : TokenValue
    , exitableBuckets : List Int
    }


getUserExitInfo : Bool -> Address -> (Result Http.Error (Maybe ExitInfo) -> msg) -> Cmd msg
getUserExitInfo testMode userAddress msgConstructor =
    BucketSaleBindings.getExitInfo
        (Config.bucketSaleQueryAddress testMode)
        (Config.bucketSaleAddress testMode)
        userAddress
        |> Eth.call (httpProvider testMode)
        |> Task.map queryBigIntListToMaybExitInfo
        |> Task.attempt msgConstructor


unlockDai : Bool -> Call Bool
unlockDai testMode =
    Token.approve
        (Config.tokenContractAddress <|
            if testMode then
                CommonTypes.KovanDai

            else
                CommonTypes.EthDai
        )
        (Config.bucketSaleAddress testMode)
        EthHelpers.maxUintValue


enter : Address -> Int -> TokenValue -> Maybe Address -> Bool -> Call ()
enter userAddress bucketId amount maybeReferrer testMode =
    BucketSaleBindings.enter
        (Config.bucketSaleAddress testMode)
        userAddress
        (BigInt.fromInt bucketId)
        (TokenValue.getEvmValue amount)
        (maybeReferrer |> Maybe.withDefault EthHelpers.zeroAddress)


exit : Address -> Int -> Bool -> Call ()
exit userAddress bucketId testMode =
    BucketSaleBindings.exit
        (Config.bucketSaleAddress testMode)
        (BigInt.fromInt bucketId)
        userAddress


queryBigIntListToMaybExitInfo : List BigInt -> Maybe ExitInfo
queryBigIntListToMaybExitInfo bigIntList =
    case ( List.head bigIntList, List.tail bigIntList ) of
        ( Just totalBigInt, Just idBigInts ) ->
            let
                totalTokenValue =
                    TokenValue.tokenValue totalBigInt
            in
            if TokenValue.isZero totalTokenValue then
                Just <|
                    ExitInfo
                        TokenValue.zero
                        []

            else
                let
                    exitableBucketIds =
                        {-
                           Because of limitations in Solidity, and to reduce scope, what we now have is a huge array
                           of mostly 0s, with the first N values being the id's of the buckets the user can exit from
                           (where N is the number of such buckets).

                           However, if the user can exit from bucket 0, then the first uint will be 0. Fortunately,
                           we now know that the user can exit from SOME buckets (because we're in this 'else'). Therefore,
                           if the first value is 0 it must mean that the 0th bucket is exitable, not that there are no
                           exitable buckets.

                           Therefore we will read the first value of this list as a bucket id straight, then after that
                           value read until we encounter a zero.
                        -}
                        idBigInts
                            |> List.map BigIntHelpers.toIntWithWarning
                            |> List.Extra.uncons
                            |> Maybe.map
                                (\( firstBucketId, otherIdsFollowedByZeroes ) ->
                                    firstBucketId
                                        :: (otherIdsFollowedByZeroes
                                                |> List.Extra.takeWhile ((/=) 0)
                                           )
                                )
                            |> Maybe.withDefault []
                in
                Just <|
                    ExitInfo
                        totalTokenValue
                        exitableBucketIds

        _ ->
            Nothing
