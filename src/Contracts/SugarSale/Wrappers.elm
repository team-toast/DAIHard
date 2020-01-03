module Contracts.SugarSale.Wrappers exposing (enter, exit, getSaleStartTimestampCmd, getTotalValueEnteredForBucket, getUserBuyForBucket, httpProvider, unlockDai)

import BigInt exposing (BigInt)
import CommonTypes
import Config
import Contracts.Generated.ERC20Token as Token
import Contracts.SugarSale.Generated.BucketSale as SugarSaleBindings
import Eth
import Eth.Types exposing (Address, Call, HttpProvider)
import Helpers.Eth as EthHelpers
import Http
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
    SugarSaleBindings.startOfSale (Config.sugarSaleAddress testMode)
        |> Eth.call (httpProvider testMode)
        |> Task.attempt msgConstructor


getTotalValueEnteredForBucket : Bool -> Int -> (Result Http.Error BigInt -> msg) -> Cmd msg
getTotalValueEnteredForBucket testMode bucketId msgConstructor =
    SugarSaleBindings.buckets (Config.sugarSaleAddress testMode) (BigInt.fromInt bucketId)
        |> Eth.call (httpProvider testMode)
        |> Task.attempt msgConstructor


getUserBuyForBucket : Bool -> Address -> Int -> (Result Http.Error SugarSaleBindings.Buy -> msg) -> Cmd msg
getUserBuyForBucket testMode userAddress bucketId msgConstructor =
    SugarSaleBindings.buys (Config.sugarSaleAddress testMode) (BigInt.fromInt bucketId) userAddress
        |> Eth.call (httpProvider testMode)
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
        (Config.sugarSaleAddress testMode)
        EthHelpers.maxUintValue


enter : Address -> TokenValue -> Maybe Address -> Bool -> Call ()
enter userAddress amount maybeReferrer testMode =
    SugarSaleBindings.enter
        (Config.sugarSaleAddress testMode)
        userAddress
        (TokenValue.getEvmValue amount)
        (maybeReferrer |> Maybe.withDefault EthHelpers.zeroAddress)


exit : Address -> Int -> Bool -> Call ()
exit userAddress bucketId testMode =
    SugarSaleBindings.exit
        (Config.sugarSaleAddress testMode)
        userAddress
        (BigInt.fromInt bucketId)
