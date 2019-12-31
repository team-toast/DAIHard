module Contracts.SugarSale.Wrappers exposing (getSaleStartTimestampCmd, getTotalValueEnteredForBucket)

import BigInt exposing (BigInt)
import Config
import Contracts.SugarSale.Generated.BucketSale as BucketSale
import Eth
import Eth.Types exposing (Address, HttpProvider)
import Http
import Task


getSaleStartTimestampCmd : HttpProvider -> (Result Http.Error BigInt -> msg) -> Cmd msg
getSaleStartTimestampCmd httpProvider msgConstructor =
    BucketSale.startOfSale Config.testSugarSaleAddress
        |> Eth.call httpProvider
        |> Task.attempt msgConstructor


getTotalValueEnteredForBucket : HttpProvider -> Int -> (Result Http.Error BigInt -> msg) -> Cmd msg
getTotalValueEnteredForBucket httpProvider bucketId msgConstructor =
    BucketSale.buckets Config.testSugarSaleAddress (BigInt.fromInt bucketId)
        |> Eth.call httpProvider
        |> Task.attempt msgConstructor
