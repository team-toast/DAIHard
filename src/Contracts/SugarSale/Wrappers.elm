module Contracts.SugarSale.Wrappers exposing (getSaleStartBlockCmd)

import BigInt exposing (BigInt)
import Config
import Contracts.SugarSale.Generated.BucketSale as BucketSale
import Eth
import Eth.Types exposing (Address, HttpProvider)
import Http
import Task


getSaleStartBlockCmd : HttpProvider -> (Result Http.Error BigInt -> msg) -> Cmd msg
getSaleStartBlockCmd httpProvider msgConstructor =
    BucketSale.saleStartBlock Config.testSugarSaleAddress
        |> Eth.call httpProvider
        |> Task.attempt msgConstructor
