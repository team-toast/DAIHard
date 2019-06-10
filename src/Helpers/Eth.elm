module Helpers.Eth exposing (EthNode, addressIfNot0x0, ethNode, getLogAt, intToNetwork, logBadFetchResultMaybe, makeViewAddressUrl, makeViewTxUrl, updateCallValue)

import Array
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Eth.Net as Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Eth.Utils


type alias EthNode =
    { network : Network
    , http : HttpProvider
    , ws : WebsocketProvider
    }


intToNetwork : Int -> Maybe Network
intToNetwork i =
    case Net.toNetworkId i of
        Net.Mainnet ->
            Just Mainnet

        Net.Kovan ->
            Just Kovan

        _ ->
            if i == 100 then
                Just XDAI

            else
                Nothing


ethNode : Network -> EthNode
ethNode network =
    case network of
        Mainnet ->
            EthNode
                network
                "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://mainnet.infura.io/ws"

        Kovan ->
            EthNode
                network
                "https://kovan.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://kovan.infura.io/ws"

        XDAI ->
            EthNode
                network
                "https://dai.poa.network"
                ""


addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
    if addr == Eth.Utils.unsafeToAddress "0x0000000000000000000000000000000000000000" then
        Nothing

    else
        Just addr


getLogAt : Int -> List Eth.Types.Log -> Maybe Eth.Types.Log
getLogAt index logList =
    Array.fromList logList
        |> Array.get index


logBadFetchResultMaybe : Result a (Maybe b) -> Result a (Maybe b)
logBadFetchResultMaybe fetchResult =
    case fetchResult of
        Ok (Just a) ->
            Debug.log "I'm confused about whether this is a bad fetch result or not!." fetchResult

        Ok Nothing ->
            Debug.log "The data was fetched, but could not be decoded." fetchResult

        Err _ ->
            Debug.log "can't fetch from Ethereum: " fetchResult


makeViewTxUrl : Network -> TxHash -> String
makeViewTxUrl network txHash =
    case network of
        Mainnet ->
            "https://etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Kovan ->
            "https://kovan.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        XDAI ->
            "https://blockscout.com/poa/dai/tx/" ++ Eth.Utils.txHashToString txHash


makeViewAddressUrl : Network -> Address -> String
makeViewAddressUrl network address =
    case network of
        Mainnet ->
            "https://etherscan.io/address/" ++ Eth.Utils.addressToString address

        Kovan ->
            "https://kovan.etherscan.io/address/" ++ Eth.Utils.addressToString address

        XDAI ->
            "https://blockscout.com/poa/dai/address/" ++ Eth.Utils.addressToString address


updateCallValue : BigInt -> Eth.Types.Call a -> Eth.Types.Call a
updateCallValue value call =
    { call
        | value = Just value
    }
