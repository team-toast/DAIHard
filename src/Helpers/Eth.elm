module Helpers.Eth exposing (EthNode, addressIfNot0x0, ethNode, getLogAt, intToNetwork, logBadFetchResultMaybe, makeViewAddressUrl, makeViewTxUrl, networkIdToNetwork, updateCallValue)

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


networkIdToNetwork : Net.NetworkId -> Maybe Network
networkIdToNetwork networkId =
    case networkId of
        Net.Mainnet ->
            Just <| Eth Mainnet

        Net.Kovan ->
            Just <| Eth Kovan

        Net.RskMain ->
            Just Rootstock

        Net.RskTest ->
            Just RootstockTest

        Net.Private 100 ->
            Just XDai

        _ ->
            let
                _ =
                    Debug.log "unknown network" networkId
            in
            Nothing


intToNetwork : Int -> Maybe Network
intToNetwork =
    Net.toNetworkId >> networkIdToNetwork


ethNode : Network -> EthNode
ethNode network =
    case network of
        Eth Mainnet ->
            EthNode
                network
                "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://mainnet.infura.io/ws"

        Eth Kovan ->
            EthNode
                network
                "https://kovan.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://kovan.infura.io/ws"

        XDai ->
            EthNode
                network
                "https://dai.poa.network"
                ""

        Rootstock ->
            EthNode
                network
                "https://public-node.rsk.co/"
                ""

        RootstockTest ->
            EthNode
                network
                "https://public-node.testnet.rsk.co/"
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
        Eth Mainnet ->
            "https://etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Eth Kovan ->
            "https://kovan.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        XDai ->
            "https://blockscout.com/poa/dai/tx/" ++ Eth.Utils.txHashToString txHash

        Rootstock ->
            "https://explorer.rsk.co/tx/" ++ Eth.Utils.txHashToString txHash

        RootstockTest ->
            ""


makeViewAddressUrl : Network -> Address -> String
makeViewAddressUrl network address =
    case network of
        Eth Mainnet ->
            "https://etherscan.io/address/" ++ Eth.Utils.addressToString address

        Eth Kovan ->
            "https://kovan.etherscan.io/address/" ++ Eth.Utils.addressToString address

        XDai ->
            "https://blockscout.com/poa/dai/address/" ++ Eth.Utils.addressToString address

        Rootstock ->
            "https://explorer.rsk.co/address/" ++ Eth.Utils.addressToString address

        RootstockTest ->
            ""


updateCallValue : BigInt -> Eth.Types.Call a -> Eth.Types.Call a
updateCallValue value call =
    { call
        | value = Just value
    }
