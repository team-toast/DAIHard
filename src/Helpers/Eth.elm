module Helpers.Eth exposing (Web3Context, addressIfNot0x0, factoryTypeToNetworkId, getLogAt, intToFactoryType, makeViewAddressUrl, makeViewTxUrl, networkIdToFactoryType, updateCallValue, web3Context)

import Array
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Eth.Net as Net
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Eth.Utils


type alias Web3Context =
    { factoryType : FactoryType
    , httpProvider : HttpProvider
    , wsProvider : WebsocketProvider
    }


networkIdToFactoryType : Net.NetworkId -> Maybe FactoryType
networkIdToFactoryType networkId =
    case networkId of
        Net.Mainnet ->
            Just <| Token EthDai

        Net.Kovan ->
            Just <| Token KovanDai

        Net.RskMain ->
            Just <| Native Rootstock

        Net.RskTest ->
            Just <| Native RootstockTest

        Net.Private 100 ->
            Just <| Native XDai

        _ ->
            Nothing


factoryTypeToNetworkId : FactoryType -> Net.NetworkId
factoryTypeToNetworkId factoryType =
    case factoryType of
        Token EthDai ->
            Net.Mainnet

        Native Eth ->
            Net.Mainnet

        Token KovanDai ->
            Net.Kovan

        Native Kovan ->
            Net.Kovan

        Native Rootstock ->
            Net.RskMain

        Native RootstockTest ->
            Net.RskTest

        Native XDai ->
            Net.Private 100


intToFactoryType : Int -> Maybe FactoryType
intToFactoryType =
    Net.toNetworkId >> networkIdToFactoryType


web3Context : FactoryType -> Web3Context
web3Context factoryType =
    case factoryType of
        Token EthDai ->
            Web3Context
                factoryType
                "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://mainnet.infura.io/ws"

        Native Eth ->
            Web3Context
                factoryType
                "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://mainnet.infura.io/ws"

        Token KovanDai ->
            Web3Context
                factoryType
                "https://kovan.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://kovan.infura.io/ws"

        Native Kovan ->
            Web3Context
                factoryType
                "https://kovan.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9"
                "wss://kovan.infura.io/ws"

        Native XDai ->
            Web3Context
                factoryType
                "https://dai.poa.network"
                ""

        Native Rootstock ->
            Web3Context
                factoryType
                "https://public-node.rsk.co"
                ""

        Native RootstockTest ->
            Web3Context
                factoryType
                "https://public-node.testnet.rsk.co"
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


makeViewTxUrl : FactoryType -> TxHash -> String
makeViewTxUrl factoryType txHash =
    case factoryType of
        Token EthDai ->
            "https://etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Native Eth ->
            "https://etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Token KovanDai ->
            "https://kovan.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Native Kovan ->
            "https://kovan.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Native XDai ->
            "https://blockscout.com/poa/dai/tx/" ++ Eth.Utils.txHashToString txHash

        Native Rootstock ->
            "https://explorer.rsk.co/tx/" ++ Eth.Utils.txHashToString txHash

        Native RootstockTest ->
            ""


makeViewAddressUrl : FactoryType -> Address -> String
makeViewAddressUrl factoryType address =
    case factoryType of
        Token EthDai ->
            "https://etherscan.io/address/" ++ Eth.Utils.addressToString address

        Native Eth ->
            "https://etherscan.io/address/" ++ Eth.Utils.addressToString address

        Token KovanDai ->
            "https://kovan.etherscan.io/address/" ++ Eth.Utils.addressToString address

        Native Kovan ->
            "https://kovan.etherscan.io/address/" ++ Eth.Utils.addressToString address

        Native XDai ->
            "https://blockscout.com/poa/dai/address/" ++ Eth.Utils.addressToString address

        Native Rootstock ->
            "https://explorer.rsk.co/address/" ++ Eth.Utils.addressToString address

        Native RootstockTest ->
            ""


updateCallValue : BigInt -> Eth.Types.Call a -> Eth.Types.Call a
updateCallValue value call =
    { call
        | value = Just value
    }
