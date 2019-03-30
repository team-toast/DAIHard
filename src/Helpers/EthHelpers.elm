module EthHelpers exposing (EthNode, addressIfNot0x0, ethNode, getLogAt, logBadFetchResultMaybe, makeEtherscanTxUrl)

import Array
import BigInt exposing (BigInt)
import Constants exposing (..)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Eth.Utils


type alias EthNode =
    { http : HttpProvider
    , ws : WebsocketProvider
    }


ethNode : NetworkId -> EthNode
ethNode networkId =
    case networkId of
        Mainnet ->
            EthNode "https://mainnet.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9" "wss://mainnet.infura.io/ws"

        Ropsten ->
            EthNode "https://ropsten.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9" "wss://ropsten.infura.io/ws"

        Kovan ->
            EthNode "https://kovan.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9" "wss://kovan.infura.io/ws"

        Rinkeby ->
            EthNode "https://rinkeby.infura.io/v3/e3eef0e2435349bf9164e6f465bd7cf9" "wss://rinkeby.infura.io/ws"

        _ ->
            EthNode "UnknownEthNetwork" "UnknownEthNetwork"


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


makeEtherscanTxUrl : NetworkId -> TxHash -> String
makeEtherscanTxUrl networkId txHash =
    case networkId of
        Mainnet ->
            "https://etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Ropsten ->
            "https://ropsten.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Kovan ->
            "https://kovan.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        Rinkeby ->
            "https://rinkeby.etherscan.io/tx/" ++ Eth.Utils.txHashToString txHash

        _ ->
            "Don't recognize that networkId..."
