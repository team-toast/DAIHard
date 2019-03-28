module EthHelpers exposing (EthNode, addressIfNot0x0, ethNode, getLogAt, logBadFetchResultMaybe)

import Array
import BigInt exposing (BigInt)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, WebsocketProvider)
import Eth.Utils


type alias EthNode =
    { http : HttpProvider
    , ws : WebsocketProvider
    }


ethNode : NetworkId -> EthNode
ethNode networkId =
    case networkId of
        Mainnet ->
            EthNode "https://mainnet.infura.io/e3eef0e2435349bf9164e6f465bd7cf9" "wss://mainnet.infura.io/ws"

        Ropsten ->
            EthNode "https://ropsten.infura.io/e3eef0e2435349bf9164e6f465bd7cf9" "wss://ropsten.infura.io/ws"

        Kovan ->
            EthNode "https://kovan.infura.io/e3eef0e2435349bf9164e6f465bd7cf9" "wss://kovan.infura.io/ws"

        Rinkeby ->
            EthNode "https://rinkeby.infura.io/e3eef0e2435349bf9164e6f465bd7cf9" "wss://rinkeby.infura.io/ws"

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
