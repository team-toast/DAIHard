module EthHelpers exposing (EthNode, addressIfNot0x0, bigIntToTime, ethNode, getLogAt, timeToBigInt)

-- Library

import Array
import BigInt exposing (BigInt)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Sentry.Tx as TxSentry
import Eth.Types exposing (Address, HttpProvider, WebsocketProvider)
import Eth.Utils as EthUtils
import Time



-- Exposed


type alias EthNode =
    { http : HttpProvider
    , ws : WebsocketProvider
    }


ethNode : NetworkId -> EthNode
ethNode networkId =
    case networkId of
        Mainnet ->
            EthNode "https://mainnet.infura.io/" "wss://mainnet.infura.io/ws"

        Ropsten ->
            EthNode "https://ropsten.infura.io/" "wss://ropsten.infura.io/ws"

        Kovan ->
            EthNode "https://kovan.infura.io/" "wss://kovan.infura.io/ws"

        Rinkeby ->
            EthNode "https://rinkeby.infura.io/" "wss://rinkeby.infura.io/ws"

        _ ->
            EthNode "UnknownEthNetwork" "UnknownEthNetwork"


bigIntToTime : BigInt -> Maybe Time.Posix
bigIntToTime bigint =
    BigInt.toString bigint
        |> String.toInt
        |> Maybe.map (\t -> Time.millisToPosix (t * 1000))


timeToBigInt : Time.Posix -> BigInt
timeToBigInt t =
    Time.posixToMillis t
        |> BigInt.fromInt


addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
    if addr == EthUtils.unsafeToAddress "0x0000000000000000000000000000000000000000" then
        Nothing

    else
        Just addr


getLogAt : Int -> List Eth.Types.Log -> Maybe Eth.Types.Log
getLogAt index logList =
    Array.fromList logList
        |> Array.get index
