module EthHelpers exposing (EthNode, addressIfNot0x0, bigIntToTime, ethNode)

-- Library

import BigInt exposing (BigInt)
import Eth.Net as Net exposing (NetworkId(..))
import Eth.Types exposing (..)
import Eth.Utils as EthUtils
import Time exposing (Time)



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


bigIntToTime : BigInt -> Result String Time
bigIntToTime time =
    BigInt.toString time
        |> String.toInt
        |> Result.map (\t -> toFloat t * Time.second)


addressIfNot0x0 : Address -> Maybe Address
addressIfNot0x0 addr =
    if addr == EthUtils.unsafeToAddress "0x0000000000000000000000000000000000000000" then
        Nothing

    else
        Just addr
