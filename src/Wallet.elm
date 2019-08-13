module Wallet exposing (State(..), defaultFactory, factory, factoryForNetwork, factoryWithDefault, httpProvider, httpProviderWithDefault, network, networkForFactory, userInfo)

import CommonTypes exposing (..)
import Eth.Net
import Eth.Types exposing (Address, HttpProvider, TxHash, WebsocketProvider)
import Helpers.Eth as EthHelpers


type State
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


userInfo : State -> Maybe UserInfo
userInfo walletState =
    case walletState of
        Active uInfo ->
            Just uInfo

        _ ->
            Nothing


factory : State -> Maybe FactoryType
factory walletState =
    network walletState
        |> Maybe.andThen factoryForNetwork


factoryWithDefault : State -> FactoryType
factoryWithDefault walletState =
    factory walletState
        |> Maybe.withDefault defaultFactory


httpProvider : State -> Maybe HttpProvider
httpProvider walletState =
    factory walletState
        |> Maybe.map EthHelpers.httpProviderForFactory


httpProviderWithDefault : State -> HttpProvider
httpProviderWithDefault =
    factoryWithDefault >> EthHelpers.httpProviderForFactory


network : State -> Maybe Eth.Net.NetworkId
network walletState =
    case walletState of
        NoneDetected ->
            Nothing

        OnlyNetwork network_ ->
            Just network_

        Active uInfo ->
            Just uInfo.network


factoryForNetwork : Eth.Net.NetworkId -> Maybe FactoryType
factoryForNetwork networkId =
    case networkId of
        Eth.Net.Mainnet ->
            Just <| Token EthDai

        Eth.Net.Kovan ->
            Just <| Token KovanDai

        Eth.Net.Private 100 ->
            Just <| Native XDai

        _ ->
            Nothing


networkForFactory : FactoryType -> Eth.Net.NetworkId
networkForFactory factory_ =
    case factory_ of
        Token EthDai ->
            Eth.Net.Mainnet

        Native Eth ->
            Eth.Net.Mainnet

        Token KovanDai ->
            Eth.Net.Kovan

        Native Kovan ->
            Eth.Net.Kovan

        Native XDai ->
            Eth.Net.Private 100


defaultFactory : FactoryType
defaultFactory =
    Token EthDai
