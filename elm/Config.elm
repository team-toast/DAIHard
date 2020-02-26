module Config exposing (activeFactories, devFeeAddress, factoryAddress, tokenContractAddress)

import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils
import Time
import TokenValue exposing (TokenValue)


activeFactories : Bool -> List FactoryType
activeFactories testMode =
    if testMode then
        [ Token KovanDai ]

    else
        [ Token EthDai
        , Native XDai
        ]


tokenContractAddress : TokenFactoryType -> Address
tokenContractAddress tokenFactoryType =
    case tokenFactoryType of
        EthDai ->
            Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"

        KovanDai ->
            Eth.Utils.unsafeToAddress "0x4F96Fe3b7A6Cf9725f59d353F723c1bDb64CA6Aa"


factoryAddress : FactoryType -> Address
factoryAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x484c3A51a794D37939cf76CD176559c8e1614D74"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0x40d2bA57cbcE5743F2325F654E48458fea0D69F6"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x7B18F19373Db2Db5A91D3687BEa6d200081e2B0b"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0xa7643Ef0546B23c122dcc3b992478F95dc65Eef6"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0xe05457c4C12eD97248bCC56A2bA1705C3B1610C7"


devFeeAddress : FactoryType -> Address
devFeeAddress factoryType =
    case factoryType of
        Token EthDai ->
            Eth.Utils.unsafeToAddress "0x917270988E3C41317710101cD28844505dC5E9eE"

        Token KovanDai ->
            Eth.Utils.unsafeToAddress "0x11B68A6E1bB525B4B64aa81c5f219834Aa849117"

        Native Eth ->
            Eth.Utils.unsafeToAddress "0x917270988E3C41317710101cD28844505dC5E9eE"

        Native Kovan ->
            Eth.Utils.unsafeToAddress "0x11B68A6E1bB525B4B64aa81c5f219834Aa849117"

        Native XDai ->
            Eth.Utils.unsafeToAddress "0x487Ac5423555B1D83F5b8BA13F260B296E9D0777"
