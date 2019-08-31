module CryptoSwap.Types exposing (Errors, Model, Msg(..), TxChainStatus(..), UpdateResult, exampleAddressForForeignCrypto, justModelUpdate, maybeUserParameters, noErrors)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Dict exposing (Dict)
import Eth.Types exposing (Address, TxHash, TxReceipt)
import Http
import PaymentMethods exposing (PaymentMethod)
import PriceFetch
import Prices exposing (Price)
import Time
import TokenValue exposing (TokenValue)
import Wallet


type alias Model =
    { wallet : Wallet.State
    , initiatorRole : BuyerOrSeller
    , amountInInput : String
    , amountIn : Maybe Float
    , dhToken : FactoryType
    , foreignCrypto : ForeignCrypto
    , marginInput : String
    , margin : Maybe Float
    , amountOutInput : String
    , amountOut : Maybe Float
    , receiveAddress : String
    , showDhTokenDropdown : Bool
    , showForeignCryptoDropdown : Bool
    , errors : Errors
    , txChainStatus : Maybe TxChainStatus
    , depositAmount : Maybe BigInt
    , allowance : Maybe BigInt
    , prices : List ( ForeignCrypto, PriceFetch.PriceData )
    , now : Time.Posix
    }


type TxChainStatus
    = Confirm FactoryType CTypes.CreateParameters String
    | ApproveNeedsSig TokenFactoryType
    | ApproveMining TokenFactoryType CTypes.CreateParameters TxHash
    | CreateNeedsSig FactoryType
    | CreateMining FactoryType TxHash


type Msg
    = UpdateNow Time.Posix
    | Refresh
    | PricesFetched (Result Http.Error (List ( ForeignCrypto, PriceFetch.PriceAndTimestamp )))
    | AmountInChanged String
    | AmountOutChanged String
    | MarginChanged String
    | SwapClicked
    | TokenTypeClicked
    | ChangeTokenType FactoryType
    | ForeignCryptoTypeClicked
    | ChangeForeignCrypto ForeignCrypto
    | ReceiveAddressChanged String
    | PlaceOrderClicked FactoryType UserInfo CTypes.UserParameters
    | AbortCreate
    | ConfirmCreate FactoryType CTypes.CreateParameters BigInt
    | AllowanceFetched TokenFactoryType (Result Http.Error BigInt)
    | ApproveSigned TokenFactoryType CTypes.CreateParameters (Result String TxHash)
    | CreateSigned FactoryType (Result String TxHash)
    | CreateMined FactoryType (Result String TxReceipt)
    | CmdUp (CmdUp Msg)
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []


type alias Errors =
    { amountIn : Maybe String
    , amountOut : Maybe String
    , margin : Maybe String
    }


noErrors : Errors
noErrors =
    Errors Nothing Nothing Nothing


exampleAddressForForeignCrypto : ForeignCrypto -> String
exampleAddressForForeignCrypto crypto =
    case crypto of
        ZEC ->
            "zs... / zc... / t..."

        XMR ->
            "4..."

        BTC ->
            "1... / 3... / bc1..."


maybeUserParameters : Model -> Maybe CTypes.UserParameters
maybeUserParameters model =
    case model.initiatorRole of
        Buyer ->
            Maybe.map3
                (\amountIn margin amountOut ->
                    { initiatorRole = model.initiatorRole
                    , tradeAmount = TokenValue.fromFloatWithWarning amountOut
                    , price = Prices.fromForeignCrypto model.foreignCrypto amountIn
                    , paymentMethods =
                        [ PaymentMethod
                            PaymentMethods.Custom
                            ("Provide your "
                                ++ foreignCryptoName model.foreignCrypto
                                ++ " address immediately upon commitment, via chat."
                            )
                        ]
                    , autorecallInterval = Time.millisToPosix (1000 * 60 * 60 * 24)
                    , autoabortInterval = Time.millisToPosix (1000 * 60 * 60 * 24)
                    , autoreleaseInterval = Time.millisToPosix (1000 * 60 * 60 * 24)
                    }
                )
                model.amountIn
                model.margin
                model.amountOut

        Seller ->
            Maybe.map4
                (\amountIn margin receiveAddress amountOut ->
                    { initiatorRole = model.initiatorRole
                    , tradeAmount =
                        (amountIn - (amountIn / 101))
                            |> TokenValue.fromFloatWithWarning
                    , price = Prices.fromForeignCrypto model.foreignCrypto amountOut
                    , paymentMethods =
                        [ PaymentMethod
                            PaymentMethods.Custom
                            ("Pay to " ++ receiveAddress ++ " immediately upon commitment.")
                        ]
                    , autorecallInterval = Time.millisToPosix (1000 * 60 * 60 * 24)
                    , autoabortInterval = Time.millisToPosix (1000 * 60 * 60 * 1)
                    , autoreleaseInterval = Time.millisToPosix (1000 * 60 * 60 * 24)
                    }
                )
                model.amountIn
                model.margin
                (if model.receiveAddress == "" then
                    Nothing

                 else
                    Just model.receiveAddress
                )
                model.amountOut
