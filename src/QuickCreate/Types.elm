module QuickCreate.Types exposing (MenuState(..), Model, Msg(..), SpecState(..), State(..), TradeRecipe, UpdateResult, constructCreateParameters, justModelUpdate, menuItems, recipeBuyerDeposit, recipeTradeAmount)

import AppCmd exposing (AppCmd)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Eth.Types exposing (Address, TxHash, TxReceipt)
import FiatValue exposing (FiatValue)
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Http
import PaymentMethods exposing (PaymentMethod)
import Routing
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { web3Context : Web3Context
    , userInfo : Maybe UserInfo
    , state : State
    , tokenAllowance : Maybe TokenValue
    , textInput : String
    }


type State
    = Menu MenuState
    | Spec TradeRecipe SpecState


type MenuState
    = NoneStarted
    | StartPrompt TradeRecipe
    | ApproveNeedsSig TradeRecipe


type SpecState
    = ApproveMining TxHash
    | ReadyToOpen
    | OpenNeedsSig
    | OpenMining


type Msg
    = Refresh Time.Posix
    | StartClicked TradeRecipe
    | ApproveClicked TradeRecipe
    | AllowanceFetched (Result Http.Error BigInt)
    | OpenClicked UserInfo TradeRecipe
    | ApproveSigned (Result String TxHash)
    | OpenSigned (Result String TxHash)
    | OpenMined (Result String TxReceipt)
    | TextInputChanged String
    | ChangeState State
    | AbortCreate
    | NoOp
    | Web3Connect


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , appCmds : List (AppCmd Msg)
    }


type alias TradeRecipe =
    { initiatorRole : BuyerOrSeller
    , daiAmountIn : TokenValue
    , fiatValue : FiatValue
    , intervals : ( Time.Posix, Time.Posix, Time.Posix )
    }


constructCreateParameters : UserInfo -> TradeRecipe -> String -> CTypes.CreateParameters
constructCreateParameters userInfo recipe paymentText =
    let
        ( autorecallInterval, autoabortInterval, autoreleaseInterval ) =
            recipe.intervals
    in
    { initiatorRole = recipe.initiatorRole
    , tradeAmount = recipeTradeAmount recipe
    , price = recipe.fiatValue
    , paymentMethods =
        [ PaymentMethod
            PaymentMethods.Custom
            paymentText
        ]
    , autorecallInterval = autorecallInterval
    , autoabortInterval = autoabortInterval
    , autoreleaseInterval = autoreleaseInterval
    }
        |> CTypes.buildCreateParameters userInfo


recipeTradeAmount : TradeRecipe -> TokenValue
recipeTradeAmount recipe =
    let
        tradeAmountToInitialDepositRatio =
            case recipe.initiatorRole of
                Buyer ->
                    3

                Seller ->
                    1
    in
    TokenValue.div
        (TokenValue.mul recipe.daiAmountIn (tradeAmountToInitialDepositRatio * 100))
        (100 + tradeAmountToInitialDepositRatio)


recipeBuyerDeposit : TradeRecipe -> TokenValue
recipeBuyerDeposit recipe =
    TokenValue.div
        (recipeTradeAmount recipe)
        3


menuItems : List TradeRecipe
menuItems =
    [ TradeRecipe
        Seller
        (TokenValue.fromIntTokenValue 5)
        (FiatValue.usd 4)
        ( Time.millisToPosix <| 1000 * 60 * 60 * 24 * 2
        , Time.millisToPosix <| 1000 * 60 * 60 * 24 * 2
        , Time.millisToPosix <| 1000 * 60 * 60 * 24 * 3
        )
    , TradeRecipe
        Buyer
        (TokenValue.fromIntTokenValue 2)
        (FiatValue.usd 7)
        ( Time.millisToPosix <| 1000 * 60 * 60 * 24 * 2
        , Time.millisToPosix <| 1000 * 60 * 60 * 24 * 2
        , Time.millisToPosix <| 1000 * 60 * 60 * 24 * 3
        )
    ]


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    { model = model
    , cmd = Cmd.none
    , chainCmd = ChainCmd.none
    , appCmds = []
    }
