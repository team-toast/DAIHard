module Create.State exposing (init, runCmdDown, subscriptions, update)

import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Wrappers
import Create.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Wallet


init : Wallet.State -> Mode -> UpdateResult
init wallet mode =
    UpdateResult
        { wallet = wallet
        , mode = mode
        , inputs = initialInputs wallet mode
        , errors = noErrors
        , margin = 0
        , showInTypeDropdown = False
        , showOutTypeDropdown = False
        , userAllowance = Nothing
        }
        Cmd.none
        ChainCmd.none
        []


initialInputs : Wallet.State -> Mode -> Inputs
initialInputs wallet mode =
    Inputs
        ""
        (Tuple.first (defaultCurrencyTypes wallet mode))
        ""
        (Tuple.second (defaultCurrencyTypes wallet mode))
        ""


defaultCurrencyTypes : Wallet.State -> Mode -> ( CurrencyType, CurrencyType )
defaultCurrencyTypes wallet mode =
    let
        defaultDhToken =
            DHToken <|
                (Wallet.factory wallet
                    |> Maybe.withDefault (Native XDai)
                )

        defaultCrypto =
            External "ZEC"

        defaultFiat =
            External "USD"
    in
    ( if mode == CryptoSwap Seller || mode == OffRamp then
        defaultDhToken

      else if mode == CryptoSwap Buyer then
        defaultCrypto

      else
        defaultFiat
    , if mode == CryptoSwap Buyer || mode == OnRamp then
        defaultDhToken

      else if mode == CryptoSwap Seller then
        defaultCrypto

      else
        defaultFiat
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh ->
            case ( Wallet.userInfo prevModel.wallet, Wallet.factory prevModel.wallet ) of
                ( Just userInfo, Just (Token tokenType) ) ->
                    let
                        cmd =
                            Contracts.Wrappers.getAllowanceCmd
                                tokenType
                                userInfo.address
                                (Config.factoryAddress (Token tokenType))
                                (AllowanceFetched tokenType)
                    in
                    UpdateResult
                        prevModel
                        cmd
                        ChainCmd.none
                        []

                _ ->
                    justModelUpdate prevModel

        AllowanceFetched tokenType fetchResult ->
            case fetchResult of
                Ok allowance ->
                    let
                        newModel =
                            { prevModel
                                | userAllowance = Just (TokenValue.tokenValue allowance)
                            }
                    in
                    -- case ( newModel.txChainStatus, newModel.depositAmount ) of
                    --     ( Just (ApproveMining _ createParameters _), Just depositAmount ) ->
                    --         if BigInt.compare allowance depositAmount /= LT then
                    --             let
                    --                 ( txChainStatus, chainCmd ) =
                    --                     initiateCreateCall (Token tokenType) createParameters
                    --             in
                    --             UpdateResult
                    --                 { newModel | txChainStatus = txChainStatus }
                    --                 Cmd.none
                    --                 chainCmd
                    --                 []
                    --         else
                    --             justModelUpdate newModel
                    --     _ ->
                    justModelUpdate newModel

                Err httpError ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <| UN.web3FetchError "allowance" httpError ]

        SearchInputChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | currencySearch = input
                        }
                }

        AmountInChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | amountIn = input
                        }
                }

        InTypeClicked ->
            justModelUpdate
                { prevModel
                    | showInTypeDropdown =
                        if prevModel.showInTypeDropdown then
                            False

                        else
                            True
                }

        InTypeSelected newType ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | inType = newType
                            , currencySearch = ""
                        }
                    , showInTypeDropdown = False
                }

        AmountOutChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | amountOut = input
                        }
                }

        OutTypeClicked ->
            justModelUpdate
                { prevModel
                    | showOutTypeDropdown =
                        if prevModel.showOutTypeDropdown then
                            False

                        else
                            True
                }

        OutTypeSelected newType ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | outType = newType
                            , currencySearch = ""
                        }
                    , showOutTypeDropdown = False
                }

        SwapClicked ->
            Debug.todo ""

        CloseModals ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | showInTypeDropdown = False
                    , showOutTypeDropdown = False
                    , inputs =
                        { oldInputs | currencySearch = "" }
                }

        NoOp ->
            justModelUpdate prevModel

        CmdUp cmdUp ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ cmdUp ]


runCmdDown : CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            UpdateResult
                { prevModel | wallet = wallet }
                (case ( Wallet.userInfo wallet, Wallet.factory wallet ) of
                    ( Just uInfo, Just (Token tokenType) ) ->
                        Contracts.Wrappers.getAllowanceCmd
                            tokenType
                            uInfo.address
                            (Config.factoryAddress (Token tokenType))
                            (AllowanceFetched tokenType)

                    _ ->
                        Cmd.none
                )
                ChainCmd.none
                []

        CmdDown.CloseAnyDropdownsOrModals ->
            prevModel
                |> update CloseModals


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 (always Refresh)
