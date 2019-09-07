module Create.State exposing (init, runCmdDown, subscriptions, update)

import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Wrappers
import Create.Types exposing (..)
import Helpers.Tuple as TupleHelpers
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
        , showMarginModal = False
        , showIntervalModals = ( False, False, False )
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
        "0"
        ""
        (defaultIntervals mode)


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


defaultIntervals : Mode -> ( UserInterval, UserInterval, UserInterval )
defaultIntervals mode =
    case mode of
        CryptoSwap Seller ->
            ( UserInterval 24 Hour
            , UserInterval 1 Hour
            , UserInterval 24 Hour
            )

        CryptoSwap Buyer ->
            ( UserInterval 24 Hour
            , UserInterval 24 Hour
            , UserInterval 24 Hour
            )

        OffRamp ->
            ( UserInterval 3 Day
            , UserInterval 3 Day
            , UserInterval 3 Day
            )

        OnRamp ->
            ( UserInterval 3 Day
            , UserInterval 3 Day
            , UserInterval 3 Day
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

        ChangeMode newMode ->
            justModelUpdate
                { prevModel | mode = newMode }

        SwapClicked ->
            Debug.todo ""

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

        MarginBoxClicked ->
            justModelUpdate
                { prevModel
                    | showMarginModal =
                        if prevModel.showMarginModal then
                            False

                        else
                            True
                }

        MarginInputChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | margin = input
                        }
                }

        MarginLossClicked ->
            Debug.todo ""

        MarginEvenClicked ->
            Debug.todo ""

        MarginProfitClicked ->
            Debug.todo ""

        ReceiveAddressChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | receiveAddress = input
                        }
                }

        ExpiryWindowBoxClicked ->
            justModelUpdate
                { prevModel
                    | showIntervalModals =
                        ( if TupleHelpers.tuple3First prevModel.showIntervalModals then
                            False

                          else
                            True
                        , False
                        , False
                        )
                }

        PaymentWindowBoxClicked ->
            justModelUpdate
                { prevModel
                    | showIntervalModals =
                        ( False
                        , if TupleHelpers.tuple3Second prevModel.showIntervalModals then
                            False

                          else
                            True
                        , False
                        )
                }

        BurnWindowBoxClicked ->
            justModelUpdate
                { prevModel
                    | showIntervalModals =
                        ( False
                        , False
                        , if TupleHelpers.tuple3Third prevModel.showIntervalModals then
                            False

                          else
                            True
                        )
                }

        CloseModals ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | showInTypeDropdown = False
                    , showOutTypeDropdown = False
                    , showMarginModal = False
                    , showIntervalModals = ( False, False, False )
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
