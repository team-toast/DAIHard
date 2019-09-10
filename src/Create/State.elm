module Create.State exposing (init, runCmdDown, subscriptions, update)

import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Wrappers
import Create.Types exposing (..)
import Currencies
import Helpers.Tuple as TupleHelpers
import PriceFetch
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Wallet


init : Wallet.State -> Mode -> UpdateResult
init wallet mode =
    { wallet = wallet
    , mode = mode
    , now = Time.millisToPosix 0
    , prices = []
    , inputs = initialInputs wallet mode
    , errors = noErrors
    , margin = 0
    , dhTokenType =
        Wallet.factory wallet
            |> Maybe.withDefault (Native XDai)
    , dhTokenAmount = Nothing
    , foreignCurrencyType =
        defaultExternalCurrency mode
    , foreignCurrencyAmount = Nothing
    , intervals = defaultIntervals mode
    , showInTypeDropdown = False
    , showOutTypeDropdown = False
    , showMarginModal = False
    , showIntervalModal = Nothing
    , userAllowance = Nothing
    }
        |> update Refresh


initialInputs : Wallet.State -> Mode -> Inputs
initialInputs wallet mode =
    Inputs
        ""
        (Tuple.first (defaultCurrencyInputs wallet mode))
        ""
        (Tuple.second (defaultCurrencyInputs wallet mode))
        ""
        "0"
        Even
        ""
        ""
        ""


defaultCurrencyInputs : Wallet.State -> Mode -> ( CurrencyType, CurrencyType )
defaultCurrencyInputs wallet mode =
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


defaultExternalCurrency : Mode -> Currencies.Symbol
defaultExternalCurrency mode =
    case mode of
        CryptoSwap _ ->
            "ZEC"

        _ ->
            "USD"


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
        UpdateNow time ->
            justModelUpdate
                { prevModel | now = time }

        Refresh ->
            let
                maybeAllowanceCmd =
                    case ( Wallet.userInfo prevModel.wallet, Wallet.factory prevModel.wallet ) of
                        ( Just userInfo, Just (Token tokenType) ) ->
                            Contracts.Wrappers.getAllowanceCmd
                                tokenType
                                userInfo.address
                                (Config.factoryAddress (Token tokenType))
                                (AllowanceFetched tokenType)

                        _ ->
                            Cmd.none

                fetchExchangeRateCmd =
                    PriceFetch.fetch PricesFetched
            in
            UpdateResult
                prevModel
                (Cmd.batch
                    [ maybeAllowanceCmd
                    , fetchExchangeRateCmd
                    ]
                )
                ChainCmd.none
                []

        PricesFetched fetchResult ->
            case fetchResult of
                Ok pricesAndTimestamps ->
                    let
                        newPrices =
                            pricesAndTimestamps
                                |> List.map (Tuple.mapSecond (PriceFetch.checkAgainstTime prevModel.now))

                        ( newModel, appCmds ) =
                            { prevModel | prices = newPrices }
                                |> tryAutofillAmountOut
                    in
                    UpdateResult
                        newModel
                        Cmd.none
                        ChainCmd.none
                        appCmds

                Err httpErr ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice UN.cantFetchPrices ]

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
            let
                newModel =
                    { prevModel
                        | mode = newMode
                    }
            in
            justModelUpdate <|
                if tradeType prevModel.mode /= tradeType newMode then
                    { newModel
                        | inputs = initialInputs prevModel.wallet newMode
                    }
                        |> updateForeignCurrencyType (defaultExternalCurrency newMode)

                else
                    newModel

        SwapClicked ->
            let
                prevInputs =
                    prevModel.inputs

                newInputs =
                    { prevInputs
                        | amountIn = prevInputs.amountOut
                        , amountOut = prevInputs.amountIn
                        , inType = prevInputs.outType
                        , outType = prevInputs.inType
                    }
            in
            case prevModel.mode of
                CryptoSwap prevInitiatorRole ->
                    let
                        ( newModel, appCmds ) =
                            { prevModel
                                | mode =
                                    case prevInitiatorRole of
                                        Buyer ->
                                            CryptoSwap Seller

                                        Seller ->
                                            CryptoSwap Buyer
                                , inputs = newInputs
                            }
                                |> tryAutofillForeignCurrencyAmount
                    in
                    UpdateResult
                        newModel
                        Cmd.none
                        ChainCmd.none
                        appCmds

                _ ->
                    let
                        _ =
                            Debug.log "swap button clicked, but it should be hidden right now!" ""
                    in
                    justModelUpdate
                        prevModel

        AmountInChanged input ->
            let
                prevInputs =
                    prevModel.inputs

                ( newMaybeAmountIn, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretAmount input of
                        Ok maybeAmount ->
                            ( maybeAmount
                            , { prevErrors | amountIn = Nothing }
                            )

                        Err errStr ->
                            ( Nothing
                            , { prevErrors | amountIn = Just errStr }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | inputs = { prevInputs | amountIn = input }
                        , errors = newErrors
                    }
                        |> updateAmountIn newMaybeAmountIn
                        |> tryAutofillAmountOut
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

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
            { prevModel
                | inputs =
                    { oldInputs
                        | inType = newType
                        , currencySearch = ""
                    }
                , showInTypeDropdown = False
            }
                |> updateInType newType
                |> tryAutofillAmountOut
                |> (\( model, cmdUps ) ->
                        UpdateResult
                            model
                            Cmd.none
                            ChainCmd.none
                            cmdUps
                   )

        AmountOutChanged input ->
            let
                prevInputs =
                    prevModel.inputs

                ( newMaybeAmountOut, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretAmount input of
                        Ok maybeAmount ->
                            ( maybeAmount
                            , { prevErrors | amountOut = Nothing }
                            )

                        Err errStr ->
                            ( Nothing
                            , { prevErrors | amountOut = Just errStr }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | inputs = { prevInputs | amountOut = input }
                        , errors = newErrors
                    }
                        |> updateAmountOut newMaybeAmountOut
                        |> tryAutofillAmountIn
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

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
            { prevModel
                | inputs =
                    { oldInputs
                        | outType = newType
                        , currencySearch = ""
                    }
                , showOutTypeDropdown = False
            }
                |> updateOutType newType
                |> tryAutofillAmountOut
                |> (\( model, cmdUps ) ->
                        UpdateResult
                            model
                            Cmd.none
                            ChainCmd.none
                            cmdUps
                   )

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
                prevInputs =
                    prevModel.inputs

                filteredInput =
                    input
                        |> String.filter
                            (\c ->
                                List.any ((==) c) [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.' ]
                            )

                ( newMargin, newMarginType, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretMargin filteredInput of
                        Ok maybeMargin ->
                            ( maybeMargin
                            , if maybeMargin == Just 0 then
                                Even

                              else
                                prevInputs.marginType
                            , { prevErrors | margin = Nothing }
                            )

                        Err errStr ->
                            ( Nothing
                            , prevInputs.marginType
                            , { prevErrors | margin = Just errStr }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | inputs =
                            { prevInputs
                                | margin = filteredInput
                                , marginType = newMarginType
                            }
                        , margin =
                            newMargin
                                |> Maybe.withDefault prevModel.margin
                        , errors = newErrors
                    }
                        |> tryAutofillAmountOut
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        MarginButtonClicked typeClicked ->
            let
                prevInputs =
                    prevModel.inputs

                newMargin =
                    case typeClicked of
                        Loss ->
                            if prevModel.margin == 0 then
                                -0.01

                            else
                                negate (abs prevModel.margin)

                        Even ->
                            0

                        Profit ->
                            if prevModel.margin == 0 then
                                0.01

                            else
                                abs prevModel.margin

                ( newModel, appCmds ) =
                    { prevModel
                        | inputs =
                            { prevInputs
                                | margin = marginToInputString newMargin
                                , marginType = typeClicked
                            }
                        , margin = newMargin
                    }
                        |> tryAutofillAmountOut
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

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

        PaymentMethodChanged input ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | inputs =
                        { oldInputs
                            | paymentMethod = input
                        }
                }

        WindowBoxClicked intervalType ->
            let
                prevInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | showIntervalModal = Just intervalType
                    , inputs =
                        { prevInputs
                            | interval =
                                getUserInterval intervalType prevModel
                                    |> .num
                                    |> String.fromInt
                        }
                }

        IntervalInputChanged input ->
            case prevModel.showIntervalModal of
                Just intervalType ->
                    let
                        prevInputs =
                            prevModel.inputs

                        ( newInterval, newErrors ) =
                            let
                                prevErrors =
                                    prevModel.errors
                            in
                            case interpretInterval input of
                                Ok maybeInt ->
                                    ( maybeInt
                                        |> Maybe.map
                                            (\num ->
                                                UserInterval
                                                    num
                                                    (getUserInterval intervalType prevModel |> .unit)
                                            )
                                        |> Maybe.withDefault (getUserInterval intervalType prevModel)
                                    , { prevErrors | interval = Nothing }
                                    )

                                Err errStr ->
                                    ( getUserInterval intervalType prevModel
                                    , { prevErrors | interval = Just errStr }
                                    )

                        newModel =
                            { prevModel
                                | inputs = { prevInputs | interval = input }
                                , errors = newErrors
                            }
                                |> updateUserInterval intervalType newInterval
                    in
                    UpdateResult
                        newModel
                        Cmd.none
                        ChainCmd.none
                        []

                Nothing ->
                    let
                        _ =
                            Debug.log "Interal input changed, but there is no interval modal open! Wut" ""
                    in
                    justModelUpdate prevModel

        IntervalUnitChanged newUnit ->
            case prevModel.showIntervalModal of
                Just intervalType ->
                    justModelUpdate
                        (prevModel
                            |> updateUserInterval
                                intervalType
                                (UserInterval
                                    (getUserInterval intervalType prevModel |> .num)
                                    newUnit
                                )
                        )

                Nothing ->
                    let
                        _ =
                            Debug.log "Interal unit changed, but there is no interval modal open! Wut" ""
                    in
                    justModelUpdate prevModel

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
                    , showIntervalModal = Nothing
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


interpretAmount : String -> Result String (Maybe Float)
interpretAmount input =
    if input == "" then
        Ok Nothing

    else
        case String.toFloat input of
            Just value ->
                Ok (Just value)

            Nothing ->
                Err "Invalid amount"


interpretMargin : String -> Result String (Maybe Float)
interpretMargin input =
    if input == "" then
        Ok Nothing

    else
        String.toFloat input
            |> Result.fromMaybe "Invalid margin"
            |> Result.map (\percent -> percent / 100.0)
            |> Result.map Just


interpretInterval : String -> Result String (Maybe Int)
interpretInterval input =
    if input == "" then
        Ok Nothing

    else
        String.toInt input
            |> Result.fromMaybe "Must be an integer"
            |> Result.map Just


tryAutofillAmountOut : Model -> ( Model, List (CmdUp Msg) )
tryAutofillAmountOut prevModel =
    case externalCurrencyPrice prevModel of
        Just (PriceFetch.Ok price) ->
            let
                newAmountOut =
                    Maybe.map
                        (\amountIn ->
                            case initiatorRole prevModel.mode of
                                Buyer ->
                                    (amountIn * price) / (1 - prevModel.margin)

                                Seller ->
                                    let
                                        tradeAmountAfterDevFee =
                                            amountIn / 1.01

                                        equivalentForeignCrypto =
                                            tradeAmountAfterDevFee / price
                                    in
                                    equivalentForeignCrypto / (1 - prevModel.margin)
                        )
                        (amountIn prevModel)

                oldInputs =
                    prevModel.inputs
            in
            ( case newAmountOut of
                Just amountOut ->
                    { prevModel
                        | inputs =
                            { oldInputs | amountOut = String.fromFloat amountOut }
                    }
                        |> updateAmountOut (Just amountOut)

                Nothing ->
                    prevModel
            , []
            )

        Just PriceFetch.Outdated ->
            ( prevModel
            , [ CmdUp.UserNotice UN.oldPriceDataWarning ]
            )

        Nothing ->
            ( prevModel
            , []
            )


tryAutofillAmountIn : Model -> ( Model, List (CmdUp Msg) )
tryAutofillAmountIn prevModel =
    case externalCurrencyPrice prevModel of
        Just (PriceFetch.Ok price) ->
            let
                newAmountIn =
                    Maybe.map
                        (\amountOut ->
                            case initiatorRole prevModel.mode of
                                Buyer ->
                                    (amountOut * (1 - prevModel.margin)) / price

                                Seller ->
                                    let
                                        amountOutMinusMargin =
                                            amountOut * (1 - prevModel.margin)

                                        equivalentDai =
                                            amountOutMinusMargin * price
                                    in
                                    equivalentDai * 1.01
                        )
                        (amountOut prevModel)

                oldInputs =
                    prevModel.inputs
            in
            ( case newAmountIn of
                Just amountIn ->
                    { prevModel
                        | inputs =
                            { oldInputs
                                | amountIn = String.fromFloat amountIn
                            }
                    }
                        |> updateAmountIn (Just amountIn)

                Nothing ->
                    prevModel
            , []
            )

        Just PriceFetch.Outdated ->
            ( prevModel
            , [ CmdUp.UserNotice UN.oldPriceDataWarning ]
            )

        Nothing ->
            ( prevModel
            , []
            )


tryAutofillDhTokenAmount : Model -> ( Model, List (CmdUp Msg) )
tryAutofillDhTokenAmount prevModel =
    case initiatorRole prevModel.mode of
        Buyer ->
            tryAutofillAmountOut prevModel

        Seller ->
            tryAutofillAmountIn prevModel


tryAutofillForeignCurrencyAmount : Model -> ( Model, List (CmdUp Msg) )
tryAutofillForeignCurrencyAmount prevModel =
    case initiatorRole prevModel.mode of
        Buyer ->
            tryAutofillAmountIn prevModel

        Seller ->
            tryAutofillAmountOut prevModel


marginToInputString : Float -> String
marginToInputString float =
    float
        * 100
        |> abs
        |> String.fromFloat


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 2000 (always Refresh)
        , Time.every 500 UpdateNow
        ]
