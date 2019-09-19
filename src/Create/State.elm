module Create.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown exposing (CmdDown)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Create.Types exposing (..)
import Currencies
import Eth
import Helpers.BigInt as BigIntHelpers
import Helpers.Tuple as TupleHelpers
import PriceFetch
import Routing
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Utils
import Wallet


init : Wallet.State -> Mode -> UpdateResult
init wallet mode =
    { wallet = wallet
    , mode = mode
    , now = Time.millisToPosix 0
    , prices = []
    , lastAmountInputChanged = AmountIn
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
    , depositAmount = Nothing
    , txChainStatus = Nothing
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
                                |> (case prevModel.lastAmountInputChanged of
                                        AmountIn ->
                                            tryAutofillAmountOut

                                        AmountOut ->
                                            tryAutofillAmountIn
                                   )
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

        ChangeMode newMode ->
            let
                newModel =
                    { prevModel
                        | mode = newMode
                    }
            in
            UpdateResult
                (if prevModel.mode /= newMode then
                    { newModel
                        | inputs = initialInputs prevModel.wallet newMode
                        , intervals = defaultIntervals newMode
                    }
                        |> updateCurrencyTypesFromInput

                 else
                    newModel
                )
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "change mode" "navigation" (modeToString newMode) 0 ]

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
                                , lastAmountInputChanged =
                                    case prevModel.lastAmountInputChanged of
                                        AmountIn ->
                                            AmountOut

                                        AmountOut ->
                                            AmountIn
                            }
                                |> (case prevModel.lastAmountInputChanged of
                                        AmountIn ->
                                            tryAutofillAmountOut

                                        AmountOut ->
                                            tryAutofillAmountIn
                                   )
                    in
                    UpdateResult
                        newModel
                        Cmd.none
                        ChainCmd.none
                        (appCmds
                            ++ [ CmdUp.gTag "swap clicked" "navigation" (buyerOrSellerToString (initiatorRole newModel.mode)) 0 ]
                        )

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

                filteredInput =
                    input |> Utils.filterPositiveNumericInput

                ( newMaybeAmountIn, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretAmount filteredInput of
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
                        | inputs = { prevInputs | amountIn = filteredInput }
                        , errors = newErrors
                        , lastAmountInputChanged = AmountIn
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
            UpdateResult
                { prevModel
                    | showInTypeDropdown =
                        if prevModel.showInTypeDropdown then
                            False

                        else
                            True
                }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "in currency dropdown clicked" "input" "" 0 ]

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
                |> (\updateResult ->
                        { updateResult
                            | cmdUps =
                                List.append
                                    updateResult.cmdUps
                                    [ CmdUp.gTag "in currency selected" "input" (currencySymbol newType) 0 ]
                        }
                   )

        AmountOutChanged input ->
            let
                prevInputs =
                    prevModel.inputs

                filteredInput =
                    input |> Utils.filterPositiveNumericInput

                ( newMaybeAmountOut, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretAmount filteredInput of
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
                        | inputs = { prevInputs | amountOut = filteredInput }
                        , errors = newErrors
                        , lastAmountInputChanged = AmountOut
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
            UpdateResult
                { prevModel
                    | showOutTypeDropdown =
                        if prevModel.showOutTypeDropdown then
                            False

                        else
                            True
                }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "out currency dropdown clicked" "input" "" 0 ]

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
                |> (\updateResult ->
                        { updateResult
                            | cmdUps =
                                List.append
                                    updateResult.cmdUps
                                    [ CmdUp.gTag "out currency selected" "input" (currencySymbol newType) 0 ]
                        }
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
            UpdateResult
                { prevModel
                    | showMarginModal =
                        if prevModel.showMarginModal then
                            False

                        else
                            True
                }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "margin box clicked" "input" "" 0 ]

        MarginInputChanged input ->
            let
                prevInputs =
                    prevModel.inputs

                filteredInput =
                    input |> Utils.filterPositiveNumericInput

                ( newMargin, newMarginType, newErrors ) =
                    let
                        prevErrors =
                            prevModel.errors
                    in
                    case interpretMargin filteredInput prevInputs.marginType of
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
                        |> (case prevModel.lastAmountInputChanged of
                                AmountIn ->
                                    tryAutofillAmountOut

                                AmountOut ->
                                    tryAutofillAmountIn
                           )
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
                        |> (case prevModel.lastAmountInputChanged of
                                AmountIn ->
                                    tryAutofillAmountOut

                                AmountOut ->
                                    tryAutofillAmountIn
                           )
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                (appCmds
                    ++ [ CmdUp.gTag "margin button clicked" "input" (marginButtonTypeToString typeClicked) 0 ]
                )

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
            UpdateResult
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
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "window box clicked" "input" (intervalTypeToString intervalType) 0 ]

        IntervalInputChanged input ->
            case prevModel.showIntervalModal of
                Just intervalType ->
                    let
                        prevInputs =
                            prevModel.inputs

                        filteredInput =
                            input |> Utils.filterPositiveNumericInput

                        ( newInterval, newErrors ) =
                            let
                                prevErrors =
                                    prevModel.errors
                            in
                            case interpretInterval filteredInput of
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
                                | inputs = { prevInputs | interval = filteredInput }
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
                    UpdateResult
                        (prevModel
                            |> updateUserInterval
                                intervalType
                                (UserInterval
                                    (getUserInterval intervalType prevModel |> .num)
                                    newUnit
                                )
                        )
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.gTag "interval unit changed" "input" (intervalUnitToString newUnit) 0 ]

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

        PlaceOrderClicked factoryType userInfo userParameters ->
            let
                createParameters =
                    CTypes.buildCreateParameters userInfo userParameters
            in
            UpdateResult
                { prevModel
                    | txChainStatus = Just <| Confirm factoryType createParameters
                    , depositAmount =
                        case initiatorRole prevModel.mode of
                            Buyer ->
                                Just <| CTypes.calculateFullInitialDeposit createParameters

                            Seller ->
                                Maybe.map
                                    TokenValue.fromFloatWithWarning
                                    (getAmountIn prevModel)
                }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "place order clicked" "txchain" prevModel.inputs.paymentMethod (createParameters.tradeAmount |> TokenValue.getFloatValueWithWarning |> floor) ]

        AbortCreate ->
            UpdateResult
                { prevModel | txChainStatus = Nothing }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "abort" "txchain" "" 0 ]

        ConfirmCreate factoryType createParameters fullDepositAmount ->
            let
                ( txChainStatus, chainCmd ) =
                    case factoryType of
                        Native _ ->
                            initiateCreateCall factoryType createParameters

                        Token tokenType ->
                            let
                                approveChainCmd =
                                    let
                                        txParams =
                                            TokenContract.approve
                                                (Config.tokenContractAddress tokenType)
                                                (Config.factoryAddress factoryType)
                                                (TokenValue.getEvmValue fullDepositAmount)
                                                |> Eth.toSend

                                        customSend =
                                            { onMined = Nothing
                                            , onSign = Just (ApproveSigned tokenType createParameters)
                                            , onBroadcast = Nothing
                                            }
                                    in
                                    ChainCmd.custom customSend txParams
                            in
                            case prevModel.userAllowance of
                                Just allowance ->
                                    if BigInt.compare allowance (TokenValue.getEvmValue fullDepositAmount) /= LT then
                                        initiateCreateCall factoryType createParameters

                                    else
                                        ( Just (ApproveNeedsSig tokenType), approveChainCmd )

                                Nothing ->
                                    ( Just (ApproveNeedsSig tokenType), approveChainCmd )
            in
            UpdateResult
                { prevModel | txChainStatus = txChainStatus }
                Cmd.none
                chainCmd
                [ CmdUp.gTag "confirm" "txchain" "" 0 ]

        ApproveSigned tokenType createParameters result ->
            case result of
                Ok txHash ->
                    UpdateResult
                        { prevModel | txChainStatus = Just <| ApproveMining tokenType createParameters txHash }
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.gTag "approve signed" "txchain" "" 0 ]

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <| UN.web3SigError "appove" s ]

        AllowanceFetched tokenType fetchResult ->
            case fetchResult of
                Ok allowance ->
                    let
                        newModel =
                            { prevModel
                                | userAllowance = Just allowance
                            }
                    in
                    case ( newModel.txChainStatus, newModel.depositAmount ) of
                        ( Just (ApproveMining _ createParameters _), Just depositAmount ) ->
                            if BigInt.compare allowance (TokenValue.getEvmValue depositAmount) /= LT then
                                let
                                    ( txChainStatus, chainCmd ) =
                                        initiateCreateCall (Token tokenType) createParameters
                                in
                                UpdateResult
                                    { newModel | txChainStatus = txChainStatus }
                                    Cmd.none
                                    chainCmd
                                    []

                            else
                                justModelUpdate newModel

                        _ ->
                            justModelUpdate newModel

                Err httpError ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <| UN.web3FetchError "allowance" httpError ]

        CreateSigned factoryType result ->
            case result of
                Ok txHash ->
                    UpdateResult
                        { prevModel | txChainStatus = Just <| CreateMining factoryType txHash }
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.gTag "create signed" "txchain" "" 0 ]

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <| UN.web3SigError "create" s ]

        CreateMined factoryType (Err s) ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ CmdUp.UserNotice <| UN.web3MiningError "create" s ]

        CreateMined factory (Ok txReceipt) ->
            let
                maybeId =
                    CTypes.txReceiptToCreatedTradeSellId factory txReceipt
                        |> Result.toMaybe
                        |> Maybe.andThen BigIntHelpers.toInt
            in
            case maybeId of
                Just id ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.gTag "create mined" "txchain" "" 0
                        , CmdUp.GotoRoute (Routing.Trade factory id)
                        ]

                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <|
                            UN.unexpectedError "Error getting the ID of the created offer. Check the \"My Trades\" page for your open offer." txReceipt
                        ]

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
                if value > 0 then
                    Ok (Just value)

                else
                    Err "Number must be greater than 0"

            Nothing ->
                Err "Invalid amount"


interpretMargin : String -> MarginButtonType -> Result String (Maybe Float)
interpretMargin input marginType =
    if input == "" then
        Ok Nothing

    else
        String.toFloat input
            |> Result.fromMaybe "Invalid margin"
            |> Result.map (\percent -> percent / 100.0)
            |> Result.map
                (if marginType == Loss then
                    negate

                 else
                    identity
                )
            |> Result.map Just


interpretInterval : String -> Result String (Maybe Int)
interpretInterval input =
    if input == "" then
        Ok Nothing

    else
        case String.toInt input of
            Just value ->
                if value > 0 then
                    Ok (Just value)

                else
                    Err "Number must be greater than 0"

            Nothing ->
                Err "Must be an integer"


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
                                    (amountIn * price) * (1 + prevModel.margin)

                                Seller ->
                                    let
                                        tradeAmountAfterDevFee =
                                            amountIn / 1.01

                                        equivalentForeignCrypto =
                                            tradeAmountAfterDevFee / price
                                    in
                                    equivalentForeignCrypto * (1 + prevModel.margin)
                        )
                        (getAmountIn prevModel)

                oldInputs =
                    prevModel.inputs

                oldErrors =
                    prevModel.errors
            in
            ( case newAmountOut of
                Just amountOut ->
                    { prevModel
                        | inputs = { oldInputs | amountOut = String.fromFloat amountOut }
                        , errors = { oldErrors | amountOut = Nothing }
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
                                    (amountOut / (1 + prevModel.margin)) / price

                                Seller ->
                                    let
                                        amountOutMinusMargin =
                                            amountOut / (1 + prevModel.margin)

                                        equivalentDai =
                                            amountOutMinusMargin * price
                                    in
                                    equivalentDai * 1.01
                        )
                        (getAmountOut prevModel)

                oldInputs =
                    prevModel.inputs

                oldErrors =
                    prevModel.errors
            in
            ( case newAmountIn of
                Just amountIn ->
                    { prevModel
                        | inputs =
                            { oldInputs | amountIn = String.fromFloat amountIn }
                        , errors = { oldErrors | amountIn = Nothing }
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


initiateCreateCall : FactoryType -> CTypes.CreateParameters -> ( Maybe TxChainStatus, ChainCmd Msg )
initiateCreateCall factoryType parameters =
    let
        txParams =
            Contracts.Wrappers.openTrade
                factoryType
                parameters
                |> Eth.toSend

        customSend =
            { onMined = Just ( CreateMined factoryType, Nothing )
            , onSign = Just (CreateSigned factoryType)
            , onBroadcast = Nothing
            }
    in
    ( Just (CreateNeedsSig factoryType)
    , ChainCmd.custom customSend txParams
    )


updateCurrencyTypesFromInput : Model -> Model
updateCurrencyTypesFromInput prevModel =
    let
        ( newForeignCurrency, newDhTokenType ) =
            case ( initiatorRole prevModel.mode, ( prevModel.inputs.inType, prevModel.inputs.outType ) ) of
                ( Buyer, ( External externalSymbol, DHToken dhTokenType ) ) ->
                    ( externalSymbol, dhTokenType )

                ( Seller, ( DHToken dhTokenType, External externalSymbol ) ) ->
                    ( externalSymbol, dhTokenType )

                _ ->
                    let
                        _ =
                            Debug.log "unexpected currency types in input when trying to update model currency types" ""
                    in
                    ( prevModel.foreignCurrencyType, prevModel.dhTokenType )
    in
    { prevModel
        | foreignCurrencyType = newForeignCurrency
        , dhTokenType = newDhTokenType
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 2000 (always Refresh)
        , Time.every 500 UpdateNow
        ]
