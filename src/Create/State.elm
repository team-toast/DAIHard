module Create.State exposing (init, subscriptions, update, updateUserInfo, updateWeb3Context)

import AppCmd
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Create.PMWizard.State as PMWizard
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Margin
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Routing
import SmoothScroll exposing (scrollToWithOptions)
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN


init : EthHelpers.Web3Context -> Maybe UserInfo -> UpdateResult
init web3Context userInfo =
    let
        model =
            { web3Context = web3Context
            , userInfo = userInfo
            , inputs = initialInputs
            , errors = noErrors
            , showFiatTypeDropdown = False
            , createParameters = Nothing
            , txChainStatus = Nothing
            , depositAmount = Nothing
            , allowance = Nothing
            }
    in
    UpdateResult
        (model |> updateInputs initialInputs)
        Cmd.none
        ChainCmd.none
        []


initialInputs =
    { userRole = Seller
    , daiAmount = ""
    , fiatType = "USD"
    , fiatAmount = ""
    , margin = "0"
    , paymentMethod = ""
    , autorecallInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    , autoabortInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    , autoreleaseInterval = Time.millisToPosix <| 1000 * 60 * 60 * 24
    }


updateUserInfo : Maybe UserInfo -> Model -> ( Model, Cmd Msg )
updateUserInfo userInfo model =
    ( { model | userInfo = userInfo }
        |> updateInputs model.inputs
    , case ( userInfo, model.web3Context.factoryType ) of
        ( Just uInfo, Token tokenType ) ->
            Contracts.Wrappers.getAllowanceCmd
                model.web3Context
                tokenType
                uInfo.address
                (Config.factoryAddress model.web3Context.factoryType)
                AllowanceFetched

        _ ->
            Cmd.none
    )


updateWeb3Context : EthHelpers.Web3Context -> Model -> Model
updateWeb3Context newWeb3Context model =
    { model | web3Context = newWeb3Context }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh time ->
            case ( prevModel.userInfo, prevModel.web3Context.factoryType ) of
                ( Just userInfo, Token tokenType ) ->
                    let
                        cmd =
                            Contracts.Wrappers.getAllowanceCmd
                                prevModel.web3Context
                                tokenType
                                userInfo.address
                                (Config.factoryAddress prevModel.web3Context.factoryType)
                                AllowanceFetched
                    in
                    UpdateResult
                        prevModel
                        cmd
                        ChainCmd.none
                        []

                _ ->
                    justModelUpdate prevModel

        ChangeRole initiatorRole ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel | inputs = { oldInputs | userRole = initiatorRole } }

        TradeAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs

                newFiatAmountString =
                    recalculateFiatAmountString newAmountStr oldInputs.margin oldInputs.fiatType
                        |> Maybe.withDefault oldInputs.fiatAmount
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | daiAmount = newAmountStr
                            , fiatAmount = newFiatAmountString
                        }
                )

        FiatAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs

                newMarginString =
                    recalculateMarginString oldInputs.daiAmount newAmountStr oldInputs.fiatType
                        |> Maybe.withDefault oldInputs.margin
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | fiatAmount = newAmountStr
                            , margin = newMarginString
                        }
                )

        FiatTypeChanged newTypeStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | fiatType = newTypeStr
                            , margin =
                                recalculateMarginString oldInputs.daiAmount oldInputs.fiatAmount newTypeStr
                                    |> Maybe.withDefault oldInputs.margin
                        }
                )

        FiatTypeLostFocus ->
            justModelUpdate
                { prevModel
                    | showFiatTypeDropdown = False
                }

        ChangePaymentMethodText newText ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | paymentMethod = newText
                        }
                )

        AutorecallIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autorecallInterval = newTime })

        AutoabortIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autoabortInterval = newTime })

        AutoreleaseIntervalChanged newTime ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate (prevModel |> updateInputs { oldInputs | autoreleaseInterval = newTime })

        ShowCurrencyDropdown flag ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                ({ prevModel
                    | showFiatTypeDropdown = flag
                 }
                    |> (if flag then
                            updateInputs { oldInputs | fiatType = "" }

                        else
                            identity
                       )
                )

        ClearDraft ->
            justModelUpdate { prevModel | inputs = initialInputs }

        CreateClicked userInfo ->
            case validateInputs prevModel.web3Context.factoryType prevModel.inputs of
                Ok userParameters ->
                    let
                        createParameters =
                            CTypes.buildCreateParameters userInfo userParameters
                    in
                    justModelUpdate
                        { prevModel
                            | txChainStatus = Just <| Confirm createParameters
                            , depositAmount =
                                Just <|
                                    (CTypes.calculateFullInitialDeposit createParameters
                                        |> TokenValue.getEvmValue
                                    )
                        }

                Err inputErrors ->
                    UpdateResult
                        { prevModel | errors = inputErrors }
                        (Task.attempt (always NoOp)
                            (let
                                defaultConfig =
                                    SmoothScroll.defaultConfig
                             in
                             scrollToWithOptions
                                { defaultConfig
                                    | offset = 60
                                }
                                "inputError"
                            )
                        )
                        ChainCmd.none
                        []

        AbortCreate ->
            justModelUpdate { prevModel | txChainStatus = Nothing }

        ConfirmCreate createParameters fullDepositAmount ->
            let
                ( txChainStatus, chainCmd ) =
                    case prevModel.web3Context.factoryType of
                        Native _ ->
                            initiateCreateCall prevModel.web3Context.factoryType createParameters

                        Token tokenType ->
                            let
                                approveChainCmd =
                                    let
                                        txParams =
                                            TokenContract.approve
                                                (Config.tokenContractAddress tokenType)
                                                (Config.factoryAddress prevModel.web3Context.factoryType)
                                                fullDepositAmount
                                                |> Eth.toSend

                                        customSend =
                                            { onMined = Nothing
                                            , onSign = Just (ApproveSigned createParameters)
                                            , onBroadcast = Nothing
                                            }
                                    in
                                    ChainCmd.custom customSend txParams
                            in
                            case prevModel.allowance of
                                Just allowance ->
                                    if BigInt.compare allowance fullDepositAmount /= LT then
                                        initiateCreateCall prevModel.web3Context.factoryType createParameters

                                    else
                                        ( Just ApproveNeedsSig, approveChainCmd )

                                Nothing ->
                                    ( Just ApproveNeedsSig, approveChainCmd )
            in
            UpdateResult
                { prevModel | txChainStatus = txChainStatus }
                Cmd.none
                chainCmd
                []

        ApproveSigned createParameters result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| ApproveMining createParameters txHash }

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3SigError "appove" s ]

        AllowanceFetched fetchResult ->
            case fetchResult of
                Ok allowance ->
                    let
                        newModel =
                            { prevModel
                                | allowance = Just allowance
                            }
                    in
                    case ( newModel.txChainStatus, newModel.depositAmount ) of
                        ( Just (ApproveMining createParameters _), Just depositAmount ) ->
                            if BigInt.compare allowance depositAmount /= LT then
                                let
                                    ( txChainStatus, chainCmd ) =
                                        initiateCreateCall newModel.web3Context.factoryType createParameters
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
                        [ AppCmd.UserNotice <| UN.web3FetchError "allowance" httpError ]

        CreateSigned result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| CreateMining txHash }

                Err s ->
                    UpdateResult
                        { prevModel | txChainStatus = Nothing }
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3SigError "create" s ]

        CreateMined (Err s) ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.UserNotice <| UN.web3MiningError "create" s ]

        CreateMined (Ok txReceipt) ->
            let
                maybeId =
                    CTypes.txReceiptToCreatedTradeSellId prevModel.web3Context.factoryType txReceipt
                        |> Result.toMaybe
                        |> Maybe.andThen BigIntHelpers.toInt
            in
            case maybeId of
                Just id ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.GotoRoute (Routing.Trade id) ]

                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <|
                            UN.unexpectedError "Error getting the ID of the created contract" txReceipt
                        ]

        Web3Connect ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.Web3Connect ]

        NoOp ->
            justModelUpdate prevModel


initiateCreateCall : FactoryType -> CTypes.CreateParameters -> ( Maybe TxChainStatus, ChainCmd Msg )
initiateCreateCall factoryType parameters =
    let
        txParams =
            Contracts.Wrappers.openTrade
                factoryType
                parameters
                |> Eth.toSend

        customSend =
            { onMined = Just ( CreateMined, Nothing )
            , onSign = Just CreateSigned
            , onBroadcast = Nothing
            }
    in
    ( Just CreateNeedsSig
    , ChainCmd.custom customSend txParams
    )


updateInputs : Inputs -> Model -> Model
updateInputs newInputs model =
    { model | inputs = newInputs }
        |> updateParameters


updateParameters : Model -> Model
updateParameters model =
    let
        validateResult =
            validateInputs model.web3Context.factoryType model.inputs

        -- Don't log errors right away (wait until the user tries to submit)
        -- But if there are already errors displaying, update them accordingly
        newErrors =
            if model.errors == noErrors then
                noErrors

            else
                case validateResult of
                    Ok _ ->
                        noErrors

                    Err errors ->
                        errors
    in
    { model
        | createParameters =
            Maybe.map2
                CTypes.buildCreateParameters
                model.userInfo
                (Result.toMaybe validateResult)
        , errors = newErrors
    }


validateInputs : FactoryType -> Inputs -> Result Errors CTypes.UserParameters
validateInputs factoryType inputs =
    Result.map3
        (\daiAmount fiatAmount paymentMethod ->
            { initiatorRole = inputs.userRole
            , tradeAmount = daiAmount
            , price = { fiatType = inputs.fiatType, amount = fiatAmount }
            , autorecallInterval = inputs.autorecallInterval
            , autoabortInterval = inputs.autoabortInterval
            , autoreleaseInterval = inputs.autoreleaseInterval
            , paymentMethods =
                [ PaymentMethod
                    PaymentMethods.Custom
                    paymentMethod
                ]
            }
        )
        (interpretDaiAmount factoryType inputs.daiAmount
            |> Result.mapError (\e -> { noErrors | daiAmount = Just e })
        )
        (interpretFiatAmount inputs.fiatAmount
            |> Result.mapError (\e -> { noErrors | fiat = Just e })
        )
        (interpretPaymentMethods inputs.paymentMethod
            |> Result.mapError (\e -> { noErrors | paymentMethod = Just e })
        )


interpretDaiAmount : FactoryType -> String -> Result String TokenValue
interpretDaiAmount factoryType input =
    if input == "" then
        Err "You must specify a trade amount."

    else
        case TokenValue.fromString input of
            Nothing ->
                Err "I don't understand this number."

            Just value ->
                if TokenValue.getFloatValueWithWarning value < 1 then
                    Err <| "Trade amount must be a least 1 " ++ Config.tokenUnitName factoryType ++ "."

                else
                    Ok value


interpretFiatAmount : String -> Result String BigInt
interpretFiatAmount input =
    if input == "" then
        Err "You must specify a fiat price."

    else
        case BigInt.fromString input of
            Nothing ->
                case String.toFloat input of
                    Just _ ->
                        Err "Fractional fiat amounts (i.e. $1.20) are not supported. Use a whole number."

                    _ ->
                        Err "I don't understand this number."

            Just value ->
                Ok value


interpretPaymentMethods : String -> Result String String
interpretPaymentMethods paymentMethod =
    if paymentMethod == "" then
        Err "Must specify a payment method."

    else
        Ok paymentMethod


recalculateFiatAmountString : String -> String -> String -> Maybe String
recalculateFiatAmountString daiAmountStr marginString fiatType =
    case fiatType of
        "USD" ->
            if String.isEmpty daiAmountStr then
                Just ""

            else
                case ( String.toFloat daiAmountStr, Margin.stringToMarginFloat marginString ) of
                    ( Just daiAmount, Just marginFloat ) ->
                        Just
                            (daiAmount
                                + (daiAmount * marginFloat)
                                |> round
                                |> String.fromInt
                            )

                    _ ->
                        Nothing

        _ ->
            Nothing


recalculateMarginString : String -> String -> String -> Maybe String
recalculateMarginString daiAmountString fiatAmountString fiatType =
    case fiatType of
        "USD" ->
            Maybe.map2
                Margin.marginFromFloats
                (String.toFloat daiAmountString)
                (String.toFloat fiatAmountString)
                |> Maybe.map Margin.marginToString

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Refresh
