module Create.State exposing (init, runCmdDown, subscriptions, update)

import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown
import CmdUp
import CommonTypes exposing (..)
import Config
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Create.PMWizard.State as PMWizard
import Create.Types exposing (..)
import Eth
import Eth.Types exposing (Address)
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Helpers.Tuple exposing (extractTuple3Result, mapEachTuple3)
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Prices exposing (Price)
import Routing
import SmoothScroll exposing (scrollToWithOptions)
import String.Extra
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN
import Wallet


init : Wallet.State -> UpdateResult
init wallet =
    let
        model =
            { wallet = wallet
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
    , paymentMethod = ""
    , autorecallInterval = Time.millisToPosix 0
    , autoabortInterval = Time.millisToPosix 0
    , autoreleaseInterval = Time.millisToPosix 0
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh time ->
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

        ChangeRole initiatorRole ->
            let
                oldInputs =
                    prevModel.inputs
            in
            UpdateResult
                { prevModel | inputs = { oldInputs | userRole = initiatorRole } }
                Cmd.none
                ChainCmd.none
                [ case initiatorRole of
                    Buyer ->
                        CmdUp.gTag "create offer type changed" "input" "sell dai" 0

                    Seller ->
                        CmdUp.gTag "create offer type changed" "input" "buy dai" 0
                ]

        TradeAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | daiAmount = newAmountStr
                        }
                )

        FiatAmountChanged newAmountStr ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                (prevModel
                    |> updateInputs
                        { oldInputs
                            | fiatAmount = newAmountStr
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
            UpdateResult
                ({ prevModel
                    | showFiatTypeDropdown = flag
                 }
                    |> (if flag then
                            updateInputs { oldInputs | fiatType = "" }

                        else
                            identity
                       )
                )
                Cmd.none
                ChainCmd.none
                (if flag then
                    [ CmdUp.gTag "currency-selector-clicked" "input" "" 0 ]

                 else
                    []
                )

        CreateClicked factoryType userInfo ->
            case validateInputs prevModel.inputs of
                Ok userParameters ->
                    let
                        createParameters =
                            CTypes.buildCreateParameters userInfo userParameters
                    in
                    justModelUpdate
                        { prevModel
                            | txChainStatus = Just <| Confirm factoryType createParameters
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
            UpdateResult
                { prevModel | txChainStatus = Nothing }
                Cmd.none
                ChainCmd.none
                [ CmdUp.gTag "abort" "abort" "create" 0 ]

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
                                                fullDepositAmount
                                                |> Eth.toSend

                                        customSend =
                                            { onMined = Nothing
                                            , onSign = Just (ApproveSigned tokenType createParameters)
                                            , onBroadcast = Nothing
                                            }
                                    in
                                    ChainCmd.custom customSend txParams
                            in
                            case prevModel.allowance of
                                Just allowance ->
                                    if BigInt.compare allowance fullDepositAmount /= LT then
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
                []

        ApproveSigned tokenType createParameters result ->
            case result of
                Ok txHash ->
                    justModelUpdate { prevModel | txChainStatus = Just <| ApproveMining tokenType createParameters txHash }

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
                                | allowance = Just allowance
                            }
                    in
                    case ( newModel.txChainStatus, newModel.depositAmount ) of
                        ( Just (ApproveMining _ createParameters _), Just depositAmount ) ->
                            if BigInt.compare allowance depositAmount /= LT then
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
                    justModelUpdate { prevModel | txChainStatus = Just <| CreateMining factoryType txHash }

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
                        [ CmdUp.GotoRoute (Routing.Trade factory id) ]

                Nothing ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ CmdUp.UserNotice <|
                            UN.unexpectedError "Error getting the ID of the created offer. Check the \"My Trades\" page for your open offer." txReceipt
                        ]

        Web3Connect ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ CmdUp.Web3Connect ]

        NoOp ->
            justModelUpdate prevModel

        CmdUp cmdUp ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ cmdUp ]


runCmdDown : CmdDown.CmdDown -> Model -> UpdateResult
runCmdDown cmdDown prevModel =
    case cmdDown of
        CmdDown.UpdateWallet wallet ->
            UpdateResult
                ({ prevModel | wallet = wallet } |> updateInputs prevModel.inputs)
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
            justModelUpdate
                { prevModel | showFiatTypeDropdown = False }


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


updateInputs : Inputs -> Model -> Model
updateInputs newInputs model =
    { model | inputs = newInputs }
        |> updateParameters


updateParameters : Model -> Model
updateParameters model =
    let
        validateResult =
            validateInputs model.inputs

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
                (Wallet.userInfo model.wallet)
                (Result.toMaybe validateResult)
        , errors = newErrors
    }


validateInputs : Inputs -> Result Errors CTypes.UserParameters
validateInputs inputs =
    Result.map5
        (\daiAmount fiatAmount fiatType paymentMethod ( autorecallInterval, autoabortInterval, autoreleaseInterval ) ->
            { initiatorRole = inputs.userRole
            , tradeAmount = daiAmount
            , price =
                Price
                    fiatType
                    fiatAmount
            , autorecallInterval = autorecallInterval
            , autoabortInterval = autoabortInterval
            , autoreleaseInterval = autoreleaseInterval
            , paymentMethods =
                [ PaymentMethod
                    PaymentMethods.Custom
                    paymentMethod
                ]
            }
        )
        (interpretDaiAmount inputs.daiAmount
            |> Result.mapError (\e -> { noErrors | daiAmount = Just e })
        )
        (interpretFiatAmount inputs.fiatAmount
            |> Result.mapError (\e -> { noErrors | fiatAmount = Just e })
        )
        (interpretFiatType inputs.fiatType
            |> Result.mapError (\e -> { noErrors | fiatType = Just e })
        )
        (interpretPaymentMethods inputs.paymentMethod
            |> Result.mapError (\e -> { noErrors | paymentMethod = Just e })
        )
        (mapEachTuple3
            (\time ->
                if Time.posixToMillis time > 0 then
                    Ok time

                else
                    Err { noErrors | autorecallInterval = Just "Must specify a non-zero time for this window" }
            )
            (\time ->
                if Time.posixToMillis time > 0 then
                    Ok time

                else
                    Err { noErrors | autoabortInterval = Just "Must specify a non-zero time for this window" }
            )
            (\time ->
                if Time.posixToMillis time > 0 then
                    Ok time

                else
                    Err { noErrors | autoreleaseInterval = Just "Must specify a non-zero time for this window" }
            )
            ( inputs.autorecallInterval, inputs.autoabortInterval, inputs.autoreleaseInterval )
            |> extractTuple3Result
        )


interpretDaiAmount : String -> Result String TokenValue
interpretDaiAmount input =
    if input == "" then
        Err "You must specify a trade amount."

    else
        case TokenValue.fromString input of
            Nothing ->
                Err "I don't understand this number."

            Just value ->
                if TokenValue.getFloatValueWithWarning value < 1 then
                    Err <| "Trade amount can't be less than 1."

                else
                    Ok value


interpretFiatType : String -> Result String String
interpretFiatType input =
    String.Extra.nonEmpty input
        |> Result.fromMaybe "You must specify a fiat type."


interpretFiatAmount : String -> Result String Float
interpretFiatAmount input =
    if input == "" then
        Err "You must specify a fiat price."

    else
        case String.toFloat input of
            Nothing ->
                Err "I don't understand this number."

            Just value ->
                Ok value


interpretPaymentMethods : String -> Result String String
interpretPaymentMethods paymentMethod =
    if paymentMethod == "" then
        Err "Must specify a payment method."

    else
        Ok paymentMethod


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Refresh
