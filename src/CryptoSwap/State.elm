module CryptoSwap.State exposing (init, runCmdDown, subscriptions, update)

import Base58
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdDown
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import CryptoSwap.Types exposing (..)
import Dict
import Eth
import Helpers.BigInt as BigIntHelpers
import Helpers.Time as TimeHelpers
import Maybe.Extra
import PriceFetch
import Routing
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Wallet.State -> UpdateResult
init wallet =
    { wallet = wallet
    , initiatorRole = Seller
    , amountInInput = ""
    , amountIn = Nothing
    , amountOutInput = ""
    , dhToken =
        Wallet.factory wallet
            |> Maybe.withDefault (Native XDai)
    , foreignCrypto = ZEC
    , marginInput = "2"
    , margin = Just 0.02
    , amountOut = Nothing
    , receiveAddress = ""
    , showDhTokenDropdown = False
    , showForeignCryptoDropdown = False
    , errors = noErrors
    , txChainStatus = Nothing
    , depositAmount = Nothing
    , allowance = Nothing
    , prices = []
    , now = Time.millisToPosix 0
    }
        |> update Refresh


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
                                |> tryUpdateAmountOut
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

        AmountInChanged input ->
            let
                ( newMaybeAmountIn, newErrors ) =
                    case interpretAmount input of
                        Ok maybeAmount ->
                            ( maybeAmount
                            , prevModel.errors
                            )

                        Err errStr ->
                            let
                                prevErrors =
                                    prevModel.errors
                            in
                            ( Nothing
                            , { prevErrors | amountIn = Just errStr }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | amountInInput = input
                        , amountIn = newMaybeAmountIn
                        , errors = newErrors
                    }
                        |> tryUpdateAmountOut
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        AmountOutChanged input ->
            let
                ( newMaybeAmountOut, newErrors ) =
                    case interpretAmount input of
                        Ok maybeAmount ->
                            ( maybeAmount
                            , prevModel.errors
                            )

                        Err errStr ->
                            let
                                prevErrors =
                                    prevModel.errors
                            in
                            ( Nothing
                            , { prevErrors
                                | amountOut = Just errStr
                              }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | amountOutInput = input
                        , amountOut = newMaybeAmountOut
                        , errors = newErrors
                    }
                        |> tryUpdateAmountIn
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        MarginChanged input ->
            let
                ( newMaybeMargin, newErrors ) =
                    case interpretMargin input of
                        Ok maybeMargin ->
                            ( maybeMargin
                            , prevModel.errors
                            )

                        Err errStr ->
                            let
                                prevErrors =
                                    prevModel.errors
                            in
                            ( Nothing
                            , { prevErrors | margin = Just errStr }
                            )

                ( newModel, appCmds ) =
                    { prevModel
                        | marginInput = input
                        , margin = newMaybeMargin
                        , errors = newErrors
                    }
                        |> tryUpdateAmountOut
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        SwapClicked ->
            let
                ( newModel, appCmds ) =
                    { prevModel
                        | initiatorRole =
                            case prevModel.initiatorRole of
                                Buyer ->
                                    Seller

                                Seller ->
                                    Buyer
                        , amountInInput = prevModel.amountOutInput
                        , amountIn = prevModel.amountOut
                        , amountOutInput = prevModel.amountInInput
                        , amountOut = prevModel.amountIn
                    }
                        |> tryUpdateDaiAmount
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        TokenTypeClicked ->
            justModelUpdate
                { prevModel
                    | showDhTokenDropdown =
                        not prevModel.showDhTokenDropdown
                }

        ChangeTokenType factory ->
            let
                ( newModel, appCmds ) =
                    { prevModel
                        | dhToken = factory
                        , showDhTokenDropdown = False
                    }
                        |> tryUpdateDaiAmount
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        ForeignCryptoTypeClicked ->
            justModelUpdate
                { prevModel
                    | showForeignCryptoDropdown =
                        not prevModel.showForeignCryptoDropdown
                }

        ChangeForeignCrypto crypto ->
            let
                ( newModel, appCmds ) =
                    { prevModel
                        | foreignCrypto = crypto
                        , showForeignCryptoDropdown = False
                    }
                        |> tryUpdateForeignCryptoAmount
            in
            UpdateResult
                newModel
                Cmd.none
                ChainCmd.none
                appCmds

        ReceiveAddressChanged input ->
            justModelUpdate
                { prevModel
                    | receiveAddress =
                        input
                }

        PlaceOrderClicked factoryType userInfo userParameters ->
            let
                createParameters =
                    CTypes.buildCreateParameters userInfo userParameters
            in
            justModelUpdate
                { prevModel
                    | txChainStatus = Just <| Confirm factoryType createParameters prevModel.receiveAddress
                    , depositAmount =
                        Just <|
                            (CTypes.calculateFullInitialDeposit createParameters
                                |> TokenValue.getEvmValue
                            )
                }

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

        CmdUp cmdUp ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ cmdUp ]

        NoOp ->
            justModelUpdate
                prevModel


runCmdDown : CmdDown.CmdDown -> Model -> UpdateResult
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
            justModelUpdate
                { prevModel
                    | showDhTokenDropdown = False
                    , showForeignCryptoDropdown = False
                }


tryUpdateAmountOut : Model -> ( Model, List (CmdUp Msg) )
tryUpdateAmountOut prevModel =
    case PriceFetch.getPriceData prevModel.foreignCrypto prevModel.prices of
        Just (PriceFetch.Ok price) ->
            let
                newAmountOut =
                    Maybe.map2
                        (\amountIn margin ->
                            case prevModel.initiatorRole of
                                Buyer ->
                                    let
                                        equivalentDai =
                                            amountIn * price
                                    in
                                    equivalentDai / (1 + margin)

                                Seller ->
                                    let
                                        tradeAmountAfterDevFee =
                                            amountIn - (amountIn / 101)

                                        equivalentForeignCrypto =
                                            tradeAmountAfterDevFee / price
                                    in
                                    equivalentForeignCrypto - (equivalentForeignCrypto * margin)
                        )
                        prevModel.amountIn
                        prevModel.margin
            in
            ( { prevModel
                | amountOut =
                    case newAmountOut of
                        Just _ ->
                            newAmountOut

                        Nothing ->
                            prevModel.amountOut
                , amountOutInput =
                    newAmountOut
                        |> Maybe.map String.fromFloat
                        |> Maybe.withDefault prevModel.amountOutInput
              }
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


tryUpdateAmountIn : Model -> ( Model, List (CmdUp Msg) )
tryUpdateAmountIn prevModel =
    case PriceFetch.getPriceData prevModel.foreignCrypto prevModel.prices of
        Just (PriceFetch.Ok price) ->
            let
                newAmountIn =
                    Maybe.map2
                        (\amountOut margin ->
                            case prevModel.initiatorRole of
                                Buyer ->
                                    let
                                        equivalentCrypto =
                                            amountOut / price
                                    in
                                    equivalentCrypto * (1 + margin)

                                Seller ->
                                    let
                                        amountOutPlusMargin =
                                            amountOut / (1 - margin)

                                        equivalentDai =
                                            amountOutPlusMargin * price
                                    in
                                    equivalentDai + (equivalentDai / 100)
                        )
                        prevModel.amountOut
                        prevModel.margin
            in
            ( { prevModel
                | amountIn =
                    case newAmountIn of
                        Just _ ->
                            newAmountIn

                        Nothing ->
                            prevModel.amountIn
                , amountInInput =
                    newAmountIn
                        |> Maybe.map String.fromFloat
                        |> Maybe.withDefault prevModel.amountInInput
              }
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


tryUpdateDaiAmount : Model -> ( Model, List (CmdUp Msg) )
tryUpdateDaiAmount prevModel =
    case prevModel.initiatorRole of
        Buyer ->
            tryUpdateAmountOut prevModel

        Seller ->
            tryUpdateAmountIn prevModel


tryUpdateForeignCryptoAmount : Model -> ( Model, List (CmdUp Msg) )
tryUpdateForeignCryptoAmount prevModel =
    case prevModel.initiatorRole of
        Buyer ->
            tryUpdateAmountIn prevModel

        Seller ->
            tryUpdateAmountOut prevModel


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



-- interpretReceiveAddress : ForeignCrypto -> String -> Result String (Maybe String)
-- interpretReceiveAddress crypto input =
--     if input == "" then
--         Ok Nothing
--     else
--         let
--             b58Validate s =
--                 case Base58.decode s of
--                     Ok _ ->
--                         Ok (Just input)
--                     Err _ ->
--                         Err "Invalid address (b58 decode failed)"
--         in
--         case crypto of
--             ZEC ->
--                 b58Validate input
--             XMR ->
--                 if
--                     (String.length input == 106 || String.length input == 95)
--                         && (String.startsWith "4" input || String.startsWith "8" input)
--                 then
--                     Ok (Just input)
--                 else
--                     Err "Invalid address"
--             BTC ->
--                 b58Validate input


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 10000 (always Refresh)
        , Time.every 500 UpdateNow
        ]
