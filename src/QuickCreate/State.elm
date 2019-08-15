module QuickCreate.State exposing (init, subscriptions, update, updateWalletState)

import AppCmd
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Config
import Contracts.Generated.ERC20Token as TokenContract
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Http
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import QuickCreate.Types exposing (..)
import Routing
import Task
import Time
import TokenValue exposing (TokenValue)
import UserNotice as UN exposing (UserNotice)
import Wallet


init : Wallet.State -> UpdateResult
init wallet =
    UpdateResult
        { wallet = wallet
        , state = Menu NoneStarted
        , tokenAllowance = Nothing
        , textInput = ""
        }
        (getAllowanceCmdIfNeeded wallet AllowanceFetched)
        ChainCmd.none
        []


updateWalletState : Wallet.State -> Model -> ( Model, Cmd Msg )
updateWalletState wallet model =
    ( { model | wallet = wallet }
    , getAllowanceCmdIfNeeded wallet AllowanceFetched
    )


getAllowanceCmdIfNeeded : Wallet.State -> (TokenFactoryType -> Result Http.Error BigInt -> Msg) -> Cmd Msg
getAllowanceCmdIfNeeded wallet msgConstructor =
    case ( Wallet.factory wallet, Wallet.userInfo wallet ) of
        ( Just (Token tokenType), Just userInfo ) ->
            Contracts.Wrappers.getAllowanceCmd
                tokenType
                userInfo.address
                (Config.factoryAddress (Token tokenType))
                (msgConstructor tokenType)

        _ ->
            Cmd.none


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Refresh time ->
            case prevModel.wallet of
                Wallet.Active userInfo ->
                    case Wallet.factory prevModel.wallet of
                        Just (Token tokenType) ->
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

                _ ->
                    justModelUpdate prevModel

        StartClicked factoryType tradeRecipe ->
            case factoryType of
                Token tokenType ->
                    let
                        newState =
                            case prevModel.tokenAllowance of
                                Just allowance ->
                                    if TokenValue.compare allowance tradeRecipe.daiAmountIn /= LT then
                                        Spec tradeRecipe ReadyToOpen

                                    else
                                        Menu (StartPrompt tokenType tradeRecipe)

                                Nothing ->
                                    Menu (StartPrompt tokenType tradeRecipe)
                    in
                    justModelUpdate { prevModel | state = newState }

                Native _ ->
                    justModelUpdate
                        { prevModel
                            | state =
                                Spec tradeRecipe ReadyToOpen
                        }

        ApproveClicked tokenType tradeRecipe ->
            let
                chainCmd =
                    approveChainCmd tokenType tradeRecipe.daiAmountIn
            in
            UpdateResult
                { prevModel
                    | state = Menu (ApproveNeedsSig tokenType tradeRecipe)
                }
                Cmd.none
                chainCmd
                []

        ApproveSigned tokenType txHashResult ->
            case ( txHashResult, prevModel.state ) of
                ( Ok txHash, Menu (ApproveNeedsSig _ tradeRecipe) ) ->
                    justModelUpdate
                        { prevModel | state = Spec tradeRecipe (ApproveMining txHash) }

                ( Err e, _ ) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3SigError "ERC20 approve" e ]

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.unexpectedError "Approve signed, but factoryType is not a token factory!" txHashResult ]

        AllowanceFetched tokenType fetchResult ->
            case ( fetchResult, prevModel.state ) of
                ( Ok tokenAllowance, Spec tradeRecipe (ApproveMining _) ) ->
                    if BigInt.compare tokenAllowance (TokenValue.getEvmValue tradeRecipe.daiAmountIn) /= LT then
                        justModelUpdate
                            { prevModel
                                | tokenAllowance = Just (TokenValue.tokenValue tokenAllowance)
                                , state =
                                    Spec tradeRecipe ReadyToOpen
                            }

                    else
                        justModelUpdate { prevModel | tokenAllowance = Just (TokenValue.tokenValue tokenAllowance) }

                ( Err httpError, _ ) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3FetchError "token allowance" httpError ]

                _ ->
                    justModelUpdate prevModel

        OpenClicked factoryType userInfo recipe ->
            let
                createParameters =
                    constructCreateParameters userInfo recipe prevModel.textInput

                chainCmd =
                    initiateCreateCall factoryType createParameters

                txChainStatus =
                    OpenNeedsSig
            in
            UpdateResult
                { prevModel
                    | state =
                        Spec
                            recipe
                            OpenNeedsSig
                }
                Cmd.none
                chainCmd
                []

        OpenSigned _ txHashResult ->
            case ( txHashResult, prevModel.state ) of
                ( Ok txHash, Spec recipe _ ) ->
                    justModelUpdate
                        { prevModel | state = Spec recipe OpenMining }

                ( Err e, _ ) ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3SigError "Open trade" e ]

                _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.unexpectedError "Open signed, but there is no recipe!" txHashResult ]

        OpenMined factory txReceiptResult ->
            case txReceiptResult of
                Ok txReceipt ->
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
                                [ AppCmd.GotoRoute (Routing.Trade factory id) ]

                        Nothing ->
                            UpdateResult
                                prevModel
                                Cmd.none
                                ChainCmd.none
                                [ AppCmd.UserNotice <|
                                    UN.unexpectedError "Error getting the ID of the created offer. Check the \"My Trades\" page for your open offer." txReceipt
                                ]

                Err e ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.web3MiningError "Open trade" e ]

        TextInputChanged newText ->
            justModelUpdate
                { prevModel | textInput = newText }

        ChangeState newState ->
            justModelUpdate { prevModel | state = newState }

        AbortCreate ->
            justModelUpdate
                { prevModel | state = Menu NoneStarted }

        NoOp ->
            justModelUpdate prevModel

        Web3Connect ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.Web3Connect ]


approveChainCmd : TokenFactoryType -> TokenValue -> ChainCmd Msg
approveChainCmd tokenType amount =
    let
        txParams =
            TokenContract.approve
                (Config.tokenContractAddress tokenType)
                (Config.factoryAddress (Token tokenType))
                (TokenValue.getEvmValue amount)
                |> Eth.toSend

        customSend =
            { onMined = Nothing
            , onSign = Just (ApproveSigned tokenType)
            , onBroadcast = Nothing
            }
    in
    ChainCmd.custom customSend txParams


initiateCreateCall : FactoryType -> CTypes.CreateParameters -> ChainCmd Msg
initiateCreateCall factoryType parameters =
    let
        txParams =
            Contracts.Wrappers.openTrade
                factoryType
                parameters
                |> Eth.toSend

        customSend =
            { onMined = Just ( OpenMined factoryType, Nothing )
            , onSign = Just (OpenSigned factoryType)
            , onBroadcast = Nothing
            }
    in
    ChainCmd.custom customSend txParams


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Refresh
