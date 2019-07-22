module QuickCreate.State exposing (init, subscriptions, update, updateUserInfo, updateWeb3Context)

import AppCmd
import BigInt exposing (BigInt)
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
import Helpers.ChainCmd as ChainCmd exposing (ChainCmd)
import Helpers.Eth as EthHelpers exposing (Web3Context)
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


init : EthHelpers.Web3Context -> Maybe UserInfo -> UpdateResult
init web3Context userInfo =
    let
        model =
            { web3Context = web3Context
            , userInfo = userInfo
            , state = Menu NoneStarted
            , tokenAllowance = Nothing
            , textInput = ""
            }

        cmd =
            case userInfo of
                Just uInfo ->
                    getAllowanceCmdIfNeeded model.web3Context uInfo AllowanceFetched

                Nothing ->
                    Cmd.none
    in
    UpdateResult
        model
        cmd
        ChainCmd.none
        []


updateUserInfo : Maybe UserInfo -> Model -> ( Model, Cmd Msg )
updateUserInfo maybeUserInfo model =
    ( { model | userInfo = maybeUserInfo }
    , case maybeUserInfo of
        Just userInfo ->
            getAllowanceCmdIfNeeded model.web3Context userInfo AllowanceFetched

        Nothing ->
            Cmd.none
    )


updateWeb3Context : EthHelpers.Web3Context -> Model -> Model
updateWeb3Context newWeb3Context model =
    { model | web3Context = newWeb3Context }


getAllowanceCmdIfNeeded : Web3Context -> UserInfo -> (Result Http.Error BigInt -> Msg) -> Cmd Msg
getAllowanceCmdIfNeeded web3Context userInfo msgConstructor =
    case web3Context.factoryType of
        Token tokenType ->
            Contracts.Wrappers.getAllowanceCmd
                web3Context
                tokenType
                userInfo.address
                (Config.factoryAddress web3Context.factoryType)
                msgConstructor

        _ ->
            Cmd.none


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

        StartClicked tradeRecipe ->
            case prevModel.web3Context.factoryType of
                Token _ ->
                    let
                        newState =
                            case prevModel.tokenAllowance of
                                Just allowance ->
                                    if TokenValue.compare allowance tradeRecipe.daiAmountIn /= LT then
                                        Spec tradeRecipe ReadyToOpen

                                    else
                                        Menu (StartPrompt tradeRecipe)

                                Nothing ->
                                    Menu (StartPrompt tradeRecipe)
                    in
                    justModelUpdate { prevModel | state = newState }

                Native _ ->
                    justModelUpdate
                        { prevModel
                            | state =
                                Spec tradeRecipe ReadyToOpen
                        }

        ApproveClicked tradeRecipe ->
            case prevModel.web3Context.factoryType of
                Token tokenType ->
                    let
                        chainCmd =
                            approveChainCmd tokenType tradeRecipe.daiAmountIn
                    in
                    UpdateResult
                        { prevModel
                            | state = Menu (ApproveNeedsSig tradeRecipe)
                        }
                        Cmd.none
                        chainCmd
                        []

                Native _ ->
                    UpdateResult
                        prevModel
                        Cmd.none
                        ChainCmd.none
                        [ AppCmd.UserNotice <| UN.unexpectedError "Approve Clicked msg received, but factoryType is not a token factory!" tradeRecipe ]

        ApproveSigned txHashResult ->
            case ( txHashResult, prevModel.state ) of
                ( Ok txHash, Menu (ApproveNeedsSig tradeRecipe) ) ->
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

        AllowanceFetched fetchResult ->
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

        OpenClicked userInfo recipe ->
            let
                createParameters =
                    constructCreateParameters userInfo recipe prevModel.textInput

                chainCmd =
                    initiateCreateCall prevModel.web3Context.factoryType createParameters

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

        OpenSigned txHashResult ->
            case ( txHashResult, prevModel.state ) of
                ( Ok txHash, Spec createParameters _ ) ->
                    justModelUpdate
                        { prevModel | state = Spec createParameters OpenMining }

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
                        [ AppCmd.UserNotice <| UN.unexpectedError "Open signed, but factoryType is not a token factory!" txHashResult ]

        OpenMined txReceiptResult ->
            case txReceiptResult of
                Ok txReceipt ->
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
            , onSign = Just ApproveSigned
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
            { onMined = Just ( OpenMined, Nothing )
            , onSign = Just OpenSigned
            , onBroadcast = Nothing
            }
    in
    ChainCmd.custom customSend txParams


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Refresh
