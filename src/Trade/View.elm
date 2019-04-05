module Trade.View exposing (root)

import Array
import CommonTypes exposing (..)
import Contracts.Types as CTypes exposing (FullTradeInfo)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Utils
import FiatValue exposing (FiatValue)
import Images exposing (Image)
import Margin
import Network exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import Trade.ChatHistory.View as ChatHistory
import Trade.Types exposing (..)


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    case model.trade of
        CTypes.LoadedTrade tradeInfo ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 40
                , Element.inFront <| chatOverlayElement model
                , Element.inFront <| getModalOrNone model
                ]
                [ header tradeInfo model.stats model.userInfo model.ethNode.network
                , Element.column
                    [ Element.width Element.fill
                    , Element.paddingXY 40 0
                    , Element.spacing 40
                    ]
                    [ phasesElement tradeInfo model.expandedPhase model.userInfo time
                    , PaymentMethods.viewList tradeInfo.paymentMethods Nothing
                    ]
                ]

        CTypes.PartiallyLoadedTrade partialTradeInfo ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.size 30
                ]
                (Element.text "Loading contract info...")


header : FullTradeInfo -> StatsModel -> Maybe UserInfo -> Network -> Element Msg
header trade stats maybeUserInfo network =
    EH.niceFloatingRow
        [ tradeStatusElement trade network
        , daiAmountElement trade maybeUserInfo
        , fiatElement trade
        , marginElement trade maybeUserInfo
        , statsElement stats
        , case maybeUserInfo of
            Just userInfo ->
                actionButtonsElement trade userInfo

            Nothing ->
                Element.none
        ]


tradeStatusElement : FullTradeInfo -> Network -> Element Msg
tradeStatusElement trade network =
    EH.withHeader
        "Trade Status"
        (Element.column
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.spacing 8
            ]
            [ Element.text
                (case trade.state.phase of
                    CTypes.Open ->
                        case trade.parameters.openMode of
                            CTypes.BuyerOpened ->
                                "Open Buy Offer"

                            CTypes.SellerOpened ->
                                "Open Sell Offer"

                    CTypes.Committed ->
                        "Committed"

                    CTypes.Claimed ->
                        "Claimed"

                    CTypes.Closed ->
                        "Closed"
                )
            , EH.etherscanAddressLink
                [ Element.Font.size 12
                , Element.Font.color EH.blue
                , Element.Font.underline
                ]
                network
                trade.creationInfo.address
            ]
        )


daiAmountElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
daiAmountElement trade maybeUserInfo =
    let
        maybeInitiatorOrResponder =
            Maybe.andThen
                (CTypes.getInitiatorOrResponder trade)
                (Maybe.map .address maybeUserInfo)
    in
    EH.withHeader
        (case ( trade.parameters.openMode, maybeInitiatorOrResponder ) of
            ( CTypes.BuyerOpened, Just Initiator ) ->
                "You're Buying"

            ( CTypes.BuyerOpened, _ ) ->
                "Buying"

            ( CTypes.SellerOpened, Just Initiator ) ->
                "You're Selling"

            ( CTypes.SellerOpened, _ ) ->
                "Selling"
        )
        (renderDaiAmount trade.parameters.tradeAmount)


renderDaiAmount : TokenValue -> Element Msg
renderDaiAmount daiAmount =
    Element.row
        [ Element.spacing 8 ]
        [ Images.toElement [] Images.daiSymbol
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text <| TokenValue.toConciseString daiAmount)
        ]


fiatElement : FullTradeInfo -> Element Msg
fiatElement trade =
    EH.withHeader
        "For Fiat"
        (renderFiatAmount trade.parameters.fiatPrice)


renderFiatAmount : FiatValue -> Element Msg
renderFiatAmount fiatValue =
    Element.row
        [ Element.spacing 5 ]
        [ FiatValue.typeStringToSymbol fiatValue.fiatType
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text <| FiatValue.renderToString fiatValue)
        ]


marginElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
marginElement trade maybeUserInfo =
    EH.withHeader
        "At Margin"
        (case trade.derived.margin of
            Just marginFloat ->
                renderMargin marginFloat maybeUserInfo

            Nothing ->
                EH.comingSoonMsg [] "Margin for non-USD currencies coming soon!"
        )


renderMargin : Float -> Maybe UserInfo -> Element Msg
renderMargin marginFloat maybeUserInfo =
    let
        marginString =
            Margin.marginToString marginFloat ++ "%"

        image =
            if marginFloat == 0 then
                Images.none

            else
                Images.marginSymbol (marginFloat > 0) Nothing
    in
    Element.row [ Element.spacing 5 ]
        [ Element.text marginString
        , Images.toElement [] image
        ]


statsElement : StatsModel -> Element Msg
statsElement stats =
    EH.withHeader
        "Initiator Stats"
        (EH.comingSoonMsg [] "Stats coming soon!")


actionButtonsElement : FullTradeInfo -> UserInfo -> Element Msg
actionButtonsElement trade userInfo =
    Element.row
        [ Element.spacing 8 ]
        (case
            ( trade.state.phase
            , CTypes.getInitiatorOrResponder trade userInfo.address
            , CTypes.getBuyerOrSeller trade userInfo.address
            )
         of
            ( CTypes.Open, Just Initiator, _ ) ->
                [ Element.map StartContractAction <| EH.blueButton "Remove and Refund this Trade" Recall ]

            ( CTypes.Open, Nothing, _ ) ->
                let
                    depositAmount =
                        CTypes.responderDeposit trade.parameters
                            |> TokenValue.getBigInt
                in
                [ EH.redButton "Deposit and Commit to Trade" <| CommitClicked trade userInfo depositAmount ]

            ( CTypes.Committed, _, Just Buyer ) ->
                [ Element.map StartContractAction <| EH.orangeButton "Abort Trade" Abort
                , Element.map StartContractAction <| EH.redButton "I Confirm I have Sent Payment" Claim
                ]

            ( CTypes.Claimed, _, Just Seller ) ->
                [ Element.map StartContractAction <| EH.redButton "Burn it all" Burn
                , Element.map StartContractAction <| EH.blueButton "Release Everything Now" Release
                ]

            _ ->
                []
        )


phasesElement : FullTradeInfo -> CTypes.Phase -> Maybe UserInfo -> Time.Posix -> Element Msg
phasesElement trade expandedPhase maybeUserInfo currentTime =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 360
        , Element.spacing 20
        ]
    <|
        case trade.state.phase of
            CTypes.Closed ->
                [ Element.el
                    (commonPhaseAttributes
                        ++ [ Element.width Element.fill
                           , Element.Background.color EH.activePhaseBackgroundColor
                           ]
                    )
                    (Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Element.Font.size 20
                        , Element.Font.semiBold
                        , Element.Font.color EH.white
                        ]
                        (Element.text "Trade Closed")
                    )
                ]

            _ ->
                [ phaseElement CTypes.Open trade maybeUserInfo (expandedPhase == CTypes.Open) currentTime
                , phaseElement CTypes.Committed trade maybeUserInfo (expandedPhase == CTypes.Committed) currentTime
                , phaseElement CTypes.Claimed trade maybeUserInfo (expandedPhase == CTypes.Claimed) currentTime
                ]


activePhaseAttributes =
    [ Element.Background.color EH.activePhaseBackgroundColor
    , Element.Font.color EH.white
    ]


inactivePhaseAttributes =
    [ Element.Background.color EH.white
    ]


commonPhaseAttributes =
    [ Element.Border.rounded 12
    , Element.height Element.fill
    ]


phaseElement : CTypes.Phase -> FullTradeInfo -> Maybe UserInfo -> Bool -> Time.Posix -> Element Msg
phaseElement viewPhase trade maybeUserInfo expanded currentTime =
    let
        ( viewPhaseInt, tradePhaseInt ) =
            ( CTypes.phaseToInt viewPhase
            , CTypes.phaseToInt trade.state.phase
            )

        phaseState =
            if viewPhaseInt > tradePhaseInt then
                NotStarted

            else if viewPhaseInt == tradePhaseInt then
                Active

            else
                Finished

        fullInterval =
            case viewPhase of
                CTypes.Open ->
                    trade.parameters.autorecallInterval

                CTypes.Committed ->
                    trade.parameters.autoabortInterval

                CTypes.Claimed ->
                    trade.parameters.autoreleaseInterval

                _ ->
                    Time.millisToPosix 0

        displayInterval =
            case phaseState of
                NotStarted ->
                    fullInterval

                Active ->
                    TimeHelpers.sub
                        (TimeHelpers.add trade.state.phaseStartTime fullInterval)
                        currentTime

                Finished ->
                    Time.millisToPosix 0

        titleElement =
            case viewPhase of
                CTypes.Open ->
                    "Open Window"

                CTypes.Committed ->
                    "Payment Window"

                CTypes.Claimed ->
                    "Release Window"

                CTypes.Closed ->
                    "Closed"

        firstEl =
            Element.el
                [ Element.padding 30 ]
            <|
                phaseStatusElement
                    Images.none
                    titleElement
                    displayInterval
                    phaseState

        secondEl =
            Element.el
                [ Element.padding 30
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (EH.comingSoonMsg [ Element.Font.size 16 ] "Phase description coming soon!")

        borderEl =
            Element.el
                [ Element.height Element.fill
                , Element.width <| Element.px 1
                , Element.Background.color <|
                    case phaseState of
                        Active ->
                            Element.rgb 0 0 1

                        _ ->
                            EH.lightGray
                ]
                Element.none
    in
    if expanded then
        Element.row
            (commonPhaseAttributes
                ++ (if phaseState == Active then
                        activePhaseAttributes

                    else
                        inactivePhaseAttributes
                   )
                ++ [ Element.width Element.fill ]
            )
            [ firstEl, borderEl, secondEl ]

    else
        Element.row
            (commonPhaseAttributes
                ++ (if phaseState == Active then
                        activePhaseAttributes

                    else
                        inactivePhaseAttributes
                   )
                ++ [ Element.pointer
                   , Element.Events.onClick <| ExpandPhase viewPhase
                   ]
            )
            [ firstEl ]


phaseStatusElement : Image -> String -> Time.Posix -> PhaseState -> Element Msg
phaseStatusElement icon title interval phaseState =
    let
        titleColor =
            case phaseState of
                Active ->
                    Element.rgb255 0 226 255

                _ ->
                    EH.black

        titleElement =
            Element.el
                [ Element.Font.color titleColor
                , Element.Font.size 20
                , Element.Font.semiBold
                , Element.centerX
                ]
                (Element.text title)

        intervalElement =
            Element.el [ Element.centerX ]
                (EH.interval False Nothing interval)

        phaseStateElement =
            Element.el
                [ Element.centerX
                , Element.Font.italic
                , Element.Font.semiBold
                , Element.Font.size 16
                ]
                (Element.text <| phaseStateString phaseState)
    in
    Element.el
        [ Element.height <| Element.px 360
        , Element.width <| Element.px 270
        , Element.padding 30
        ]
    <|
        Element.column
            [ Element.centerX
            , Element.height Element.fill
            , Element.spaceEvenly
            ]
            [ Element.none -- add icon!
            , titleElement
            , intervalElement
            , phaseStateElement
            ]


phaseStateString : PhaseState -> String
phaseStateString status =
    case status of
        NotStarted ->
            "Not Started"

        Active ->
            "Active"

        Finished ->
            "Finished"


chatOverlayElement : Model -> Element Msg
chatOverlayElement model =
    case ( model.userInfo, model.trade ) of
        ( Just userInfo, CTypes.LoadedTrade trade ) ->
            if trade.state.phase == CTypes.Open then
                Element.none

            else if CTypes.getInitiatorOrResponder trade userInfo.address == Nothing then
                Element.none

            else
                let
                    openChatButton =
                        EH.elOnCircle
                            [ Element.pointer
                            , Element.Events.onClick ToggleChat
                            ]
                            80
                            (Element.rgb 1 1 1)
                            (Images.toElement
                                [ Element.centerX
                                , Element.centerY
                                , Element.moveRight 5
                                ]
                                Images.chatIcon
                            )

                    chatWindow =
                        Maybe.map
                            ChatHistory.window
                            model.chatHistoryModel
                            |> Maybe.withDefault Element.none
                in
                if model.showChatHistory then
                    EH.modal
                        (Element.rgba 0 0 0 0.6)
                        (Element.row
                            [ Element.height Element.fill
                            , Element.spacing 50
                            , Element.alignRight
                            ]
                            [ Element.map ChatHistoryMsg chatWindow
                            , Element.el [ Element.alignBottom ] openChatButton
                            ]
                        )

                else
                    Element.el
                        [ Element.alignRight
                        , Element.alignBottom
                        ]
                        openChatButton

        _ ->
            Element.none


getModalOrNone : Model -> Element Msg
getModalOrNone model =
    case model.txChainStatus of
        NoTx ->
            Element.none

        ConfirmingCommit trade userInfo deposit ->
            let
                depositAmountEl =
                    TokenValue.tokenValue tokenDecimals deposit
                        |> TokenValue.toConciseString
                        |> Element.text

                fiatPriceString =
                    FiatValue.renderToStringFull trade.parameters.fiatPrice

                daiAmountString =
                    TokenValue.toConciseString trade.parameters.tradeAmount ++ " DAI"

                ( buyerOrSellerEl, agreeToWhatTextList ) =
                    case CTypes.getResponderRole trade.parameters of
                        Buyer ->
                            ( Element.el [ Element.Font.medium, Element.Font.color EH.black ] <| Element.text "buyer"
                            , [ Element.text "pay the seller "
                              , Element.el [ Element.Font.color EH.blue ] <| Element.text fiatPriceString
                              , Element.text " in exchange for the "
                              , Element.el [ Element.Font.color EH.blue ] <| Element.text daiAmountString
                              , Element.text " held in this contract."
                              ]
                            )

                        Seller ->
                            ( Element.el [ Element.Font.medium, Element.Font.color EH.black ] <| Element.text "seller"
                            , [ Element.text "accept "
                              , Element.el [ Element.Font.color EH.blue ] <| Element.text fiatPriceString
                              , Element.text " from the buyer in exchange for the "
                              , Element.el [ Element.Font.color EH.blue ] <| Element.text daiAmountString
                              , Element.text " held in this contract."
                              ]
                            )
            in
            EH.closeableModal
                (Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    , Element.height Element.fill
                    , Element.Font.center
                    ]
                    [ Element.el
                        [ Element.Font.size 26
                        , Element.Font.semiBold
                        , Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Just to Confirm...")
                    , Element.column
                        [ Element.spacing 20
                        , Element.centerX
                        , Element.centerY
                        ]
                        (List.map
                            (Element.paragraph
                                [ Element.width <| Element.px 500
                                , Element.centerX
                                , Element.Font.size 18
                                , Element.Font.medium
                                , Element.Font.color EH.permanentTextColor
                                ]
                            )
                            [ [ Element.text <| "You will deposit "
                              , depositAmountEl
                              , Element.el [ Element.Font.color EH.blue ] <| Element.text " DAI, thereby becoming the "
                              , buyerOrSellerEl
                              , Element.text " of this trade. By doing so, you are agreeing to "
                              ]
                                ++ agreeToWhatTextList
                            , [ Element.text <| "(This ususally requires two Metamask signatures. Your DAI will not be deposited until the second transaction has been mined.)" ]
                            ]
                        )
                    , Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        ]
                        (EH.redButton "Yes, I definitely want to commit to this trade." (ConfirmCommit trade userInfo deposit))
                    ]
                )
                AbortCommit

        ApproveNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the approve call."
                , Element.text "(check Metamask!)"
                , Element.text "Note that there will be a second transaction to sign after this."
                ]

        ApproveMining txHash ->
            EH.txProcessModal
                [ Element.text "Mining the initial approve transaction..."

                -- , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                --     { url = EthHelpers.makeEtherscanTxUrl txHash
                --     , label = Element.text "See the transaction on Etherscan"
                --     }
                , Element.text "Funds will not be sent until you sign the next transaction."
                ]

        CommitNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the final commit call."
                , Element.text "(check Metamask!)"
                , Element.text "This will make the deposit and commit you to the trade."
                ]

        CommitMining txHash ->
            EH.txProcessModal
                [ Element.text "Mining the final commit transaction..."

                -- , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                --     { url = EthHelpers.makeEtherscanTxUrl txHash
                --     , label = Element.text "See the transaction on Etherscan"
                --     }
                ]

        ActionNeedsSig action ->
            EH.txProcessModal
                [ Element.text <| "Waiting for user signature for the " ++ actionName action ++ " call."
                , Element.text "(check Metamask!)"
                ]

        ActionMining action txHash ->
            Element.none

        TxError s ->
            EH.txProcessModal
                [ Element.text "Something has gone terribly wrong"
                , Element.text s
                ]


actionName : ContractAction -> String
actionName action =
    case action of
        Poke ->
            "poke"

        Recall ->
            "recall"

        Claim ->
            "claim"

        Abort ->
            "abort"

        Release ->
            "release"

        Burn ->
            "burn"
