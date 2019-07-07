module Trade.View exposing (root)

import Array
import BigInt exposing (BigInt)
import Collage exposing (Collage)
import Collage.Render
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes exposing (FullTradeInfo)
import DateFormat
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import FiatValue exposing (FiatValue)
import Helpers.Element as EH
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Helpers.Time as TimeHelpers
import Images exposing (Image)
import Margin
import PaymentMethods exposing (PaymentMethod)
import Time
import TokenValue exposing (TokenValue)
import Trade.ChatHistory.View as ChatHistory
import Trade.Types exposing (..)
import TradeCache.Types exposing (TradeCache)


root : Time.Posix -> TradeCache -> Model -> Element.Element Msg
root time tradeCache model =
    case model.trade of
        CTypes.LoadedTrade tradeInfo ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 40
                , Element.inFront <| chatOverlayElement model
                , Element.inFront <| getModalOrNone model
                ]
                [ header time tradeInfo model.userInfo model.web3Context.factoryType tradeCache model.showStatsModal
                , Element.column
                    [ Element.width Element.fill
                    , Element.paddingXY 40 0
                    , Element.spacing 40
                    ]
                    [ phasesElement tradeInfo model.expandedPhase model.userInfo time
                    , PaymentMethods.viewList tradeInfo.terms.paymentMethods Nothing
                    ]
                ]

        CTypes.PartiallyLoadedTrade partialTradeInfo ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.size 30
                ]
                (Element.text "Loading contract info...")


header : Time.Posix -> FullTradeInfo -> Maybe UserInfo -> FactoryType -> TradeCache -> Bool -> Element Msg
header currentTime trade maybeUserInfo factoryType tradeCache showStatsModal =
    EH.niceFloatingRow
        [ tradeStatusElement trade factoryType
        , daiAmountElement trade maybeUserInfo
        , fiatElement trade
        , marginElement trade maybeUserInfo
        , statsElement trade tradeCache showStatsModal
        , case maybeUserInfo of
            Just userInfo ->
                actionButtonsElement currentTime trade userInfo

            Nothing ->
                Element.el
                    [ Element.width <| Element.px 200 ]
                <|
                    EH.maybeErrorElement
                        []
                        (Just "Can't find user address. Is Metamask unlocked?")
        ]


tradeStatusElement : FullTradeInfo -> FactoryType -> Element Msg
tradeStatusElement trade factoryType =
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
                        case trade.parameters.initiatorRole of
                            Buyer ->
                                "Open Buy Offer"

                            Seller ->
                                "Open Sell Offer"

                    CTypes.Committed ->
                        "Committed"

                    CTypes.Judgment ->
                        "Judgment"

                    CTypes.Closed ->
                        "Closed"
                )
            , EH.etherscanAddressLink
                [ Element.Font.size 12
                , Element.Font.color EH.blue
                , Element.Font.underline
                ]
                factoryType
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
        (case ( trade.parameters.initiatorRole, maybeInitiatorOrResponder ) of
            ( Buyer, Just Initiator ) ->
                "You're Buying"

            ( Buyer, _ ) ->
                "Buying"

            ( Seller, Just Initiator ) ->
                "You're Selling"

            ( Seller, _ ) ->
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
        (renderFiatAmount trade.terms.price)


renderFiatAmount : FiatValue -> Element Msg
renderFiatAmount fiatValue =
    Element.row
        [ Element.spacing 5 ]
        [ FiatValue.typeStringToSymbol fiatValue.fiatType
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text <| FiatValue.renderToStringFull fiatValue)
        ]


marginElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
marginElement trade maybeUserInfo =
    EH.withHeader
        "At Margin"
        (case trade.derived.margin of
            Just marginFloat ->
                EH.uncoloredMargin marginFloat

            Nothing ->
                EH.comingSoonMsg [] "Margin for non-USD currencies coming soon!"
        )


type alias Stats =
    { asRole : BuyerOrSeller
    , firstTrade : Maybe Time.Posix
    , numTrades : Int
    , numBurns : Int
    , numReleases : Int
    , numAborts : Int
    , amountBurned : TokenValue
    , amountReleased : TokenValue
    }


generateUserStats : TradeCache -> BuyerOrSeller -> Address -> Stats
generateUserStats tradeCache forRole userAddress =
    let
        fullTradesByUserAsRole =
            tradeCache.trades
                |> Array.toList
                |> List.filterMap
                    (\t ->
                        case t of
                            CTypes.LoadedTrade loadedT ->
                                Just loadedT

                            _ ->
                                Nothing
                    )
                |> List.filter
                    -- filter for trades that share the same Seller
                    (\t ->
                        CTypes.getBuyerOrSeller t userAddress == Just forRole
                    )

        talliedVals =
            fullTradesByUserAsRole
                |> List.foldl
                    (\trade tallied ->
                        case trade.state.closedReason of
                            CTypes.Released ->
                                { tallied
                                    | numReleases = tallied.numReleases + 1
                                    , amountReleased =
                                        TokenValue.add
                                            tallied.amountReleased
                                            trade.parameters.tradeAmount
                                }

                            CTypes.Burned ->
                                { tallied
                                    | numBurns = tallied.numBurns + 1
                                    , amountBurned =
                                        TokenValue.add
                                            tallied.amountBurned
                                            trade.parameters.tradeAmount
                                }

                            CTypes.Aborted ->
                                { tallied | numAborts = tallied.numAborts + 1 }

                            _ ->
                                tallied
                    )
                    { numBurns = 0
                    , numReleases = 0
                    , numAborts = 0
                    , amountBurned = TokenValue.zero
                    , amountReleased = TokenValue.zero
                    }

        numTrades =
            List.length fullTradesByUserAsRole

        firstTradeDate =
            fullTradesByUserAsRole
                |> List.filterMap
                    (\t ->
                        Time.posixToMillis t.phaseStartInfo.committedTime
                            |> (\millis ->
                                    if millis == 0 then
                                        Nothing

                                    else
                                        Just millis
                               )
                    )
                |> List.sort
                |> List.head
                |> Maybe.map Time.millisToPosix
    in
    { asRole = forRole
    , numTrades = numTrades
    , firstTrade = firstTradeDate
    , numBurns = talliedVals.numBurns
    , numReleases = talliedVals.numReleases
    , numAborts = talliedVals.numAborts
    , amountBurned = talliedVals.amountBurned
    , amountReleased = talliedVals.amountReleased
    }


statsElement : FullTradeInfo -> TradeCache -> Bool -> Element Msg
statsElement trade tradeCache showModal =
    let
        userStats =
            trade.parameters.initiatorAddress
                |> generateUserStats tradeCache trade.parameters.initiatorRole

        headerText =
            buyerOrSellerToString trade.parameters.initiatorRole
                ++ " Stats"
    in
    Element.el
        (if showModal then
            [ Element.below
                (Element.el
                    [ Element.moveDown 30 ]
                    (statsModal trade.parameters.initiatorAddress userStats)
                )
            ]

         else
            []
        )
    <|
        EH.withHeader
            headerText
            (Element.row
                [ Element.width Element.fill
                , Element.spacing 30
                , Element.pointer
                , Element.Events.onClick ToggleStatsModal
                ]
                [ Element.row
                    []
                    [ Images.toElement
                        [ Element.height <| Element.px 28
                        ]
                        Images.release
                    , Element.el
                        [ Element.Font.size 24
                        , Element.Font.medium
                        ]
                        (Element.text (String.padLeft 2 '0' <| String.fromInt userStats.numReleases))
                    ]
                , Element.row
                    []
                    [ Images.toElement
                        [ Element.height <| Element.px 28
                        ]
                        Images.flame
                    , Element.el
                        [ Element.Font.size 24
                        , Element.Font.medium
                        ]
                        (Element.text (String.padLeft 2 '0' <| String.fromInt userStats.numAborts))
                    ]
                , Element.row
                    []
                    [ Images.toElement
                        [ Element.height <| Element.px 28
                        ]
                        Images.flame
                    , Element.el
                        [ Element.Font.size 24
                        , Element.Font.medium
                        ]
                        (Element.text (String.padLeft 2 '0' <| String.fromInt userStats.numBurns))
                    ]
                ]
            )


statsModal : Address -> Stats -> Element Msg
statsModal address stats =
    let
        statEl titleString statString =
            Element.column
                [ Element.Font.size 18
                , Element.spacing 6
                ]
                [ Element.el
                    [ Element.Font.bold ]
                    (Element.text titleString)
                , Element.el
                    [ Element.Font.regular ]
                    (Element.text statString)
                ]

        statsBody =
            Element.column
                [ Element.spacing 23
                , Element.width Element.fill
                ]
                (List.map
                    (\( titleString, statString ) -> statEl titleString statString)
                    [ ( "First Trade"
                      , case stats.firstTrade of
                            Just date ->
                                dateFormatter
                                    Time.utc
                                    date

                            Nothing ->
                                "No Committed Trades yet!"
                      )
                    , ( "Release Outcomes"
                      , String.fromInt stats.numReleases
                            ++ " trades / "
                            ++ TokenValue.toConciseString stats.amountReleased
                            ++ " DAI Released"
                      )
                    , ( "Abort Outcomes"
                      , String.fromInt stats.numAborts
                            ++ " trades"
                      )
                    , ( "Burn Outcomes"
                      , String.fromInt stats.numBurns
                            ++ " trades / "
                            ++ TokenValue.toConciseString stats.amountBurned
                            ++ " DAI Burned"
                      )
                    ]
                    ++ [ Element.el [ Element.centerX ]
                            (EH.blueButton "View User History" (ViewUserHistory stats.asRole))
                       ]
                )

        dateFormatter =
            DateFormat.format
                [ DateFormat.monthNameFull
                , DateFormat.text ", "
                , DateFormat.yearNumber
                ]
    in
    Element.column
        [ Element.Border.rounded 8
        , Element.clipX
        , Element.clipY
        , Element.Background.color EH.lightGray
        , Element.spacing 1
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 20
            , color = Element.rgba 0 0 0 0.08
            }
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , Element.padding 17
            ]
            (EH.ethAddress 18 address)
        , Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , Element.padding 17
            ]
            statsBody
        ]


actionButtonsElement : Time.Posix -> FullTradeInfo -> UserInfo -> Element Msg
actionButtonsElement currentTime trade userInfo =
    case CTypes.getCurrentPhaseTimeoutInfo currentTime trade of
        CTypes.TimeUp _ ->
            Element.none

        CTypes.TimeLeft _ ->
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
                                    |> TokenValue.getEvmValue
                        in
                        [ EH.redButton "Deposit and Commit to Trade" <| CommitClicked trade userInfo depositAmount ]

                    ( CTypes.Committed, _, Just Buyer ) ->
                        [ Element.map StartContractAction <| EH.orangeButton "Abort Trade" Abort
                        , Element.map StartContractAction <| EH.redButton "Confirm Payment" Claim
                        ]

                    ( CTypes.Judgment, _, Just Seller ) ->
                        [ Element.map StartContractAction <| EH.redButton "Burn it All!" Burn
                        , Element.map StartContractAction <| EH.blueButton "Release Everything" Release
                        ]

                    _ ->
                        []
                )


phasesElement : FullTradeInfo -> CTypes.Phase -> Maybe UserInfo -> Time.Posix -> Element Msg
phasesElement trade expandedPhase maybeUserInfo currentTime =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.shrink
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
                        (Element.text <| "Trade " ++ closedReasonToText trade.state.closedReason)
                    )
                ]

            _ ->
                [ phaseElement CTypes.Open trade maybeUserInfo (expandedPhase == CTypes.Open) currentTime
                , phaseElement CTypes.Committed trade maybeUserInfo (expandedPhase == CTypes.Committed) currentTime
                , phaseElement CTypes.Judgment trade maybeUserInfo (expandedPhase == CTypes.Judgment) currentTime
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
    , Element.height (Element.shrink |> Element.minimum 360)
    ]


phaseState : CTypes.FullTradeInfo -> CTypes.Phase -> PhaseState
phaseState trade phase =
    let
        ( viewPhaseInt, activePhaseInt ) =
            ( CTypes.phaseToInt phase
            , CTypes.phaseToInt trade.state.phase
            )
    in
    if viewPhaseInt > activePhaseInt then
        NotStarted

    else if viewPhaseInt == activePhaseInt then
        Active

    else
        Finished


phaseElement : CTypes.Phase -> FullTradeInfo -> Maybe UserInfo -> Bool -> Time.Posix -> Element Msg
phaseElement viewPhase trade maybeUserInfo expanded currentTime =
    let
        viewPhaseState =
            phaseState trade viewPhase

        fullInterval =
            case viewPhase of
                CTypes.Open ->
                    trade.parameters.autorecallInterval

                CTypes.Committed ->
                    trade.parameters.autoabortInterval

                CTypes.Judgment ->
                    trade.parameters.autoreleaseInterval

                _ ->
                    Time.millisToPosix 0

        displayInterval =
            case viewPhaseState of
                NotStarted ->
                    fullInterval

                Active ->
                    TimeHelpers.sub
                        (TimeHelpers.add trade.state.phaseStartTime fullInterval)
                        currentTime

                Finished ->
                    Time.millisToPosix 0

        firstEl =
            phaseStatusElement
                viewPhase
                trade
                currentTime

        secondEl =
            Element.el
                [ Element.padding 30
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (phaseAdviceElement viewPhase trade maybeUserInfo)

        borderEl =
            Element.el
                [ Element.height Element.fill
                , Element.width <| Element.px 1
                , Element.Background.color <|
                    case viewPhaseState of
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
                ++ (if viewPhaseState == Active then
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
                ++ (if viewPhaseState == Active then
                        activePhaseAttributes

                    else
                        inactivePhaseAttributes
                   )
                ++ [ Element.pointer
                   , Element.Events.onClick <| ExpandPhase viewPhase
                   ]
            )
            [ firstEl ]


phaseStatusElement : CTypes.Phase -> CTypes.FullTradeInfo -> Time.Posix -> Element Msg
phaseStatusElement viewPhase trade currentTime =
    let
        viewPhaseState =
            phaseState trade viewPhase

        iconEl =
            phaseIconElement viewPhase viewPhaseState

        titleColor =
            case viewPhaseState of
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
                (Element.text <|
                    case viewPhase of
                        CTypes.Open ->
                            "Open Window"

                        CTypes.Committed ->
                            "Payment Window"

                        CTypes.Judgment ->
                            "Release Window"

                        CTypes.Closed ->
                            "Closed"
                )

        intervalElement =
            case viewPhase of
                CTypes.Closed ->
                    Element.none

                _ ->
                    case viewPhaseState of
                        NotStarted ->
                            EH.interval
                                [ Element.centerX ]
                                [ Element.Font.size 22, Element.Font.medium ]
                                ( EH.black, EH.lightGray )
                                (CTypes.getPhaseInterval viewPhase trade)

                        Active ->
                            case CTypes.getCurrentPhaseTimeoutInfo currentTime trade of
                                CTypes.TimeLeft timeoutInfo ->
                                    EH.intervalWithElapsedBar
                                        [ Element.centerX ]
                                        [ Element.Font.size 22, Element.Font.medium ]
                                        ( EH.white, EH.lightGray )
                                        timeoutInfo

                                CTypes.TimeUp _ ->
                                    Element.column
                                        [ Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ Element.el [ Element.centerX ] <| Element.text (CTypes.getPokeText viewPhase)
                                        , EH.blueButton "Poke" (StartContractAction Poke)
                                        ]

                        Finished ->
                            Element.el [ Element.height <| Element.px 1 ] Element.none

        phaseStateElement =
            Element.el
                [ Element.centerX
                , Element.Font.italic
                , Element.Font.semiBold
                , Element.Font.size 16
                ]
                (Element.text <| phaseStateString viewPhaseState)
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
            [ iconEl
            , titleElement
            , intervalElement
            , phaseStateElement
            ]


phaseIconElement : CTypes.Phase -> PhaseState -> Element Msg
phaseIconElement viewPhase viewPhaseState =
    let
        circleColor =
            case viewPhaseState of
                NotStarted ->
                    Element.rgb255 10 33 108

                Active ->
                    Element.rgb255 0 100 170

                Finished ->
                    Element.rgb255 1 129 104

        circleElement =
            Collage.circle 75
                |> Collage.filled (Collage.uniform (EH.elementColorToAvh4Color circleColor))
                |> Collage.Render.svg
                |> Element.html

        image =
            CTypes.phaseIcon viewPhase
    in
    Element.el
        [ Element.inFront
            (Images.toElement
                [ Element.centerX
                , Element.centerY
                ]
                image
            )
        ]
        circleElement


phaseStateString : PhaseState -> String
phaseStateString status =
    case status of
        NotStarted ->
            "Not Started"

        Active ->
            "Active"

        Finished ->
            "Finished"


phaseAdviceElement : CTypes.Phase -> CTypes.FullTradeInfo -> Maybe UserInfo -> Element Msg
phaseAdviceElement viewPhase trade maybeUserInfo =
    let
        phaseIsActive =
            viewPhase == trade.state.phase

        maybeBuyerOrSeller =
            maybeUserInfo
                |> Maybe.map .address
                |> Maybe.andThen (CTypes.getBuyerOrSeller trade)

        mainFontColor =
            if phaseIsActive then
                EH.white

            else
                EH.black

        makeParagraph =
            Element.paragraph
                [ Element.Font.color mainFontColor
                , Element.Font.size 18
                , Element.Font.semiBold
                ]

        emphasizedColor =
            if phaseIsActive then
                Element.rgb255 0 226 255

            else
                Element.rgb255 16 7 234

        emphasizedText =
            Element.el [ Element.Font.color emphasizedColor ] << Element.text

        scaryText =
            Element.el [ Element.Font.color <| Element.rgb 1 0 0 ] << Element.text

        tradeAmountString =
            TokenValue.toConciseString trade.parameters.tradeAmount ++ " DAI"

        fiatAmountString =
            FiatValue.renderToStringFull trade.terms.price

        buyerDepositString =
            TokenValue.toConciseString trade.parameters.buyerDeposit ++ " DAI"

        tradePlusDepositString =
            (TokenValue.add
                trade.parameters.tradeAmount
                trade.parameters.buyerDeposit
                |> TokenValue.toConciseString
            )
                ++ " DAI"

        abortPunishment =
            trade.parameters.abortPunishment

        abortPunishmentString =
            TokenValue.toConciseString
                abortPunishment
                ++ " DAI"

        sellerAbortRefundString =
            TokenValue.toConciseString
                (TokenValue.sub
                    trade.parameters.tradeAmount
                    abortPunishment
                )
                ++ " DAI"

        buyerAbortRefundString =
            TokenValue.toConciseString
                (TokenValue.sub
                    trade.parameters.buyerDeposit
                    abortPunishment
                )
                ++ " DAI"

        threeFlames =
            Element.row []
                (List.repeat 3 (Images.toElement [ Element.height <| Element.px 18 ] Images.flame))

        ( titleString, paragraphEls ) =
            case ( viewPhase, maybeBuyerOrSeller ) of
                ( CTypes.Open, Nothing ) ->
                    ( "Get it while it's hot"
                    , case trade.parameters.initiatorRole of
                        Seller ->
                            List.map makeParagraph
                                [ [ Element.text "The Seller has deposited "
                                  , emphasizedText tradeAmountString
                                  , Element.text " into this contract, and offers to sell it for "
                                  , emphasizedText fiatAmountString
                                  , Element.text ". To become the Buyer, you must deposit 1/3 of the trade amount "
                                  , emphasizedText <| "(" ++ buyerDepositString ++ ")"
                                  , Element.text " into this contract by clicking "
                                  , scaryText "Deposit and Commit to Trade"
                                  , Element.text "."
                                  ]
                                , [ Element.text "If the trade is successful, the combined DAI balance "
                                  , emphasizedText <| "(" ++ tradePlusDepositString ++ ")"
                                  , Element.text " will be released to you. If anything goes wrong, there are "
                                  , scaryText "burnable punishments "
                                  , threeFlames
                                  , Element.text " for both parties."
                                  ]
                                , [ Element.text "Don't commit unless you can fulfil one of the seller’s accepted payment methods below for "
                                  , emphasizedText fiatAmountString
                                  , Element.text " within the payment window."
                                  ]
                                ]

                        Buyer ->
                            List.map makeParagraph
                                [ [ Element.text "The Buyer is offering to buy "
                                  , emphasizedText tradeAmountString
                                  , Element.text " for "
                                  , emphasizedText fiatAmountString
                                  , Element.text ", and has deposited "
                                  , emphasizedText buyerDepositString
                                  , Element.text " into this contract as a "
                                  , scaryText "burnable deposit"
                                  , Element.text ". To become the Seller, deposit "
                                  , emphasizedText tradeAmountString
                                  , Element.text " into this contract by clicking "
                                  , scaryText "Deposit and Commit to Trade"
                                  , Element.text "."
                                  ]
                                , [ Element.text "When you receive the "
                                  , emphasizedText fiatAmountString
                                  , Element.text " from the Buyer, the combined DAI balance "
                                  , emphasizedText <| "(" ++ tradePlusDepositString ++ ")"
                                  , Element.text " will be released to the Buyer. If anything goes wrong, there are "
                                  , scaryText "burnable punishments "
                                  , threeFlames
                                  , Element.text " for both parties."
                                  ]
                                , [ Element.text "Don't commit unless you can receive "
                                  , emphasizedText fiatAmountString
                                  , Element.text " via one of the Buyer's payment methods below, within the payment window."
                                  ]
                                ]
                    )

                ( CTypes.Open, Just buyerOrSeller ) ->
                    ( "And Now, We Wait"
                    , case buyerOrSeller of
                        Buyer ->
                            List.map makeParagraph
                                [ [ Element.text "Your "
                                  , scaryText "burnable deposit"
                                  , Element.text " of "
                                  , emphasizedText buyerDepositString
                                  , Element.text " is now held in this contract, and your offer to buy "
                                  , emphasizedText tradeAmountString
                                  , Element.text " for "
                                  , emphasizedText fiatAmountString
                                  , Element.text " is now listed in the marketplace."
                                  ]
                                , [ Element.text "If another user likes your offer, they can become the Seller by depositing the full "
                                  , emphasizedText tradeAmountString
                                  , Element.text " into this contract."
                                  ]
                                , [ Element.text "If no one commits within the Open Window, your offer will expire, refunding the "
                                  , emphasizedText buyerDepositString
                                  , Element.text " to you."
                                  ]
                                ]

                        Seller ->
                            List.map makeParagraph
                                [ [ Element.text "Your offer to sell the "
                                  , emphasizedText tradeAmountString
                                  , Element.text " held in this contract for "
                                  , emphasizedText fiatAmountString
                                  , Element.text " is now listed in the marketplace."
                                  ]
                                , [ Element.text "If another user likes your offer, they can become the Buyer by depositing a "
                                  , scaryText "burnable deposit"
                                  , Element.text " of 1/3 of the trade amount "
                                  , emphasizedText <| "(" ++ buyerDepositString ++ ")"
                                  , Element.text " into this contract."
                                  ]
                                , [ Element.text "If no one commits within the Open Window, your offer will expire, refunding the "
                                  , emphasizedText tradeAmountString
                                  , Element.text " to you."
                                  ]
                                ]
                    )

                ( CTypes.Committed, Just Buyer ) ->
                    ( "Time to Pay Up"
                    , List.map makeParagraph
                        [ [ Element.text "You must now pay the Seller "
                          , emphasizedText fiatAmountString
                          , Element.text " via one of the accepted payment methods below, "
                          , Element.el [ Element.Font.semiBold ] <| Element.text "and then click "
                          , scaryText "Confirm Payment"
                          , Element.text " before the payment window runs out. Use the chat to coordinate."
                          ]
                        , [ Element.text "If you abort the trade, or do not confirm payment before this time is up, "
                          , emphasizedText abortPunishmentString
                          , Element.text " (1/4 of the "
                          , scaryText "burnable deposit"
                          , Element.text ") will be "
                          , scaryText "burned"
                          , Element.text " from both parties, while the remainder of each party's deposit is refunded ("
                          , emphasizedText sellerAbortRefundString
                          , Element.text " to the Seller, "
                          , emphasizedText buyerAbortRefundString
                          , Element.text " to you)."
                          ]
                        , [ Element.text "This may be your last chance to clear up any ambiguity before Judgement. Do not confirm unless you're sure the "
                          , emphasizedText fiatAmountString
                          , Element.text " has been unmistakably transferred."
                          ]
                        ]
                    )

                ( CTypes.Committed, Just Seller ) ->
                    ( "Time to Get Paid"
                    , List.map makeParagraph
                        [ [ Element.text "Work and communicate with the Buyer to receive "
                          , emphasizedText fiatAmountString
                          , Element.text ". Then, the Buyer should confirm the payment, moving the trade to the final phase."
                          ]
                        , [ Element.text "If the Buyer aborts the trade, or doesn't confirm payment before this time is up, "
                          , emphasizedText abortPunishmentString
                          , Element.text " (1/4 of the "
                          , scaryText "burnable deposit"
                          , Element.text ") will be "
                          , scaryText "burned"
                          , Element.text " from both parties, while the remainder of each party's deposit is refunded ("
                          , emphasizedText sellerAbortRefundString
                          , Element.text " to you, "
                          , emphasizedText buyerAbortRefundString
                          , Element.text " to the Buyer)."
                          ]
                        ]
                    )

                ( CTypes.Committed, Nothing ) ->
                    ( "Making the Payment"
                    , List.map makeParagraph
                        [ [ Element.text "During this phase, the Buyer is expected to transfer "
                          , emphasizedText fiatAmountString
                          , Element.text " to the Seller, via one of the payment methods listed below, "
                          , Element.el [ Element.Font.semiBold ] <| Element.text "and "
                          , scaryText "Confirm the Payment "
                          , Element.text " before the payment window runs out. This would move the trade to the final phase."
                          ]
                        , [ Element.text "If the Buyer aborts the trade, or doesn't confirm payment before this time is up, "
                          , emphasizedText abortPunishmentString
                          , Element.text " (1/4 of the "
                          , scaryText "burnable deposit"
                          , Element.text " amount) will be "
                          , scaryText "burned"
                          , Element.text " from both parties, while the remainder of each party's deposit is refunded ("
                          , emphasizedText sellerAbortRefundString
                          , Element.text " to the Seller, "
                          , emphasizedText buyerAbortRefundString
                          , Element.text " to the Buyer)."
                          ]
                        ]
                    )

                ( CTypes.Judgment, Just Buyer ) ->
                    ( "Judgement"
                    , List.map makeParagraph
                        [ [ Element.text "If the Seller confirms receipt of payment, or fails to decide within the release window, the combined balance of "
                          , emphasizedText tradePlusDepositString
                          , Element.text " will be released to you."
                          ]
                        , [ Element.text "If they cannot confirm they've received payment from you, they will probably instead "
                          , scaryText "burn the contract's balance of "
                          , emphasizedText tradePlusDepositString
                          , scaryText "."
                          , Element.text " In this case the "
                          , emphasizedText tradePlusDepositString
                          , Element.text " will be lost to both parties."
                          ]
                        , [ Element.text "These are the only options the Seller has. So, fingers crossed!"
                          ]
                        ]
                    )

                ( CTypes.Judgment, Just Seller ) ->
                    ( "Judgement"
                    , List.map makeParagraph
                        [ [ Element.text "By pushing the contract to the final stage, the Buyer has indicated that the transfer has taken place, and awaits your judgement."
                          ]
                        , [ Element.text "So, have you recieved the "
                          , emphasizedText fiatAmountString
                          , Element.text "? If so, you can click "
                          , emphasizedText "Release Everything"
                          , Element.text "."
                          ]
                        , [ Element.text "If not, the Buyer is probably trying to scam you, and you should probably "
                          , scaryText "burn it all"
                          , Element.text ". You're not getting it back either way, and you wouldn't want the other guy to get it, would you?"
                          ]
                        , [ Element.text "If you don't decide within the Release Window, the balance will be automatically released."
                          ]
                        ]
                    )

                ( CTypes.Judgment, Nothing ) ->
                    ( "Judgement"
                    , List.map makeParagraph
                        [ [ Element.text "The Buyer has indicated that the transfer has taken place, and awaits the Seller's judgement on the fact of the matter."
                          ]
                        , [ Element.text "If the Seller can verify he has received the "
                          , emphasizedText fiatAmountString
                          , Element.text ", he will probably release the total balance of "
                          , emphasizedText tradeAmountString
                          , Element.text " to the Buyer. If he cannot verify payment, he will probably instead "
                          , scaryText "burn it all"
                          , Element.text "."
                          ]
                        , [ Element.text "If the Seller has not made a decision before the Release Window expires, the "
                          , emphasizedText tradeAmountString
                          , Element.text " will be automaticall released."
                          ]
                        ]
                    )

                ( CTypes.Closed, Just _ ) ->
                    ( "Contract closed."
                    , [ makeParagraph [ Element.text "Check the chat log for the full history." ] ]
                    )

                ( CTypes.Closed, Nothing ) ->
                    ( "Contract closed."
                    , []
                    )
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 90 10
        , Element.spacing 16
        ]
        [ Element.el
            [ Element.Font.size 24
            , Element.Font.semiBold
            , Element.Font.color emphasizedColor
            ]
            (Element.text titleString)
        , Element.column
            [ Element.width Element.fill
            , Element.centerY
            , Element.spacing 13
            , Element.paddingEach
                { right = 40
                , top = 0
                , bottom = 0
                , left = 0
                }
            ]
            paragraphEls
        ]


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
        Nothing ->
            Element.none

        Just (ConfirmingCommit trade userInfo deposit) ->
            let
                depositAmountString =
                    TokenValue.tokenValue deposit
                        |> TokenValue.toConciseString

                fiatPriceString =
                    FiatValue.renderToStringFull trade.terms.price

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
                            ([ [ Element.text <| "You will deposit "
                               , Element.el [ Element.Font.color EH.blue ] <| Element.text <| depositAmountString ++ " DAI"
                               , Element.text ", thereby becoming the "
                               , buyerOrSellerEl
                               , Element.text " of this trade. By doing so, you are agreeing to "
                               ]
                                ++ agreeToWhatTextList
                             ]
                                ++ (case model.web3Context.factoryType of
                                        Token _ ->
                                            [ [ Element.text <| "(This ususally requires two Metamask signatures. Your DAI will not be deposited until the second transaction has been mined.)" ] ]

                                        _ ->
                                            []
                                   )
                            )
                        )
                    , Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        ]
                        (EH.redButton "Yes, I definitely want to commit to this trade." (ConfirmCommit trade userInfo deposit))
                    ]
                )
                AbortCommit

        Just ApproveNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the approve call."
                , Element.text "(check Metamask!)"
                , Element.text "Note that there will be a second transaction to sign after this."
                ]

        Just (ApproveMining txHash) ->
            EH.txProcessModal
                [ Element.text "Mining the initial approve transaction..."
                , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                    { url = EthHelpers.makeViewTxUrl model.web3Context.factoryType txHash
                    , label = Element.text "See the transaction on Etherscan"
                    }
                , Element.text "Funds will not leave your wallet until you sign the next transaction."
                ]

        Just CommitNeedsSig ->
            EH.txProcessModal
                [ Element.text "Waiting for user signature for the final commit call."
                , Element.text "(check Metamask!)"
                , Element.text "This will make the deposit and commit you to the trade."
                ]

        Just (CommitMining txHash) ->
            EH.txProcessModal
                [ Element.text "Mining the final commit transaction..."
                , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                    { url = EthHelpers.makeViewTxUrl model.web3Context.factoryType txHash
                    , label = Element.text "See the transaction"
                    }
                ]

        Just (ActionNeedsSig action) ->
            EH.txProcessModal
                [ Element.text <| "Waiting for user signature for the " ++ actionName action ++ " call."
                , Element.text "(check Metamask!)"
                ]

        Just (ActionMining action txHash) ->
            Element.none


closedReasonToText : CTypes.ClosedReason -> String
closedReasonToText reason =
    case reason of
        CTypes.NotClosed ->
            ""

        CTypes.Recalled ->
            "Recalled"

        CTypes.Aborted ->
            "Aborted"

        CTypes.Released ->
            "Released"

        CTypes.Burned ->
            "Burned"
