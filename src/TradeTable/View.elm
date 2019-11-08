module TradeTable.View exposing (view)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Flip exposing (flip)
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Images exposing (Image)
import PaymentMethods exposing (PaymentMethod)
import PriceFetch
import ResponderProfit
import Time
import TokenValue exposing (TokenValue)
import TradeTable.Types exposing (..)


view : Time.Posix -> DisplayProfile -> Model -> List ( Currencies.Symbol, PriceFetch.PriceData ) -> List ColType -> List CTypes.FullTradeInfo -> Element Msg
view time dProfile model prices colTypes trades =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
        [ viewColHeaders model.orderBy colTypes
        , viewTradeRows time dProfile model prices colTypes trades
        ]


viewColHeaders : ( ColType, Ordering ) -> List ColType -> Element Msg
viewColHeaders orderBy colTypes =
    Element.row [ Element.width Element.fill ]
        (colTypes
            |> List.map
                (\colType ->
                    let
                        maybeOrdering =
                            if Tuple.first orderBy == colType then
                                Just <| Tuple.second orderBy

                            else
                                Nothing
                    in
                    viewColHeader maybeOrdering colType
                )
        )


viewColHeader : Maybe Ordering -> ColType -> Element Msg
viewColHeader maybeOrdering colType =
    cellMaker (colTypePortion colType) 60 (sortableColumnHeader colType maybeOrdering)


colTypePortion : ColType -> Int
colTypePortion colType =
    case colType of
        Phase ->
            1

        Expires ->
            1

        Offer ->
            1

        Windows ->
            1

        ResponderProfit ->
            1

        PaymentWindow ->
            2

        BurnWindow ->
            2


sortableColumnHeader : ColType -> Maybe Ordering -> Element Msg
sortableColumnHeader colType maybeOrdering =
    Element.row
        [ Element.spacing 5
        , Element.pointer
        , Element.Events.onClick (ChangeSort colType)
        ]
        [ colTitleEl colType
        , case maybeOrdering of
            Just ordering ->
                Element.el
                    [ Element.centerY ]
                    (Images.toElement
                        [ Element.width <| Element.px 8 ]
                        (case ordering of
                            Ascending ->
                                Images.downArrow

                            Descending ->
                                Images.upArrow
                        )
                    )

            Nothing ->
                Element.none
        ]


colTitleEl : ColType -> Element Msg
colTitleEl colType =
    Element.el [ Element.Font.medium, Element.Font.size 17 ] <|
        Element.text <|
            case colType of
                Phase ->
                    "Phase"

                Expires ->
                    "Expiring"

                Offer ->
                    "Offer"

                Windows ->
                    "Phase Times"

                ResponderProfit ->
                    "Responder Profit"

                PaymentWindow ->
                    "Payment Window"

                BurnWindow ->
                    "Burn Window"


viewTradeRows : Time.Posix -> DisplayProfile -> Model -> List ( Currencies.Symbol, PriceFetch.PriceData ) -> List ColType -> List CTypes.FullTradeInfo -> Element Msg
viewTradeRows time dProfile model prices colTypes trades =
    Element.column
        [ Element.width Element.fill
        , Element.Border.width 2
        , Element.Border.rounded 8
        , Element.Border.color EH.darkGray
        , Element.spacing 2
        , Element.Background.color EH.darkGray
        , Element.clip
        ]
        (trades
            |> List.sortWith (sortByFunc prices model.orderBy)
            |> List.map (viewTradeRow time dProfile prices colTypes)
        )


viewTradeRow : Time.Posix -> DisplayProfile -> List ( Currencies.Symbol, PriceFetch.PriceData ) -> List ColType -> CTypes.FullTradeInfo -> Element Msg
viewTradeRow time dProfile prices colTypes trade =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 1
        , Element.Background.color EH.white
        , Element.pointer
        , Element.Events.onClick (TradeClicked trade.reference)
        ]
        [ Element.row
            [ Element.width <| Element.fill
            , Element.spacing 1
            ]
            (colTypes
                |> List.map
                    (\colType ->
                        viewTradeCell time dProfile prices colType trade
                    )
            )
        , cellMaker 1 80 <| viewPaymentMethods trade.terms.paymentMethods
        ]


viewPaymentMethods : List PaymentMethod -> Element Msg
viewPaymentMethods paymentMethods =
    paymentMethods
        |> List.head
        |> Maybe.map PaymentMethods.previewTextHack
        |> Maybe.withDefault Element.none


viewTradeCell : Time.Posix -> DisplayProfile -> List ( Currencies.Symbol, PriceFetch.PriceData ) -> ColType -> CTypes.FullTradeInfo -> Element Msg
viewTradeCell time dProfile prices colType trade =
    cellMaker
        (colTypePortion colType)
        (60 |> changeForMobile 90 dProfile)
        (case colType of
            Phase ->
                let
                    phaseTitle =
                        CTypes.phaseToString trade.state.phase
                in
                case ( CTypes.getCurrentPhaseTimeoutInfo time trade, trade.state.phase ) of
                    ( _, CTypes.Closed ) ->
                        Element.text phaseTitle

                    ( CTypes.TimeLeft timeoutInfo, _ ) ->
                        let
                            baseIntervalColor =
                                if TimeHelpers.getRatio (Tuple.first timeoutInfo) (Tuple.second timeoutInfo) < 0.05 then
                                    EH.softRed

                                else
                                    EH.black
                        in
                        Element.column
                            [ Element.spacing 3 ]
                            [ Element.text phaseTitle
                            , EH.intervalWithElapsedBar
                                [ Element.width Element.fill ]
                                [ Element.Font.size 16 ]
                                ( baseIntervalColor, EH.lightGray )
                                timeoutInfo
                            ]

                    ( CTypes.TimeUp totalInterval, _ ) ->
                        Element.row
                            [ Element.spacing 6
                            , Element.Font.color EH.darkGray
                            ]
                            [ Element.text phaseTitle
                            , Element.el [ Element.Font.size 16 ] <| Element.text "(stale)"
                            ]

            Expires ->
                case trade.state.phase of
                    CTypes.Open ->
                        case CTypes.getCurrentPhaseTimeoutInfo time trade of
                            CTypes.TimeLeft timeoutInfo ->
                                let
                                    baseIntervalColor =
                                        if TimeHelpers.getRatio (Tuple.first timeoutInfo) (Tuple.second timeoutInfo) < 0.05 then
                                            EH.softRed

                                        else
                                            EH.black
                                in
                                EH.intervalWithElapsedBar
                                    [ Element.width Element.fill ]
                                    [ Element.Font.size 16 ]
                                    ( baseIntervalColor, EH.lightGray )
                                    timeoutInfo

                            CTypes.TimeUp totalInterval ->
                                EH.intervalWithElapsedBar
                                    [ Element.width Element.fill ]
                                    [ Element.Font.size 16 ]
                                    ( EH.softRed, EH.lightGray )
                                    ( Time.millisToPosix 0, totalInterval )

                    _ ->
                        Element.none

            Offer ->
                (Element.column |> changeForMobile Element.column dProfile)
                    [ Element.spacing 5
                    , Element.Font.size 18
                    ]
                    [ Element.row
                        [ Element.spacing 5 ]
                        [ Element.text <|
                            ((case trade.parameters.initiatorRole of
                                Buyer ->
                                    "Buying "

                                Seller ->
                                    "Selling "
                             )
                                ++ TokenValue.toConciseString trade.parameters.tradeAmount
                                ++ " "
                                ++ tokenUnitName trade.reference.factory
                            )
                        ]
                    , Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "for"
                        , EH.price trade.terms.price
                        ]
                    ]

            ResponderProfit ->
                ResponderProfit.calculate prices trade
                    |> Maybe.map (EH.coloredResponderProfit True)
                    |> Maybe.withDefault Element.none

            Windows ->
                let
                    timeLeftIntervalOrTotalTime : CTypes.Phase -> Element Msg
                    timeLeftIntervalOrTotalTime phase =
                        if trade.state.phase == phase then
                            case CTypes.getCurrentPhaseTimeoutInfo time trade of
                                CTypes.TimeLeft timeoutInfo ->
                                    let
                                        baseIntervalColor =
                                            if TimeHelpers.getRatio (Tuple.first timeoutInfo) (Tuple.second timeoutInfo) < 0.05 then
                                                EH.softRed

                                            else
                                                EH.black
                                    in
                                    EH.intervalWithElapsedBar
                                        [ Element.width Element.fill ]
                                        [ Element.Font.size 16
                                        , Element.Font.color EH.blue
                                        ]
                                        ( baseIntervalColor, EH.lightGray )
                                        timeoutInfo

                                CTypes.TimeUp totalInterval ->
                                    EH.intervalWithElapsedBar
                                        [ Element.width Element.fill ]
                                        [ Element.Font.size 16 ]
                                        ( EH.softRed, EH.lightGray )
                                        ( Time.millisToPosix 0, totalInterval )

                        else
                            EH.interval [] [ Element.Font.size 16 ] ( EH.black, EH.lightGray ) <|
                                case phase of
                                    CTypes.Open ->
                                        trade.parameters.autorecallInterval

                                    CTypes.Committed ->
                                        trade.parameters.autoabortInterval

                                    CTypes.Judgment ->
                                        trade.parameters.autoreleaseInterval

                                    CTypes.Closed ->
                                        Time.millisToPosix 0
                in
                Element.column
                    [ Element.spacing 5 ]
                    ([ CTypes.Open, CTypes.Committed, CTypes.Judgment ]
                        |> List.map
                            (\phase ->
                                Element.row
                                    [ Element.spacing 5 ]
                                    [ Images.toElement [ Element.height <| Element.px 20 ] <|
                                        CTypes.phaseIconBlack phase
                                    , timeLeftIntervalOrTotalTime phase
                                    ]
                            )
                    )

            PaymentWindow ->
                let
                    lowValColor =
                        case trade.parameters.initiatorRole of
                            Seller ->
                                EH.softRed

                            Buyer ->
                                EH.green

                    baseColor =
                        if Time.posixToMillis trade.parameters.autoabortInterval < (1000 * 60 * 60 * 6) then
                            lowValColor

                        else
                            EH.black
                in
                EH.interval
                    []
                    []
                    ( baseColor, EH.lightGray )
                    trade.parameters.autoabortInterval

            BurnWindow ->
                let
                    lowValColor =
                        case trade.parameters.initiatorRole of
                            Seller ->
                                EH.green

                            Buyer ->
                                EH.softRed

                    baseColor =
                        if Time.posixToMillis trade.parameters.autoabortInterval < (1000 * 60 * 60 * 6) then
                            lowValColor

                        else
                            EH.black
                in
                EH.interval
                    []
                    []
                    ( baseColor, EH.lightGray )
                    trade.parameters.autoreleaseInterval
        )


cellMaker : Int -> Int -> Element Msg -> Element Msg
cellMaker portion height cellElement =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height <| Element.px height
        , Element.clip
        ]
    <|
        Element.el
            [ Element.padding 12
            , Element.centerY
            , Element.width Element.fill
            ]
            cellElement


sortByFunc : List ( Currencies.Symbol, PriceFetch.PriceData ) -> ( ColType, Ordering ) -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
sortByFunc prices ( sortCol, ordering ) =
    (case sortCol of
        Phase ->
            \a b ->
                if a.state.phase == b.state.phase then
                    sortByFunc prices ( Expires, Descending ) a b

                else
                    compare (CTypes.phaseToInt a.state.phase) (CTypes.phaseToInt b.state.phase)

        Expires ->
            \a b -> TimeHelpers.compare a.derived.phaseEndTime b.derived.phaseEndTime

        Offer ->
            \a b -> TokenValue.compare a.parameters.tradeAmount b.parameters.tradeAmount

        Windows ->
            \a b ->
                TimeHelpers.compare
                    (TimeHelpers.add a.parameters.autoabortInterval a.parameters.autoreleaseInterval)
                    (TimeHelpers.add b.parameters.autoabortInterval b.parameters.autoreleaseInterval)

        ResponderProfit ->
            \a b ->
                Maybe.map2
                    compare
                    (ResponderProfit.calculate prices a)
                    (ResponderProfit.calculate prices b)
                    |> Maybe.withDefault EQ

        PaymentWindow ->
            \a b -> TimeHelpers.compare a.parameters.autoabortInterval b.parameters.autoabortInterval

        BurnWindow ->
            \a b ->
                TimeHelpers.compare a.parameters.autoreleaseInterval b.parameters.autoreleaseInterval
    )
        |> (if ordering == Ascending then
                identity

            else
                flip
           )
