module TradeTable.View exposing (view)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Images exposing (Image)
import PaymentMethods exposing (PaymentMethod)
import Time
import TokenValue exposing (TokenValue)
import TradeTable.Types exposing (..)


view : Time.Posix -> Model -> List ColType -> List CTypes.FullTradeInfo -> Element Msg
view time model colTypes trades =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        , Element.spacing 5
        ]
        [ viewColHeaders model.orderBy colTypes
        , viewTradeRows time model colTypes trades
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
    cellMaker (colTypePortion colType) (sortableColumnHeader colType maybeOrdering)


colTypePortion : ColType -> Int
colTypePortion colType =
    case colType of
        Phase ->
            1

        Expires ->
            2

        Offer ->
            1

        FiatPrice ->
            2

        Margin ->
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
                                Images.upArrow

                            Descending ->
                                Images.downArrow
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

                FiatPrice ->
                    "For Fiat"

                Margin ->
                    "Margin"

                PaymentWindow ->
                    "Payment Window"

                BurnWindow ->
                    "Burn Window"


viewTradeRows : Time.Posix -> Model -> List ColType -> List CTypes.FullTradeInfo -> Element Msg
viewTradeRows time model colTypes trades =
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
            |> List.sortWith (sortByFunc model.orderBy)
            |> List.map (viewTradeRow time colTypes)
        )


viewTradeRow : Time.Posix -> List ColType -> CTypes.FullTradeInfo -> Element Msg
viewTradeRow time colTypes trade =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 1
        , Element.Background.color EH.lightGray
        , Element.width <| Element.fillPortion 7
        ]
        [ Element.row
            [ Element.width <| Element.fillPortion 6
            , Element.spacing 1
            ]
            (colTypes
                |> List.map
                    (\colType ->
                        viewTradeCell time colType trade
                    )
            )
        , cellMaker 1 <| viewPaymentMethods trade.terms.paymentMethods
        ]


viewPaymentMethods : List PaymentMethod -> Element Msg
viewPaymentMethods paymentMethods =
    paymentMethods
        |> List.head
        |> Maybe.map PaymentMethods.previewTextHack
        |> Maybe.withDefault Element.none


viewTradeCell : Time.Posix -> ColType -> CTypes.FullTradeInfo -> Element Msg
viewTradeCell time colType trade =
    cellMaker
        (colTypePortion colType)
        (case colType of
            Phase ->
                Element.text <|
                    CTypes.phaseToString trade.state.phase

            Expires ->
                case trade.state.phase of
                    CTypes.Open ->
                        case CTypes.getCurrentPhaseTimeoutInfo time trade of
                            CTypes.TimeLeft timeoutInfo ->
                                let
                                    baseIntervalColor =
                                        if TimeHelpers.getRatio (Tuple.first timeoutInfo) (Tuple.second timeoutInfo) < 0.05 then
                                            EH.red

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
                                    ( EH.red, EH.lightGray )
                                    ( Time.millisToPosix 0, totalInterval )

                    _ ->
                        Element.none

            Offer ->
                Element.row
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
                            ++ tokenUnitName trade.factory
                        )
                    ]

            FiatPrice ->
                EH.fiatValue trade.terms.price

            Margin ->
                let
                    upIsGreen =
                        trade.parameters.initiatorRole == Buyer
                in
                trade.derived.margin
                    |> Maybe.map (EH.coloredMargin upIsGreen)
                    |> Maybe.withDefault Element.none

            PaymentWindow ->
                let
                    lowValColor =
                        case trade.parameters.initiatorRole of
                            Seller ->
                                EH.red

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
                                EH.red

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


cellMaker : Int -> Element Msg -> Element Msg
cellMaker portion cellElement =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height <| Element.px 60
        , Element.clip
        , Element.Background.color EH.white
        ]
    <|
        Element.el
            [ Element.padding 12
            , Element.centerY
            , Element.width Element.fill
            ]
            cellElement


sortByFunc : ( ColType, Ordering ) -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
sortByFunc ( sortCol, ordering ) =
    (case sortCol of
        Phase ->
            \a b ->
                if a.state.phase == b.state.phase then
                    sortByFunc ( Expires, Descending ) a b

                else
                    compare (CTypes.phaseToInt a.state.phase) (CTypes.phaseToInt b.state.phase)

        Expires ->
            \a b -> TimeHelpers.compare a.derived.phaseEndTime b.derived.phaseEndTime

        Offer ->
            \a b -> TokenValue.compare a.parameters.tradeAmount b.parameters.tradeAmount

        FiatPrice ->
            \a b -> FiatValue.compare a.terms.price b.terms.price

        Margin ->
            \a b ->
                Maybe.map2
                    (\marginA marginB -> compare marginA marginB)
                    a.derived.margin
                    b.derived.margin
                    |> Maybe.withDefault EQ

        PaymentWindow ->
            \a b -> TimeHelpers.compare a.parameters.autoabortInterval b.parameters.autoabortInterval

        BurnWindow ->
            \a b ->
                TimeHelpers.compare a.parameters.autoreleaseInterval b.parameters.autoreleaseInterval
    )
        |> (if ordering == Ascending then
                flip

            else
                identity
           )
