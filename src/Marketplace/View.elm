module Marketplace.View exposing (root)

import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue exposing (FiatValue)
import Html.Events.Extra
import Images exposing (Image)
import Margin
import Marketplace.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TradeCache.State as TradeCache
import TradeCache.Types exposing (TradeCache)


root : Time.Posix -> TradeCache -> Model -> Element Msg
root time tradeCache model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Events.onClick (ShowCurrencyDropdown False)
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            , Element.padding 30
            ]
            [ Element.el
                [ Element.alignTop
                , Element.width (Element.fill |> Element.maximum 300)
                ]
                (EH.withHeader
                    "Offer Type"
                    (typeToggleElement model.inputs.openMode)
                )
            , searchInputElement model.inputs model.errors model.showCurrencyDropdown
            ]
        , resultsElement time tradeCache model
        ]


searchInputElement : SearchInputs -> Errors -> Bool -> Element Msg
searchInputElement inputs errors showCurrencyDropdown =
    Element.column
        [ Element.spacing 10
        , Element.width Element.shrink
        , Element.centerX
        ]
        [ Element.row
            [ Element.width Element.shrink
            , Element.spacing 10
            ]
            [ Element.el
                [ Element.width <| Element.shrink
                , Element.alignTop
                ]
              <|
                daiRangeInput inputs.minDai inputs.maxDai errors
            , Element.el
                [ Element.width Element.shrink
                , Element.alignTop
                ]
              <|
                fiatInput showCurrencyDropdown inputs.fiatType inputs.minFiat inputs.maxFiat errors
            , Element.column
                [ Element.width Element.shrink
                , Element.alignTop
                , Element.spacing 5
                ]
                [ paymentMethodsInput inputs.paymentMethod
                , searchTermsDisplayElement inputs.paymentMethodTerms
                ]
            , Element.column
                [ Element.spacing 5
                , Element.width Element.shrink
                ]
                [ applyButton, resetButton ]
                |> withInputHeader " "
            ]
        ]


typeToggleElement : CTypes.OpenMode -> Element Msg
typeToggleElement openMode =
    let
        baseStyles =
            [ Element.Font.size 24
            , Element.Font.medium
            , Element.pointer
            ]

        ( buyStyles, sellStyles ) =
            case openMode of
                CTypes.BuyerOpened ->
                    ( baseStyles
                    , baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    )

                CTypes.SellerOpened ->
                    ( baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    , baseStyles
                    )
    in
    Element.wrappedRow
        [ Element.spacing 20
        , Element.width Element.fill
        ]
        [ Element.el
            ([ Element.Events.onClick <| ChangeOfferType CTypes.SellerOpened ] ++ sellStyles)
            (Element.text "Selling DAI")
        , Element.el
            ([ Element.Events.onClick <| ChangeOfferType CTypes.BuyerOpened ] ++ buyStyles)
            (Element.text "Buying DAI")
        ]


searchTermsDisplayElement : List String -> Element Msg
searchTermsDisplayElement searchTerms =
    case searchTerms of
        [] ->
            Element.none

        terms ->
            Element.row [ Element.width Element.fill, Element.padding 10, Element.spacing 10 ]
                (terms
                    |> List.map
                        (\term ->
                            Element.row
                                [ Element.Background.color <| Element.rgba255 16 7 234 0.2
                                , Element.Border.rounded 5
                                , Element.padding 4
                                , Element.spacing 3
                                ]
                                [ Element.text term
                                , removeSearchTermButton term
                                ]
                        )
                )


removeSearchTermButton : String -> Element Msg
removeSearchTermButton term =
    Element.el
        [ Element.padding 3
        , Element.alignTop
        , Element.pointer
        , Element.Events.onClick <| RemoveTerm term
        , Element.Font.size 10
        , Element.Font.color EH.red
        ]
        (Element.text "x")


resultsElement : Time.Posix -> TradeCache -> Model -> Element Msg
resultsElement time tradeCache model =
    let
        visibleTrades =
            TradeCache.loadedTrades tradeCache
                |> filterAndSortTrades time model.filterFunc model.sortFunc

        buyingOrSellingString =
            case model.inputs.openMode of
                CTypes.BuyerOpened ->
                    "Buying"

                CTypes.SellerOpened ->
                    "Selling"
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ cellMaker ( 1, sortableColumnHeader "Expires" Expiring Nothing )
            , cellMaker ( 1, sortableColumnHeader buyingOrSellingString TradeAmount Nothing )
            , cellMaker ( 2, sortableColumnHeader "For Fiat" Fiat Nothing )
            , cellMaker ( 1, sortableColumnHeader "Margin" Margin Nothing )
            , cellMaker ( 6, columnHeader "Accepted Payment Methods" )
            , cellMaker ( 2, sortableColumnHeader "Payment Window" AutoabortWindow Nothing )
            , cellMaker ( 2, sortableColumnHeader "Auto-Release" AutoreleaseWindow Nothing )
            , cellMaker ( 2, Element.none )
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.Border.width 1
            , Element.Border.rounded 8
            , Element.Border.color EH.lightGray
            , Element.spacing 1
            , Element.Background.color EH.lightGray
            , Element.clip
            ]
            (visibleTrades
                |> List.map
                    (viewTradeRow time (model.inputs.openMode == CTypes.SellerOpened))
            )
        ]


daiRangeInput : String -> String -> Errors -> Element Msg
daiRangeInput minDai maxDai errors =
    let
        daiLabelElement =
            EH.daiSymbol [ Element.centerY ]

        minElement =
            Element.row [ Element.spacing 8, Element.centerY, Element.width <| Element.px 60 ]
                [ daiLabelElement
                , Element.el [ Element.Font.size 16, Element.centerY ] (Element.text "min")
                ]

        maxElement =
            Element.row [ Element.spacing 8, Element.centerY, Element.width <| Element.px 60 ]
                [ daiLabelElement
                , Element.el [ Element.Font.size 16, Element.centerY ] (Element.text "max")
                ]
    in
    Element.column [ Element.spacing 5, Element.width <| Element.px 200 ]
        [ EH.textInputWithElement
            [ Element.onLeft <|
                EH.maybeErrorElement
                    [ Element.moveLeft 5, Element.width <| Element.px 200 ]
                    errors.minDai
            ]
            [ Element.Events.onFocus (ShowCurrencyDropdown False) ]
            minElement
            "min dai"
            minDai
            Nothing
            Nothing
            MinDaiChanged
        , EH.textInputWithElement
            [ Element.onLeft <|
                EH.maybeErrorElement
                    [ Element.moveLeft 5, Element.width <| Element.px 200 ]
                    errors.maxDai
            ]
            [ Element.Events.onFocus (ShowCurrencyDropdown False) ]
            maxElement
            "max dai"
            maxDai
            Nothing
            Nothing
            MaxDaiChanged
        ]
        |> withInputHeader "Dai Range"


fiatInput : Bool -> String -> String -> String -> Errors -> Element Msg
fiatInput showTypeDropdown fiatType minFiat maxFiat errors =
    let
        fiatLabelElement =
            EH.fiatTypeToSymbolElement fiatType

        minElement =
            Element.row [ Element.spacing 8, Element.centerY, Element.width <| Element.px 60 ]
                [ fiatLabelElement
                , Element.el [ Element.Font.size 16, Element.centerY ] (Element.text "min")
                ]

        maxElement =
            Element.row [ Element.spacing 8, Element.centerY, Element.width <| Element.px 60 ]
                [ fiatLabelElement
                , Element.el [ Element.Font.size 16, Element.centerY ] (Element.text "max")
                ]
    in
    Element.row [ Element.spacing 5, Element.width Element.shrink ]
        [ Element.el [ Element.alignTop, Element.width <| Element.px 120 ] <|
            EH.currencySelector showTypeDropdown fiatType (ShowCurrencyDropdown True) FiatTypeInputChanged
        , Element.column [ Element.spacing 5, Element.alignTop, Element.width <| Element.px 200 ]
            [ EH.textInputWithElement
                [ Element.above <|
                    EH.maybeErrorElement
                        [ Element.moveUp 5, Element.width (Element.shrink |> Element.maximum 200) ]
                        errors.minFiat
                ]
                [ Element.Events.onFocus (ShowCurrencyDropdown False) ]
                minElement
                "min"
                minFiat
                Nothing
                Nothing
                MinFiatChanged
            , EH.textInputWithElement
                [ Element.below <|
                    EH.maybeErrorElement
                        [ Element.moveDown 5, Element.width (Element.shrink |> Element.maximum 200) ]
                        errors.maxFiat
                ]
                [ Element.Events.onFocus (ShowCurrencyDropdown False) ]
                maxElement
                "max"
                maxFiat
                Nothing
                Nothing
                MaxFiatChanged
            ]
        ]
        |> withInputHeader "Fiat Type"


paymentMethodsInput : String -> Element Msg
paymentMethodsInput searchString =
    Element.Input.text
        [ Element.alignTop
        , Element.width <| Element.px 250
        , Element.height <| Element.px 40
        , Element.Border.color EH.lightGray
        , Element.Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 20
            , color = Element.rgba255 233 237 242 0.05
            }
        , Element.htmlAttribute <| Html.Events.Extra.onEnter AddSearchTerm
        ]
        { onChange = PaymentMethodInputChanged
        , text = searchString
        , placeholder = Nothing
        , label = Element.Input.labelHidden "payment methods search"
        }
        |> withInputHeader "Search Payment Methods"


applyButton : Element Msg
applyButton =
    Element.Input.button
        [ Element.Background.color EH.blue
        , Element.padding 10
        , Element.Border.rounded 5
        ]
        { onPress = Just ApplyInputs
        , label =
            Element.el
                [ Element.Font.color EH.white
                , Element.centerX
                , Element.centerY
                ]
                (Element.text "Apply")
        }


resetButton : Element Msg
resetButton =
    Element.Input.button
        [ Element.Background.color EH.blue
        , Element.padding 10
        , Element.Border.rounded 5
        ]
        { onPress = Just ResetSearch
        , label =
            Element.el
                [ Element.Font.color EH.white
                , Element.centerX
                , Element.centerY
                ]
                (Element.text "Reset")
        }


withInputHeader : String -> Element Msg -> Element Msg
withInputHeader title element =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 8
        ]
        [ Element.el [ Element.Font.size 17, Element.Font.medium ] <| Element.text title
        , element
        ]


viewTradeRow : Time.Posix -> Bool -> CTypes.FullTradeInfo -> Element Msg
viewTradeRow time asBuyer trade =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 1
        ]
        (List.map cellMaker
            [ ( 1, viewExpiring time trade )
            , ( 1, viewTradeAmount trade )
            , ( 2, viewFiat trade )
            , ( 1, viewMargin trade (not asBuyer) )
            , ( 6, viewPaymentMethods trade.paymentMethods )
            , ( 2, viewAutoabortWindow asBuyer trade )
            , ( 2, viewAutoreleaseWindow asBuyer trade )
            , ( 2, viewTradeButton trade.factoryID )
            ]
        )


cellMaker : ( Int, Element Msg ) -> Element Msg
cellMaker ( portion, cellElement ) =
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


viewExpiring : Time.Posix -> CTypes.FullTradeInfo -> Element Msg
viewExpiring time trade =
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


viewTradeAmount : CTypes.FullTradeInfo -> Element Msg
viewTradeAmount trade =
    EH.daiValue trade.parameters.tradeAmount


viewFiat : CTypes.FullTradeInfo -> Element Msg
viewFiat trade =
    EH.fiatValue trade.parameters.fiatPrice


viewMargin : CTypes.FullTradeInfo -> Bool -> Element Msg
viewMargin trade upIsGreen =
    trade.derived.margin
        |> Maybe.map (EH.margin upIsGreen)
        |> Maybe.withDefault Element.none


viewPaymentMethods : List PaymentMethod -> Element Msg
viewPaymentMethods paymentMethods =
    paymentMethods
        |> List.head
        |> Maybe.map PaymentMethods.previewTextHack
        |> Maybe.withDefault Element.none


viewAutoabortWindow : Bool -> CTypes.FullTradeInfo -> Element Msg
viewAutoabortWindow viewAsBuyer trade =
    let
        lowValColor =
            if viewAsBuyer then
                EH.red

            else
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


viewAutoreleaseWindow : Bool -> CTypes.FullTradeInfo -> Element Msg
viewAutoreleaseWindow viewAsBuyer trade =
    let
        lowValColor =
            if viewAsBuyer then
                EH.green

            else
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


viewTradeButton : Int -> Element Msg
viewTradeButton factoryID =
    Element.Input.button
        [ Element.Background.color <| Element.rgba255 16 7 234 0.2
        , Element.padding 11
        , Element.Border.rounded 4
        , Element.width Element.fill
        , Element.mouseOver [ Element.Background.color <| Element.rgba255 16 7 234 0.4 ]
        ]
        { onPress = Just <| TradeClicked factoryID
        , label =
            Element.el [ Element.centerX, Element.Font.color <| Element.rgb255 16 7 234, Element.Font.medium ] <| Element.text "View Offer"
        }


getLoadedTrades : List CTypes.Trade -> List CTypes.FullTradeInfo
getLoadedTrades =
    List.filterMap
        (\trade ->
            case trade of
                CTypes.LoadedTrade tradeInfo ->
                    Just tradeInfo

                _ ->
                    Nothing
        )


filterAndSortTrades :
    Time.Posix
    -> (Time.Posix -> CTypes.FullTradeInfo -> Bool)
    -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
    -> List CTypes.FullTradeInfo
    -> List CTypes.FullTradeInfo
filterAndSortTrades time filterFunc sortFunc =
    List.filter (filterFunc time)
        >> List.sortWith sortFunc


sortableColumnHeader : String -> ResultColumnType -> Maybe Bool -> Element Msg
sortableColumnHeader title colType sorting =
    Element.row [ Element.spacing 8 ]
        [ columnHeader title
        , Element.column
            [ Element.spacing 2 ]
            [ Element.el
                [ Element.padding 4
                , Element.pointer
                , Element.Events.onClick <|
                    SortBy colType True
                ]
                (Images.toElement
                    [ Element.width <| Element.px 8
                    , Element.centerX
                    , Element.centerY
                    ]
                    Images.upArrow
                )
            , Element.el
                [ Element.padding 4
                , Element.pointer
                , Element.Events.onClick <|
                    SortBy colType False
                ]
                (Images.toElement
                    [ Element.width <| Element.px 8
                    , Element.centerX
                    , Element.centerY
                    ]
                    Images.downArrow
                )
            ]
        ]


columnHeader : String -> Element Msg
columnHeader title =
    Element.el [ Element.Font.medium, Element.Font.size 17 ] <| Element.text title
