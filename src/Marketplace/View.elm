module Marketplace.View exposing (root)

import Array exposing (Array)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Filters.Types as Filters
import Filters.View as Filters
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Html.Events.Extra
import Images exposing (Image)
import List.Extra
import Margin
import Marketplace.Types exposing (..)
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Prices exposing (Price)
import Time
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import TradeTable.View as TradeTable


root : Time.Posix -> List TradeCache -> Model -> ( Element Msg, List (Element Msg) )
root time tradeCaches model =
    let
        onlyOpenPhaseChecked =
            let
                nonOpenPhasesChecked =
                    model.filters
                        |> Filters.getFilterSet Filters.Phase
                        |> Maybe.map
                            (\filterSet ->
                                List.Extra.count
                                    (\filter ->
                                        filter.checked && filter.label /= "Open"
                                    )
                                    filterSet.options
                            )
                        |> Maybe.withDefault 0
            in
            nonOpenPhasesChecked == 0

        tcDoneLoading =
            List.all
                (TradeCache.loadingStatus >> (==) TradeCache.AllFetched)
                tradeCaches
    in
    ( EH.submodelContainer
        1800
        "Browse Offers. Local or Worldwide, Cash or Crypto."
        "MARKETPLACE"
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 30
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ statusFiltersAndSearchElement tradeCaches model.filters model.inputs model.errors model.showCurrencyDropdown
                ]
            , maybeResultsElement
                time
                onlyOpenPhaseChecked
                tcDoneLoading
                tradeCaches
                model
            ]
        )
    , []
    )


statusFiltersAndSearchElement : List TradeCache -> Filters.Model -> SearchInputs -> Errors -> Bool -> Element Msg
statusFiltersAndSearchElement tradeCaches filters inputs errors showCurrencyDropdown =
    let
        statusMsgElement s =
            Element.el
                [ Element.Font.size 20
                , Element.Font.semiBold
                , Element.Font.color EH.darkGray
                , Element.centerX
                ]
                (Element.text s)

        statusMessages : List (Element Msg)
        statusMessages =
            if List.all ((==) TradeCache.NoneFound) (List.map TradeCache.loadingStatus tradeCaches) then
                [ statusMsgElement "No trades found." ]

            else
                tradeCaches
                    |> List.map
                        (\tc ->
                            case TradeCache.loadingStatus tc of
                                TradeCache.QueryingNumTrades ->
                                    Just <| "Querying " ++ factoryName tc.factory ++ " Factory..."

                                TradeCache.NoneFound ->
                                    Nothing

                                TradeCache.FetchingTrades ->
                                    Just <| "Fetching " ++ factoryName tc.factory ++ " Trades..."

                                TradeCache.AllFetched ->
                                    Nothing
                        )
                    |> Maybe.Extra.values
                    |> List.map statusMsgElement
    in
    Element.el
        [ Element.width Element.fill
        , Element.inFront <|
            Element.column
                [ Element.spacing 5
                , Element.alignLeft
                ]
                statusMessages
        ]
    <|
        Element.row
            [ Element.centerX
            , Element.spacing 50
            ]
            [ Element.map FiltersMsg <| Filters.view filters
            , Element.row
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
                    fiatInput showCurrencyDropdown inputs.fiatType errors
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


maybeResultsElement : Time.Posix -> Bool -> Bool -> List TradeCache -> Model -> Element Msg
maybeResultsElement time onlyOpenTrades tcDoneLoading tradeCaches model =
    let
        visibleTrades =
            tradeCaches
                |> List.map TradeCache.loadedValidTrades
                |> List.concat
                |> filterTrades time model.filterFunc
    in
    if visibleTrades == [] then
        Element.el
            [ Element.centerX
            , Element.Font.size 24
            , Element.paddingEach
                { top = 30
                , left = 0
                , right = 0
                , bottom = 0
                }
            , Element.Font.italic
            ]
            (if tcDoneLoading then
                Element.text "No trades found with those filters."

             else
                Element.text "Initializing Trade Cache..."
            )

    else
        TradeTable.view
            time
            model.tradeTable
            [ if onlyOpenTrades then
                TradeTable.Expires

              else
                TradeTable.Phase
            , TradeTable.Offer
            , TradeTable.Price
            , TradeTable.Margin
            , TradeTable.PaymentWindow
            , TradeTable.BurnWindow
            ]
            visibleTrades
            |> Element.map TradeTableMsg


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
            "min Dai"
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
            "max Dai"
            maxDai
            Nothing
            Nothing
            MaxDaiChanged
        ]
        |> withInputHeader "Dai Range"


fiatInput : Bool -> Prices.Symbol -> Errors -> Element Msg
fiatInput showTypeDropdown symbol errors =
    let
        fiatLabelElement =
            Prices.getIcon symbol
                |> Maybe.withDefault Element.none

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

        flagClickedMsg =
            CmdUp <| CmdUp.gTag "click" "misclick" "currency flag" 0
    in
    Element.el
        [ Element.alignTop, Element.width <| Element.px 120 ]
        (EH.currencySelector showTypeDropdown symbol (ShowCurrencyDropdown True) FiatTypeInputChanged flagClickedMsg
            |> withInputHeader "Fiat Type"
        )


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


filterTrades :
    Time.Posix
    -> (Time.Posix -> CTypes.FullTradeInfo -> Bool)
    -> List CTypes.FullTradeInfo
    -> List CTypes.FullTradeInfo
filterTrades time filterFunc =
    List.filter (filterFunc time)
