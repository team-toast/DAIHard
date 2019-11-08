module Marketplace.View exposing (root)

import Array exposing (Array)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Currencies exposing (Price)
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
import Marketplace.Types exposing (..)
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Time
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import TradeTable.View as TradeTable


root : Time.Posix -> DisplayProfile -> List TradeCache -> Model -> ( Element Msg, List (Element Msg) )
root time dProfile tradeCaches model =
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
    ( EH.simpleSubmodelContainer
        1800
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding (30 |> changeForMobile 10 dProfile)
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ statusFiltersAndSearchElement dProfile tradeCaches model.filters model.inputs model.errors model.showCurrencyDropdown
                ]
            , maybeResultsElement
                time
                dProfile
                onlyOpenPhaseChecked
                tcDoneLoading
                tradeCaches
                model
            ]
        )
    , []
    )


statusFiltersAndSearchElement : DisplayProfile -> List TradeCache -> Filters.Model -> SearchInputs -> Errors -> Bool -> Element Msg
statusFiltersAndSearchElement dProfile tradeCaches filters inputs errors showCurrencyDropdown =
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
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 10
        ]
        [ (Element.row |> changeForMobile Element.column dProfile)
            [ Element.centerX
            , Element.spacing (50 |> changeForMobile 10 dProfile)
            ]
            [ Element.el
                [ Element.centerX ]
                (Element.map FiltersMsg <| Filters.view dProfile filters)
            , searchAndMoreFilters dProfile inputs errors showCurrencyDropdown
            ]
        , Element.column
            [ Element.spacing 5
            , Element.centerX
            ]
            statusMessages
        ]


searchAndMoreFilters : DisplayProfile -> SearchInputs -> Errors -> Bool -> Element Msg
searchAndMoreFilters dProfile inputs errors showCurrencyDropdown =
    case dProfile of
        Desktop ->
            Element.row
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

        Mobile ->
            Element.column
                [ Element.centerX
                , Element.Font.color EH.darkGray
                , Element.Font.size 16
                , Element.Font.italic
                , Element.Font.bold
                , Element.spacing 4
                ]
                [ Element.text "Visit DAIHard on Desktop"
                , Element.text "for more search options!"
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
        , Element.Font.color EH.softRed
        ]
        (Element.text "x")


maybeResultsElement : Time.Posix -> DisplayProfile -> Bool -> Bool -> List TradeCache -> Model -> Element Msg
maybeResultsElement time dProfile onlyOpenTrades tcDoneLoading tradeCaches model =
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
        let
            cols =
                case dProfile of
                    Desktop ->
                        [ if onlyOpenTrades then
                            TradeTable.Expires

                          else
                            TradeTable.Phase
                        , TradeTable.Offer
                        , TradeTable.ResponderProfit
                        , TradeTable.PaymentWindow
                        , TradeTable.BurnWindow
                        ]

                    Mobile ->
                        [ TradeTable.Offer
                        , TradeTable.Windows
                        ]
        in
        TradeTable.view
            time
            dProfile
            model.tradeTable
            model.prices
            cols
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


fiatInput : Bool -> Currencies.Symbol -> Errors -> Element Msg
fiatInput showTypeDropdown symbol errors =
    let
        fiatLabelElement =
            Currencies.icon symbol
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
    in
    Element.el
        [ Element.alignTop
        , Element.width <| Element.px 120
        , Element.below
            (if showTypeDropdown then
                currencyTypeDropdown
                    symbol
                    FiatTypeInputChanged
                    FiatTypeSelected

             else
                Element.none
            )
        ]
        (withInputHeader "Currency Type" <|
            currencyTypeButton
                showTypeDropdown
                symbol
                (ShowCurrencyDropdown True)
        )


currencyTypeButton : Bool -> Currencies.Symbol -> Msg -> Element Msg
currencyTypeButton dropdownOpen symbol onClick =
    Element.row
        [ Element.Background.color <| Element.rgb 0.98 0.98 0.98
        , Element.height Element.fill
        , Element.padding 13
        , Element.spacing 13
        , Element.pointer
        , EH.onClickNoPropagation onClick
        , Element.Border.width 1
        , Element.Border.color EH.lightGray
        ]
        [ Currencies.icon symbol
            |> Maybe.withDefault Element.none
        , Element.text
            (symbol
                |> (\s ->
                        if s == "" then
                            "[any]"

                        else
                            s
                   )
            )
        , Images.toElement
            [ Element.width <| Element.px 12 ]
          <|
            if dropdownOpen then
                Images.upArrow

            else
                Images.downArrow
        ]


currencyTypeDropdown : String -> (String -> Msg) -> (Currencies.Symbol -> Msg) -> Element Msg
currencyTypeDropdown searchInput searchChangedMsg selectedMsg =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        (ShowCurrencyDropdown False)
    <|
        let
            currenciesList =
                Currencies.fiatList ++ Currencies.foreignCryptoList
        in
        EH.searchableOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 18
            , Element.alignRight
            , EH.moveToFront
            ]
            "search currencies"
            ([ ( Element.el [ Element.width Element.fill ] (Element.text "[any]")
               , []
               , selectedMsg ""
               )
             ]
                ++ (currenciesList
                        |> List.map
                            (\fiatSymbol ->
                                ( Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 18
                                    ]
                                    (Maybe.Extra.values
                                        [ Currencies.icon fiatSymbol
                                        , Just <| Element.text fiatSymbol
                                        ]
                                    )
                                , Maybe.Extra.values [ Just fiatSymbol, Currencies.fiatChar fiatSymbol ]
                                , selectedMsg fiatSymbol
                                )
                            )
                   )
                ++ (if not (List.member searchInput currenciesList) && searchInput /= "" then
                        [ ( Element.el [ Element.width Element.fill ] (Element.text <| "\"" ++ searchInput ++ "\"")
                          , [ searchInput ]
                          , selectedMsg <| searchInput
                          )
                        ]

                    else
                        []
                   )
            )
            searchInput
            searchChangedMsg


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
