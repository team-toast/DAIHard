module Create.View exposing (root)

import CommonTypes exposing (..)
import Create.Types exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Helpers.Element as EH
import Images
import Maybe.Extra
import Prices


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( EH.simpleSubmodelContainer
        1000
        (Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            ]
            [ header model.mode
            , EH.thinGrayHRuler
            , body model
            ]
        )
    , viewModals model
    )


header : Mode -> Element Msg
header mode =
    let
        descriptionText =
            case mode of
                CryptoSwap _ ->
                    "Please choose which two cryptocurrencies you’d like to exchange, indicate which wallet address you’d like your exchanged coins sent, and edit the three trade windows if necessary."

                OffRamp ->
                    Debug.todo ""

                OnRamp ->
                    Debug.todo ""
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 20
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spaceEvenly
            , Element.spacing 30
            ]
            [ modeHeader (mode == CryptoSwap Seller || mode == CryptoSwap Buyer) (CryptoSwap Seller)
            , modeHeader (mode == OffRamp) OffRamp
            , modeHeader (mode == OnRamp) OnRamp
            ]
        , Element.paragraph
            [ Element.Font.size 16 ]
            [ Element.text descriptionText ]
        ]


modeHeader : Bool -> Mode -> Element Msg
modeHeader selected mode =
    let
        fontAlpha =
            if selected then
                1.0

            else
                0.15

        text =
            case mode of
                CryptoSwap _ ->
                    "CryptoSwap"

                OffRamp ->
                    "Off Ramp"

                OnRamp ->
                    "???"
    in
    Element.el
        [ Element.Font.size 28
        , Element.Font.semiBold
        , Element.Font.color <| Element.rgba 0 0 0 fontAlpha
        ]
        (Element.text text)


body : Model -> Element Msg
body model =
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        ]
        [ amountAndTypeIn model ]


amountAndTypeIn : Model -> Element Msg
amountAndTypeIn model =
    EH.withInputHeader
        [ Element.width Element.fill ]
        "I want to Sell"
        (Element.row
            [ Element.width Element.fill
            , Element.Background.color EH.lightGray
            , Element.Border.rounded 4
            , Element.Border.width 1
            , Element.Border.color EH.lightGray
            , Element.spacing 1
            ]
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.Border.width 0
                ]
                { onChange = AmountInChanged
                , text = model.inputs.amountIn
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color EH.placeholderTextColor ]
                            (Element.text "0")
                , label = Element.Input.labelHidden "amount in"
                }
            , Element.el
                (if model.showInTypeDropdown then
                    [ Element.below
                        (inTypeDropdown model)
                    ]

                 else
                    []
                )
                (typeDropdownButton
                    model.showInTypeDropdown
                    model.inputs.inType
                    InTypeClicked
                )
            ]
        )


inTypeDropdown : Model -> Element Msg
inTypeDropdown model =
    case model.mode of
        CryptoSwap Seller ->
            dhTokenTypeDropdown
                (InTypeSelected << DHToken)

        OffRamp ->
            dhTokenTypeDropdown
                (InTypeSelected << DHToken)

        CryptoSwap Buyer ->
            cryptoTypeDropdown
                model.inputs.currencySearch
                SearchInputChanged
                (InTypeSelected << External << foreignCryptoName)

        OnRamp ->
            fiatTypeDropdown
                model.inputs.currencySearch
                SearchInputChanged
                (InTypeSelected << External)


dhTokenTypeDropdown : (FactoryType -> Msg) -> Element Msg
dhTokenTypeDropdown msgConstructor =
    EH.basicOpenDropdown
        []
        Nothing
        (dhTokenList
            |> List.map
                (\tokenType ->
                    ( Element.row
                        [ Element.width Element.fill ]
                        [ Images.toElement
                            [ Element.width <| Element.px 36 ]
                            (Images.currencyIcon <| tokenUnitName tokenType)
                        , Element.text <| tokenUnitName tokenType
                        ]
                    , msgConstructor tokenType
                    )
                )
        )


cryptoTypeDropdown : String -> (String -> Msg) -> (ForeignCrypto -> Msg) -> Element Msg
cryptoTypeDropdown searchInput searchChangedMsg selectedMsg =
    EH.searchableOpenDropdown
        []
        "search cryptocurrencies"
        (foreignCryptoList
            |> List.map
                (\foreignCrypto ->
                    ( Element.row
                        [ Element.width Element.fill ]
                        [ Images.toElement
                            [ Element.width <| Element.px 36 ]
                            (Images.currencyIcon <| foreignCryptoName foreignCrypto)
                        , Element.text <| foreignCryptoName foreignCrypto
                        ]
                    , [ foreignCryptoName foreignCrypto ]
                    , selectedMsg foreignCrypto
                    )
                )
        )
        searchInput
        searchChangedMsg


fiatTypeDropdown : String -> (String -> Msg) -> (Prices.Symbol -> Msg) -> Element Msg
fiatTypeDropdown searchInput searchChangedMsg selectedMsg =
    EH.searchableOpenDropdown
        []
        "search currencies"
        (Prices.symbolList
            |> List.map
                (\fiatSymbol ->
                    ( Element.row
                        [ Element.width Element.fill ]
                        [ Images.toElement
                            [ Element.width <| Element.px 36 ]
                            (Images.currencyIcon fiatSymbol)
                        , Element.text fiatSymbol
                        ]
                    , Maybe.Extra.values [ Just fiatSymbol, Prices.char fiatSymbol ]
                    , selectedMsg fiatSymbol
                    )
                )
        )
        searchInput
        searchChangedMsg


typeDropdownButton : Bool -> CurrencyType -> Msg -> Element Msg
typeDropdownButton dropdownOpen currencyType onClick =
    Element.row
        [ Element.Background.color <| Element.rgb 0.07 0.07 0.07
        , Element.padding 13
        , Element.spacing 13
        ]
        [ Images.toElement
            [ Element.width <| Element.px 36 ]
            (Images.currencyIcon (currencySymbol currencyType))
        , Element.text <| currencySymbol currencyType
        , Images.toElement
            [ Element.width <| Element.px 12 ]
          <|
            if dropdownOpen then
                Images.upArrow

            else
                Images.downArrow
        ]


viewModals : Model -> List (Element Msg)
viewModals model =
    Debug.todo ""
