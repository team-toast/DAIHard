module Create.View exposing (root)

import CommonTypes exposing (..)
import Create.Types exposing (..)
import Currencies
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH
import Images
import Maybe.Extra


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( EH.simpleSubmodelContainer
        800
        (Element.column
            [ Element.width Element.fill
            , Element.spacing 20
            ]
            [ header model.mode
            , EH.thinGrayHRuler
            , body model
            , Element.el
                [ Element.height <| Element.px 600 ]
                Element.none
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
                    "You are the seller. Choose what cryptocurrency/fiat you wish to exchange, what payment methods you are willing to accept from the buyer, and edit the three trade windows if necessary."

                OnRamp ->
                    "You are the buyer. Choose what cryptocurrency/fiat you wish to exchange, what payment methods you are willing to send to the seller, and edit the three trade windows if necessary."
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 30
        ]
        [ Element.row
            [ Element.spacing 40
            , Element.centerX
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
                (InTypeSelected << External)

        OnRamp ->
            fiatTypeDropdown
                model.inputs.currencySearch
                SearchInputChanged
                (InTypeSelected << External)


dhTokenTypeDropdown : (FactoryType -> Msg) -> Element Msg
dhTokenTypeDropdown msgConstructor =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        NoOp
        CloseModals
    <|
        EH.basicOpenDropdown
            [ Element.width (Element.shrink |> Element.maximum 300 |> Element.minimum 100)
            , Element.moveDown 10
            ]
            Nothing
            (dhTokenList
                |> List.map
                    (\tokenType ->
                        ( Element.row
                            [ Element.width Element.fill ]
                            (Maybe.Extra.values
                                [ Currencies.icon <| tokenUnitName tokenType
                                , Just <| Element.text <| tokenUnitName tokenType
                                ]
                            )
                        , msgConstructor tokenType
                        )
                    )
            )


cryptoTypeDropdown : String -> (String -> Msg) -> (Currencies.Symbol -> Msg) -> Element Msg
cryptoTypeDropdown searchInput searchChangedMsg selectedMsg =
    EH.searchableOpenDropdown
        [ Element.width (Element.shrink |> Element.maximum 300 |> Element.minimum 100)
        , Element.moveDown 18
        ]
        "search cryptocurrencies"
        (Currencies.foreignCryptoList
            |> List.map
                (\symbol ->
                    ( Element.row
                        [ Element.width Element.fill ]
                        (Maybe.Extra.values
                            [ Currencies.icon symbol
                            , Just <| Element.text symbol
                            ]
                        )
                    , [ symbol ]
                    , selectedMsg symbol
                    )
                )
        )
        searchInput
        searchChangedMsg


fiatTypeDropdown : String -> (String -> Msg) -> (Currencies.Symbol -> Msg) -> Element Msg
fiatTypeDropdown searchInput searchChangedMsg selectedMsg =
    EH.searchableOpenDropdown
        [ Element.width (Element.shrink |> Element.maximum 300 |> Element.minimum 100)
        , Element.moveDown 18
        ]
        "search currencies"
        (Currencies.fiatList
            |> List.map
                (\fiatSymbol ->
                    ( Element.row
                        [ Element.width Element.fill ]
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
        searchInput
        searchChangedMsg


typeDropdownButton : Bool -> CurrencyType -> Msg -> Element Msg
typeDropdownButton dropdownOpen currencyType onClick =
    Element.row
        [ Element.Background.color <| Element.rgb 0.98 0.98 0.98
        , Element.padding 13
        , Element.spacing 13
        , Element.pointer
        , EH.onClickNoPropagation onClick
        ]
        [ Currencies.icon (currencySymbol currencyType)
            |> Maybe.withDefault Element.none
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
    []
