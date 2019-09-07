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
import FormatFloat exposing (formatFloat)
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
        , Element.spacing 25
        ]
        [ amountAndTypeIn model
        , Element.el [ Element.centerX ] swapButton
        , amountOutRow model
        ]


amountAndTypeIn : Model -> Element Msg
amountAndTypeIn model =
    EH.withInputHeader
        [ Element.width Element.fill ]
        "I want to Sell"
        (inputContainer
            []
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.height Element.fill
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


swapButton : Element Msg
swapButton =
    EH.elOnCircle
        [ Element.pointer
        , Element.Events.onClick SwapClicked
        ]
        52
        (Element.rgba 0.05 0.03 0.92 0.05)
        (Images.toElement
            [ Element.height <| Element.px 30 ]
            Images.verticalSwapArrows
        )


amountOutRow : Model -> Element Msg
amountOutRow model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 25
        ]
        [ Element.el [ Element.width <| Element.fillPortion 2 ]
            (amountAndTypeOut model)
        , Element.el
            [ Element.Font.size 28
            , Element.Font.color <| Element.rgba 0.05 0.1 0.3 0.25
            , Element.Font.semiBold
            , Element.alignBottom
            , Element.paddingEach
                { bottom = 14
                , top = 0
                , right = 0
                , left = 0
                }
            ]
            (Element.text "@")
        , Element.el [ Element.width <| Element.fillPortion 1 ]
            (marginBox model)
        ]


amountAndTypeOut : Model -> Element Msg
amountAndTypeOut model =
    EH.withInputHeader
        [ Element.width Element.fill ]
        "In Exchange for"
        (inputContainer
            []
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.width 0
                ]
                { onChange = AmountOutChanged
                , text = model.inputs.amountOut
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color EH.placeholderTextColor ]
                            (Element.text "0")
                , label = Element.Input.labelHidden "amount out"
                }
            , Element.el
                (if model.showOutTypeDropdown then
                    [ Element.below
                        (outTypeDropdown model)
                    ]

                 else
                    []
                )
                (typeDropdownButton
                    model.showOutTypeDropdown
                    model.inputs.outType
                    OutTypeClicked
                )
            ]
        )


marginBox : Model -> Element Msg
marginBox model =
    EH.withInputHeader
        [ Element.width Element.fill ]
        "Margin"
        (inputContainer
            [ Element.pointer
            ]
            [ Element.row
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.Background.color EH.white
                , Element.spacing 13
                ]
                [ profitLossOrEven model.margin
                , absMarginPercentage model.margin
                ]
            , Element.el
                [ Element.alignRight
                , Element.height Element.fill
                , Element.Background.color <| Element.rgb 0.98 0.98 0.98
                , Element.padding 13
                ]
              <|
                Images.toElement
                    [ Element.centerY ]
                    (if False then
                        Images.upArrow

                     else
                        Images.downArrow
                    )
            ]
        )


profitLossOrEven : Float -> Element Msg
profitLossOrEven margin =
    let
        ( text, bgColor, textColor ) =
            if margin == 0 then
                ( "Even"
                , Element.rgba255 16 7 234 0.05
                , Element.rgb255 16 7 234
                )

            else if margin > 0 then
                ( "Profit"
                , Element.rgba255 0 188 137 0.05
                , Element.rgb255 0 188 137
                )

            else
                ( "Loss"
                , Element.rgba255 255 0 118 0.05
                , Element.rgb255 255 0 118
                )
    in
    Element.el
        [ Element.padding 7 ]
    <|
        Element.el
            [ Element.paddingXY 15 9
            , Element.Background.color bgColor
            , Element.Border.rounded 4
            , Element.Font.color textColor
            , Element.Font.size 20
            , Element.Font.semiBold
            ]
            (Element.text text)


absMarginPercentage : Float -> Element Msg
absMarginPercentage margin =
    Element.el
        [ Element.Font.size 20
        , Element.Font.semiBold
        ]
        (Element.text
            (((margin * 100)
                |> abs
                |> formatFloat 2
             )
                ++ "%"
            )
        )


inputContainer : List (Element.Attribute Msg) -> List (Element Msg) -> Element Msg
inputContainer attributes =
    Element.row <|
        attributes
            ++ [ Element.width Element.fill
               , Element.Background.color EH.lightGray
               , Element.Border.rounded 4
               , Element.Border.width 1
               , Element.Border.color EH.lightGray
               , Element.spacing 1
               ]


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


outTypeDropdown : Model -> Element Msg
outTypeDropdown model =
    case model.mode of
        CryptoSwap Seller ->
            cryptoTypeDropdown
                model.inputs.currencySearch
                SearchInputChanged
                (OutTypeSelected << External)

        CryptoSwap Buyer ->
            dhTokenTypeDropdown
                (OutTypeSelected << DHToken)

        OffRamp ->
            fiatTypeDropdown
                model.inputs.currencySearch
                SearchInputChanged
                (OutTypeSelected << External)

        OnRamp ->
            dhTokenTypeDropdown
                (OutTypeSelected << DHToken)


dhTokenTypeDropdown : (FactoryType -> Msg) -> Element Msg
dhTokenTypeDropdown msgConstructor =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        NoOp
        CloseModals
    <|
        EH.basicOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 10
            ]
            Nothing
            (dhTokenList
                |> List.map
                    (\tokenType ->
                        ( Element.row
                            [ Element.width Element.fill
                            , Element.spacing 18
                            ]
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
    EH.modal
        (Element.rgba 0 0 0 0.1)
        NoOp
        CloseModals
    <|
        EH.searchableOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 18
            ]
            "search cryptocurrencies"
            (Currencies.foreignCryptoList
                |> List.map
                    (\symbol ->
                        ( Element.row
                            [ Element.width Element.fill
                            , Element.spacing 18
                            ]
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
    EH.modal
        (Element.rgba 0 0 0 0.1)
        NoOp
        CloseModals
    <|
        EH.searchableOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 18
            ]
            "search currencies"
            (Currencies.fiatList
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
