module Create.View exposing (root)

import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
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
import Helpers.Eth as EthHelpers
import Helpers.Tuple as TupleHelpers
import Images
import Maybe.Extra
import Routing
import TokenValue exposing (TokenValue)
import Wallet


root : DisplayProfile -> Model -> ( Element Msg, List (Element Msg) )
root dProfile model =
    ( Element.column
        [ Element.width Element.fill
        , Element.paddingEach
            { bottom = 40
            , top = 0
            , right = 0
            , left = 0
            }
        ]
        [ EH.simpleSubmodelContainer
            800
            (Element.column
                [ Element.width Element.fill
                , Element.spacing (20 |> changeForMobile 10 dProfile)
                ]
                [ header dProfile model.mode
                , EH.thinGrayHRuler
                , body dProfile model
                ]
            )
        ]
    , viewModals dProfile model
    )


header : DisplayProfile -> Mode -> Element Msg
header dProfile mode =
    let
        descriptionText =
            case mode of
                CryptoSwap Seller ->
                    "Trade Sai/xDai for another crypto. Choose the amounts and types of crypto, and fill in your crypto receive address. Advanced users may wish to change the three trade windows."

                CryptoSwap Buyer ->
                    "Trade another crypto for Sai/xDai. Choose the amounts and types of crypto, and advanced users may wish to change the three trade windows."

                OffRamp ->
                    "Turn your Sai/xDai into any local currency. Choose your amounts and fiat type, describe how you can accept the fiat payment from a Buyer, and if necessary edit the three trade windows."

                OnRamp ->
                    "Deposit Sai/xDai to begin a fiat purchase to get 3X more Sai/xDai than your deposit. Choose your amounts and fiat type, describe how you can make the fiat payment to a Seller, and if necessary edit the three trade windows."
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding (30 |> changeForMobile 10 dProfile)
        ]
        [ Element.row
            [ Element.spacing (40 |> changeForMobile 10 dProfile)
            , Element.centerX
            ]
            [ modeHeader dProfile (mode == CryptoSwap Seller || mode == CryptoSwap Buyer) (CryptoSwap Seller)
            , modeHeader dProfile (mode == OffRamp) OffRamp
            , modeHeader dProfile (mode == OnRamp) OnRamp
            ]
        , Element.paragraph
            [ Element.Font.size (16 |> changeForMobile 12 dProfile)
            , Element.spacing 2
            ]
            [ Element.text descriptionText ]
        ]


modeHeader : DisplayProfile -> Bool -> Mode -> Element Msg
modeHeader dProfile selected mode =
    let
        fontAlpha =
            if selected then
                1.0

            else
                0.15

        text =
            case mode of
                CryptoSwap _ ->
                    "Crypto Portal"

                OffRamp ->
                    "Get Fiat"

                OnRamp ->
                    "Get More Sai"
    in
    Element.el
        [ Element.Font.size (28 |> changeForMobile 14 dProfile)
        , Element.Font.semiBold
        , Element.Font.color <| Element.rgba 0 0 0 fontAlpha
        , Element.pointer
        , Element.Events.onClick <| ChangeMode mode
        ]
        (Element.text text)


body : DisplayProfile -> Model -> Element Msg
body dProfile model =
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 25
        ]
        [ amountAndTypeIn dProfile model
        , case model.mode of
            CryptoSwap _ ->
                Element.el [ Element.centerX ] swapButton

            _ ->
                Element.none
        , amountOutRow dProfile model
        , moreInfoInput dProfile model
        , intervalsElement dProfile model
        , placeOrderButton dProfile model
        ]


amountAndTypeIn : DisplayProfile -> Model -> Element Msg
amountAndTypeIn dProfile model =
    EH.withInputHeaderAndMaybeError dProfile
        [ Element.width Element.fill ]
        "I want to Sell"
        model.errors.amountIn
        (inputContainer dProfile
            ([ Element.width Element.fill ]
                ++ (if model.showInTypeDropdown then
                        [ Element.below
                            (inTypeDropdown dProfile model)
                        ]

                    else
                        []
                   )
            )
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
                [ Element.height Element.fill ]
                (typeDropdownButton
                    dProfile
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


amountOutRow : DisplayProfile -> Model -> Element Msg
amountOutRow dProfile model =
    let
        marginEls =
            case model.mode of
                CryptoSwap _ ->
                    [ Element.el
                        [ Element.Font.size (28 |> changeForMobile 22 dProfile)
                        , Element.Font.color <| Element.rgba 0.05 0.1 0.3 0.25
                        , Element.Font.semiBold
                        , Element.alignBottom
                        , Element.paddingEach
                            { bottom = 14 |> changeForMobile 12 dProfile
                            , top = 0
                            , right = 0
                            , left = 0
                            }
                        ]
                        (Element.text "@")
                    , Element.el
                        [ Element.width <| Element.fillPortion 1
                        , Element.above
                            (if model.showMarginModal then
                                marginModal dProfile model.margin model.inputs.margin model.errors.margin

                             else
                                Element.none
                            )
                        ]
                        (marginBox dProfile model)
                    ]

                _ ->
                    []
    in
    case dProfile of
        Desktop ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing 25
                ]
                ([ Element.el [ Element.width <| Element.fillPortion 2 ]
                    (amountAndTypeOut dProfile model)
                 ]
                    ++ marginEls
                )

        Mobile ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ amountAndTypeOut dProfile model
                , Element.row
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    marginEls
                ]


moreInfoInput : DisplayProfile -> Model -> Element Msg
moreInfoInput dProfile model =
    case model.mode of
        CryptoSwap Seller ->
            case model.inputs.outType of
                External cryptoSymbol ->
                    cryptoAddressInput dProfile cryptoSymbol model.inputs.receiveAddress

                _ ->
                    let
                        _ =
                            Debug.log "Unexpected currency type for outType!" model.inputs.outType
                    in
                    Element.none

        CryptoSwap Buyer ->
            Element.none

        OffRamp ->
            paymentMethodInput dProfile Seller model.inputs.paymentMethod

        OnRamp ->
            paymentMethodInput dProfile Buyer model.inputs.paymentMethod


amountAndTypeOut : DisplayProfile -> Model -> Element Msg
amountAndTypeOut dProfile model =
    EH.withInputHeaderAndMaybeError dProfile
        [ Element.width Element.fill ]
        "In Exchange for"
        model.errors.amountOut
        (inputContainer dProfile
            ([ Element.width Element.fill ]
                ++ (if model.showOutTypeDropdown then
                        [ Element.below
                            (outTypeDropdown dProfile model)
                        ]

                    else
                        []
                   )
            )
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
                [ Element.height Element.fill ]
                (typeDropdownButton
                    dProfile
                    model.showOutTypeDropdown
                    model.inputs.outType
                    OutTypeClicked
                )
            ]
        )


marginBox : DisplayProfile -> Model -> Element Msg
marginBox dProfile model =
    (case dProfile of
        Desktop ->
            EH.withInputHeader dProfile
                [ Element.width Element.fill ]
                "Margin"

        Mobile ->
            identity
    )
        (inputContainer dProfile
            [ Element.width Element.fill
            , Element.pointer
            , EH.onClickNoPropagation MarginBoxClicked
            ]
            [ Element.row
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.Background.color EH.white
                , Element.spacing 13
                ]
                [ profitLossOrEven dProfile model.margin
                , absMarginPercentage dProfile model.margin
                ]
            , dropdownArrow model.showMarginModal
            ]
        )


marginModal : DisplayProfile -> Float -> String -> Maybe String -> Element Msg
marginModal dProfile margin marginInput maybeError =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        CloseModals
    <|
        Element.column
            [ Element.alignRight
            , Element.moveUp 10
            , Element.Border.rounded 8
            , Element.Background.color EH.lightGray
            , Element.clip
            , Element.spacing 1
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.08
                }
            ]
            [ Element.el
                [ Element.paddingXY 23 18 |> changeForMobile (Element.paddingXY 18 16) dProfile
                , Element.Background.color EH.white
                ]
              <|
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ Element.el
                        [ Element.Font.size (20 |> changeForMobile 18 dProfile)
                        , Element.Font.semiBold
                        , Element.Font.color <| Element.rgb255 16 7 234
                        ]
                        (Element.text "Margin")
                    , Element.paragraph
                        [ Element.Font.size (16 |> changeForMobile 14 dProfile)
                        , Element.Font.color <| Element.rgba 0 0 0 0.75
                        , Element.spacing 2
                        ]
                        [ Element.text "This is how much you want to either make as a profit or loss from this trade. Trading at a loss can help to find a buyer fast, but it's possible to trade at a profit if your payment method is highly convenient to the other party." ]
                    ]
            , let
                inactiveBgColor =
                    Element.rgba255 10 33 108 0.04

                inactiveTextColor =
                    Element.rgb255 10 33 108

                inputEl =
                    inputContainer dProfile
                        [ Element.width <| Element.px 140
                        , Element.above <|
                            case maybeError of
                                Just error ->
                                    Element.el
                                        [ Element.Font.size 12
                                        , Element.Font.color EH.softRed
                                        , Element.moveUp 16
                                        , Element.alignLeft
                                        , Element.Background.color EH.white
                                        , Element.Border.widthEach
                                            { top = 0
                                            , bottom = 0
                                            , right = 1
                                            , left = 1
                                            }
                                        , Element.paddingXY 5 0
                                        , Element.Border.color EH.lightGray
                                        ]
                                        (Element.text error)

                                Nothing ->
                                    Element.none
                        ]
                        [ Element.Input.text
                            [ Element.Border.width 0
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            { onChange = MarginInputChanged
                            , text = marginInput ++ "%"
                            , placeholder = Nothing
                            , label = Element.Input.labelHidden "margin"
                            }
                        ]

                profitLossEvenButtons =
                    [ if margin < 0 then
                        button dProfile
                            EH.softRed
                            EH.white
                            "Loss"
                            Nothing

                      else
                        button dProfile
                            inactiveBgColor
                            inactiveTextColor
                            "Loss"
                            (Just <| MarginButtonClicked Loss)
                    , if margin == 0 then
                        button dProfile
                            (Element.rgb255 16 7 234)
                            EH.white
                            "Even"
                            Nothing

                      else
                        button dProfile
                            inactiveBgColor
                            inactiveTextColor
                            "Even"
                            (Just <| MarginButtonClicked Even)
                    , if margin > 0 then
                        button dProfile
                            (Element.rgb255 0 188 137)
                            EH.white
                            "Profit"
                            Nothing

                      else
                        button dProfile
                            inactiveBgColor
                            inactiveTextColor
                            "Profit"
                            (Just <| MarginButtonClicked Profit)
                    ]
              in
              case dProfile of
                Desktop ->
                    Element.row
                        [ Element.width Element.shrink
                        , Element.Background.color EH.white
                        , Element.paddingXY 23 18
                        , Element.spacing 12
                        ]
                        ([ inputEl ] ++ profitLossEvenButtons)

                Mobile ->
                    Element.column
                        [ Element.spacing 5
                        , Element.width Element.fill
                        , Element.paddingXY 18 16
                        , Element.Background.color EH.white
                        ]
                        [ Element.el [ Element.centerX ] inputEl
                        , Element.row
                            [ Element.centerX
                            , Element.spacing 5
                            ]
                            profitLossEvenButtons
                        ]
            ]


button : DisplayProfile -> Element.Color -> Element.Color -> String -> Maybe Msg -> Element Msg
button dProfile bgColor textColor text maybeOnClick =
    Element.el
        ([ Element.Background.color bgColor
         , Element.Border.rounded 4
         , Element.paddingXY 22 16 |> changeForMobile (Element.padding 10) dProfile
         , Element.Font.color textColor
         , Element.Font.size (20 |> changeForMobile 16 dProfile)
         , EH.noSelectText
         ]
            ++ (case maybeOnClick of
                    Just onClick ->
                        [ Element.pointer
                        , Element.Events.onClick onClick
                        ]

                    Nothing ->
                        []
               )
        )
        (Element.text text)


profitLossOrEven : DisplayProfile -> Float -> Element Msg
profitLossOrEven dProfile margin =
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
                , EH.softRed |> EH.addAlpha 0.05
                , EH.softRed
                )
    in
    Element.el
        [ Element.padding (7 |> changeForMobile 4 dProfile) ]
    <|
        Element.el
            [ Element.paddingXY 15 9
            , Element.Background.color bgColor
            , Element.Border.rounded 4
            , Element.Font.color textColor
            , Element.Font.size (20 |> changeForMobile 16 dProfile)
            , Element.Font.semiBold
            ]
            (Element.text text)


absMarginPercentage : DisplayProfile -> Float -> Element Msg
absMarginPercentage dProfile margin =
    Element.el
        [ Element.Font.size (20 |> changeForMobile 16 dProfile)
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


inTypeDropdown : DisplayProfile -> Model -> Element Msg
inTypeDropdown dProfile model =
    case model.mode of
        CryptoSwap Seller ->
            dhTokenTypeDropdown dProfile
                model.testMode
                (InTypeSelected << DHToken)

        OffRamp ->
            dhTokenTypeDropdown dProfile
                model.testMode
                (InTypeSelected << DHToken)

        CryptoSwap Buyer ->
            cryptoTypeDropdown dProfile
                model.inputs.currencySearch
                SearchInputChanged
                (InTypeSelected << External)

        OnRamp ->
            fiatTypeDropdown dProfile
                model.inputs.currencySearch
                SearchInputChanged
                (InTypeSelected << External)


outTypeDropdown : DisplayProfile -> Model -> Element Msg
outTypeDropdown dProfile model =
    case model.mode of
        CryptoSwap Seller ->
            cryptoTypeDropdown dProfile
                model.inputs.currencySearch
                SearchInputChanged
                (OutTypeSelected << External)

        CryptoSwap Buyer ->
            dhTokenTypeDropdown dProfile
                model.testMode
                (OutTypeSelected << DHToken)

        OffRamp ->
            fiatTypeDropdown dProfile
                model.inputs.currencySearch
                SearchInputChanged
                (OutTypeSelected << External)

        OnRamp ->
            dhTokenTypeDropdown dProfile
                model.testMode
                (OutTypeSelected << DHToken)


dhTokenTypeDropdown : DisplayProfile -> Bool -> (FactoryType -> Msg) -> Element Msg
dhTokenTypeDropdown dProfile testMode msgConstructor =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        CloseModals
    <|
        EH.basicOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 10
            , Element.alignRight |> changeForMobile Element.centerX dProfile
            ]
            Nothing
            (dhTokenList testMode
                |> List.map
                    (\tokenType ->
                        ( Element.row
                            [ Element.width Element.fill
                            , Element.spacing 18
                            ]
                            (Maybe.Extra.values
                                [ Currencies.icon <| tokenSymbol tokenType
                                , Just <| Element.text <| tokenUnitName tokenType
                                ]
                            )
                        , msgConstructor tokenType
                        )
                    )
            )


cryptoTypeDropdown : DisplayProfile -> String -> (String -> Msg) -> (Currencies.Symbol -> Msg) -> Element Msg
cryptoTypeDropdown dProfile searchInput searchChangedMsg selectedMsg =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        CloseModals
    <|
        EH.searchableOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 18
            , Element.alignRight |> changeForMobile Element.centerX dProfile
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


fiatTypeDropdown : DisplayProfile -> String -> (String -> Msg) -> (Currencies.Symbol -> Msg) -> Element Msg
fiatTypeDropdown dProfile searchInput searchChangedMsg selectedMsg =
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        CloseModals
    <|
        EH.searchableOpenDropdown
            [ Element.width <| Element.px 300
            , Element.moveDown 18
            , Element.alignRight |> changeForMobile Element.centerX dProfile
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


typeDropdownButton : DisplayProfile -> Bool -> CurrencyType -> Msg -> Element Msg
typeDropdownButton dProfile dropdownOpen currencyType onClick =
    Element.row
        [ Element.Background.color <| Element.rgb 0.98 0.98 0.98
        , Element.height Element.fill
        , Element.padding (13 |> changeForMobile 8 dProfile)
        , Element.spacing (13 |> changeForMobile 10 dProfile)
        , Element.pointer
        , EH.onClickNoPropagation onClick
        ]
        [ Currencies.image (currencySymbol currencyType)
            |> Maybe.map
                (Images.toElement
                    [ Element.height <| Element.px (26 |> changeForMobile 18 dProfile) ]
                )
            |> Maybe.withDefault Element.none
        , Element.text <| currencySymbol currencyType
        , Images.toElement
            [ Element.width <| Element.px (12 |> changeForMobile 9 dProfile) ]
          <|
            if dropdownOpen then
                Images.upArrow

            else
                Images.downArrow
        ]


cryptoAddressInput : DisplayProfile -> Currencies.Symbol -> String -> Element Msg
cryptoAddressInput dProfile symbol input =
    EH.withInputHeader dProfile
        [ Element.width Element.fill ]
        (symbol ++ " Receive Address")
    <|
        inputContainer dProfile
            [ Element.width Element.fill
            ]
            [ Element.Input.text
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.width 0
                ]
                { onChange = ReceiveAddressChanged
                , text = input
                , placeholder = Nothing
                , label = Element.Input.labelHidden "receive address"
                }
            ]


paymentMethodInput : DisplayProfile -> BuyerOrSeller -> String -> Element Msg
paymentMethodInput dProfile initiatorRole input =
    EH.withInputHeader dProfile
        [ Element.width Element.fill ]
        (case initiatorRole of
            Buyer ->
                "Making the Payment"

            Seller ->
                "Accepting the Payment"
        )
    <|
        inputContainer dProfile
            [ Element.height <| Element.px 134
            , Element.width Element.fill
            ]
            [ Element.Input.multiline
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Border.width 0
                , Element.scrollbarY
                ]
                { onChange = PaymentMethodChanged
                , text = input
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color EH.placeholderTextColor ]
                            (Element.column
                                [ Element.spacing 5 ]
                             <|
                                List.map
                                    (Element.paragraph
                                        [ Element.spacing 0
                                        , Element.Font.size (16 |> changeForMobile 12 dProfile)
                                        ]
                                    )
                                    (case initiatorRole of
                                        Buyer ->
                                            [ [ Element.text "Indicate here how you will send payment to the Seller. Some examples:" ]
                                            , [ Element.text "\"I can send to any EU bank\"" ]
                                            , [ Element.text "\"I'll reveal a hidden cash drop in St. James Park, London\"" ]
                                            , [ Element.text "\"Can send via ecocash\"" ]
                                            ]

                                        Seller ->
                                            [ [ Element.text "Indicate here how you will send payment to the Buyer. Some examples:" ]
                                            , [ Element.text "\"I have TransferWise\"" ]
                                            , [ Element.text "\"I can pick up a cash drop in St. James Park, London\"" ]
                                            , [ Element.text "\"I can receive a WorldRemit payment to Zimbabwe\"" ]
                                            ]
                                    )
                            )
                , label = Element.Input.labelHidden "payment method"
                , spellcheck = True
                }
            ]


intervalsElement : DisplayProfile -> Model -> Element Msg
intervalsElement dProfile model =
    case dProfile of
        Desktop ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing 23
                ]
                [ phaseWindowBoxAndMaybeModal dProfile Expiry model
                , phaseWindowBoxAndMaybeModal dProfile Payment model
                , phaseWindowBoxAndMaybeModal dProfile Judgment model
                ]

        Mobile ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing 18
                ]
                [ phaseWindowBoxAndMaybeModal dProfile Expiry model
                , phaseWindowBoxAndMaybeModal dProfile Payment model
                , phaseWindowBoxAndMaybeModal dProfile Judgment model
                ]


phaseWindowBoxAndMaybeModal : DisplayProfile -> IntervalType -> Model -> Element Msg
phaseWindowBoxAndMaybeModal dProfile intervalType model =
    let
        showModal =
            model.showIntervalModal == Just intervalType
    in
    Element.el
        [ Element.width Element.fill
        , Element.above <|
            if showModal then
                intervalModal
                    dProfile
                    intervalType
                    (initiatorRole model.mode)
                    (getUserInterval intervalType model)
                    model.inputs.interval
                    model.errors.interval

            else
                Element.none
        ]
    <|
        phaseWindowBox
            dProfile
            intervalType
            (getUserInterval intervalType model)
            showModal


phaseWindowBox : DisplayProfile -> IntervalType -> UserInterval -> Bool -> Element Msg
phaseWindowBox dProfile intervalType interval modalIsOpen =
    let
        titleText =
            case intervalType of
                Expiry ->
                    "Offer Expiry"

                Payment ->
                    "Payment Due"

                Judgment ->
                    "Burn Window"

        phaseBox =
            inputContainer dProfile
                [ Element.width Element.fill
                , Element.pointer
                , EH.onClickNoPropagation (WindowBoxClicked intervalType)
                ]
                [ Element.el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Element.Background.color EH.white
                    , Element.paddingXY 15 17 |> changeForMobile (Element.padding 6) dProfile
                    ]
                    (userInterval dProfile interval)
                , dropdownArrow modalIsOpen
                ]
    in
    case dProfile of
        Desktop ->
            EH.withInputHeader dProfile
                [ Element.width Element.fill ]
                titleText
                phaseBox

        Mobile ->
            Element.row
                [ Element.width Element.fill ]
                [ Element.el
                    [ Element.Font.size 16
                    , Element.Font.bold
                    , Element.width Element.fill
                    ]
                    (Element.text titleText)
                , phaseBox
                ]


userInterval : DisplayProfile -> UserInterval -> Element Msg
userInterval dProfile interval =
    Element.el
        [ Element.Font.size (20 |> changeForMobile 16 dProfile)
        , Element.Font.medium
        , Element.centerY
        ]
    <|
        Element.text <|
            String.fromInt interval.num
                ++ " "
                ++ intervalUnitToString interval.unit
                ++ (if interval.num == 1 then
                        ""

                    else
                        "s"
                   )


intervalModal : DisplayProfile -> IntervalType -> BuyerOrSeller -> UserInterval -> String -> Maybe String -> Element Msg
intervalModal dProfile intervalType userRole value input maybeError =
    let
        ( title, text ) =
            case intervalType of
                Expiry ->
                    ( "Offer Expiry"
                    , "This is how long your offer remains valid and visible in the marketplace. Note that you can manually recall the trade at any time, as long as no one has yet committed to it."
                    )

                Payment ->
                    ( "Payment Due"
                    , case userRole of
                        Buyer ->
                            "Once a Seller commits, this is how long you to work with the Seller to complete the payment, and click \"confirm payment\". We recommend setting this to about 2X or 3X more than you expect you'll need!"

                        Seller ->
                            "Once a Buyer commits, this is how long they have to work with you to complete the payment and click \"confirm payment\". We recommend setting this to about 2X or 3X more than you expect you'll need!"
                    )

                Judgment ->
                    ( "Burn Window"
                    , case userRole of
                        Buyer ->
                            "Once you confirm payment, this is how long the Seller will have the option to burn the entire Sai/xDai balance (or manually release early). If the Seller makes no decision before this timer expires, the Sai/xDai balance is yours to claim."

                        Seller ->
                            "Once the Buyer confirms payment, this is how long you will have the option to burn the entire Sai/xDai balance (or manually release early). If you don't make a decision before this timer expires, the Buyer can then claim the Sai/xDai balance."
                    )
    in
    EH.modal
        (Element.rgba 0 0 0 0.1)
        False
        NoOp
        CloseModals
    <|
        Element.column
            [ Element.moveUp 10
            , Element.Border.rounded 8
            , Element.Background.color EH.lightGray
            , Element.spacing 1
            , Element.clip
            , EH.moveToFront
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 0
                , blur = 20
                , color = Element.rgba 0 0 0 0.08
                }
            , (if intervalType == Judgment then
                Element.alignRight

               else
                Element.alignLeft
              )
                |> changeForMobile Element.centerX dProfile
            ]
            [ Element.el
                [ Element.paddingXY 23 18 |> changeForMobile (Element.paddingXY 18 16) dProfile
                , Element.Background.color EH.white
                , Element.width Element.fill
                ]
              <|
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]
                    [ Element.el
                        [ Element.Font.size (20 |> changeForMobile 18 dProfile)
                        , Element.Font.semiBold
                        , Element.Font.color <| Element.rgb255 16 7 234
                        ]
                        (Element.text title)
                    , Element.paragraph
                        [ Element.Font.size (16 |> changeForMobile 14 dProfile)
                        , Element.Font.color <| Element.rgba 0 0 0 0.75
                        , Element.spacing 2
                        ]
                        [ Element.text text ]
                    ]
            , let
                timeUnitButton : IntervalUnit -> Bool -> Element Msg
                timeUnitButton unit selected =
                    button dProfile
                        (if selected then
                            Element.rgb255 16 7 234

                         else
                            Element.rgba255 10 33 108 0.04
                        )
                        (if selected then
                            EH.white

                         else
                            Element.rgb255 10 33 108
                        )
                        (intervalUnitToString unit
                            ++ "s"
                            |> String.toLower
                        )
                        (if selected then
                            Nothing

                         else
                            Just <|
                                IntervalUnitChanged unit
                        )

                phaseBox =
                    inputContainer dProfile
                        [ Element.width <| Element.px 70
                        , Element.above <|
                            case maybeError of
                                Just error ->
                                    Element.el
                                        [ Element.Font.size 12
                                        , Element.Font.color EH.softRed
                                        , Element.moveUp 16
                                        , Element.alignLeft
                                        , Element.Background.color EH.white
                                        , Element.Border.widthEach
                                            { top = 0
                                            , bottom = 0
                                            , right = 1
                                            , left = 1
                                            }
                                        , Element.paddingXY 5 0
                                        , Element.Border.color EH.lightGray
                                        ]
                                        (Element.text error)

                                Nothing ->
                                    Element.none
                        ]
                        [ Element.Input.text
                            [ Element.Border.width 0
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            { onChange = IntervalInputChanged
                            , text = input
                            , placeholder = Nothing
                            , label = Element.Input.labelHidden (title ++ " input")
                            }
                        ]
              in
              case dProfile of
                Desktop ->
                    Element.row
                        [ Element.paddingXY 23 18
                        , Element.Background.color EH.white
                        , Element.spacing 12
                        ]
                        [ phaseBox
                        , timeUnitButton Minute (value.unit == Minute)
                        , timeUnitButton Hour (value.unit == Hour)
                        , timeUnitButton Day (value.unit == Day)
                        , timeUnitButton Week (value.unit == Week)
                        ]

                Mobile ->
                    Element.row
                        [ Element.paddingXY 18 15
                        , Element.Background.color EH.white
                        , Element.spacing 7
                        , Element.centerX
                        ]
                        [ phaseBox
                        , Element.wrappedRow
                            [ Element.spacing 8
                            , Element.width Element.fill
                            ]
                            [ timeUnitButton Minute (value.unit == Minute)
                            , timeUnitButton Hour (value.unit == Hour)
                            , timeUnitButton Day (value.unit == Day)
                            , timeUnitButton Week (value.unit == Week)
                            ]
                        ]
            ]


dropdownArrow : Bool -> Element Msg
dropdownArrow pointUp =
    Element.el
        [ Element.alignRight
        , Element.height Element.fill
        , Element.Background.color <| Element.rgb 0.98 0.98 0.98
        , Element.padding 13
        ]
    <|
        Images.toElement
            [ Element.centerY ]
            (if pointUp then
                Images.upArrow

             else
                Images.downArrow
            )


inputContainer : DisplayProfile -> List (Element.Attribute Msg) -> List (Element Msg) -> Element Msg
inputContainer dProfile attributes =
    Element.row <|
        [ Element.Background.color EH.lightGray
        , Element.height <| Element.px (55 |> changeForMobile 40 dProfile)
        , Element.Border.rounded 4
        , Element.Border.width 1
        , Element.Border.color EH.lightGray
        , Element.spacing 1
        ]
            ++ attributes
            ++ (case dProfile of
                    Desktop ->
                        []

                    Mobile ->
                        [ Element.Font.size 14 ]
               )


placeOrderButton : DisplayProfile -> Model -> Element Msg
placeOrderButton dProfile model =
    let
        buttonBuilder bgColor textColor text maybeOnClick maybeError =
            Element.el
                ([ Element.width Element.fill
                 , Element.padding (17 |> changeForMobile 10 dProfile)
                 , Element.Border.rounded 4
                 , Element.Font.size (20 |> changeForMobile 16 dProfile)
                 , Element.Font.semiBold
                 , Element.Font.center
                 , Element.Background.color bgColor
                 , Element.Font.color textColor
                 , EH.noSelectText
                 , Element.above <|
                    case maybeError of
                        Just error ->
                            Element.el
                                [ Element.Font.size (12 |> changeForMobile 10 dProfile)
                                , Element.Font.color EH.softRed
                                , Element.moveUp 16
                                , Element.centerX
                                ]
                                (Element.text error)

                        Nothing ->
                            Element.none
                 ]
                    ++ (case maybeOnClick of
                            Just onClick ->
                                [ Element.pointer
                                , Element.Events.onClick onClick
                                ]

                            Nothing ->
                                []
                       )
                )
                (Element.text text)
    in
    case Wallet.userInfo model.wallet of
        Just userInfo ->
            if Wallet.factory model.wallet == Just model.dhTokenType then
                case maybeUserParameters model of
                    Just userParameters ->
                        buttonBuilder
                            (Element.rgb255 255 0 110)
                            EH.white
                            "Proceed With This Offer"
                            (Just <| PlaceOrderClicked model.dhTokenType userInfo userParameters)
                            Nothing

                    Nothing ->
                        buttonBuilder
                            EH.lightGray
                            EH.black
                            "Proceed With This Offer"
                            Nothing
                            Nothing

            else
                Element.paragraph
                    [ Element.Font.size 18
                    , Element.Font.italic
                    , Element.Font.color EH.darkGray
                    , Element.centerX
                    ]
                    [ Element.text <|
                        "You must switch your wallet to the "
                            ++ networkNameForFactory model.dhTokenType
                            ++ " network to create a trade with "
                            ++ tokenUnitName model.dhTokenType
                            ++ "."
                    ]

        Nothing ->
            buttonBuilder
                EH.softRed
                EH.white
                "Connect to Wallet"
                (Just <| CmdUp CmdUp.Web3Connect)
                Nothing


txChainStatusModal : DisplayProfile -> TxChainStatus -> Model -> Element Msg
txChainStatusModal dProfile txChainStatus model =
    let
        confirmAbortAttributes =
            if txChainStatus.confirmingAbort then
                let
                    message =
                        case txChainStatus.mode of
                            Confirm _ _ ->
                                ""

                            ApproveNeedsSig _ ->
                                ""

                            ApproveMining _ _ _ ->
                                "If you abort now, your deposit will not complete and the offer won't be created."

                            CreateNeedsSig _ ->
                                "If you abort now, your deposit will not complete and the offer won't be created."

                            CreateMining _ _ ->
                                "Aborting now will return you to the Create form, but WILL NOT cancel the offer creation call. To manage the call, check your web3 wallet."
                in
                [ Element.inFront <|
                    Element.column
                        [ Element.centerX
                        , Element.centerY
                        , Element.padding 10
                        , Element.spacing 10
                        , Element.Border.rounded 5
                        , Element.Background.color EH.white
                        ]
                        [ Element.paragraph
                            [ Element.width Element.fill ]
                            [ Element.text message ]
                        , Element.row
                            [ Element.centerX
                            , Element.spacing 10
                            ]
                            [ EH.redButton
                                dProfile
                                []
                                [ "Abort" ]
                                ConfirmCloseTxModalClicked
                            , EH.blueButton
                                dProfile
                                []
                                [ "Nevermind" ]
                                NevermindCloseTxModalClicked
                            ]
                        ]
                ]

            else
                []
    in
    case txChainStatus.mode of
        Confirm factoryType createParameters ->
            createConfirmModal dProfile model factoryType createParameters

        ApproveNeedsSig tokenType ->
            Element.el
                (confirmAbortAttributes
                    ++ [ Element.centerX
                       , Element.centerY
                       , Element.Events.onClick <|
                            CmdUp <|
                                CmdUp.gTag "txChainModal clicked" "misclick" "ApproveNeedsSig" 0
                       ]
                )
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the approve call."
                    , Element.text "(check your web3 wallet!)"
                    , Element.text "Note that there will be a second transaction to sign after this."
                    ]
                    NoOp
                    CloseTxModalClicked
                    NoOp

        ApproveMining tokenType createParameters txHash ->
            Element.el
                (confirmAbortAttributes
                    ++ [ Element.centerX
                       , Element.centerY
                       , Element.Events.onClick <|
                            CmdUp <|
                                CmdUp.gTag "txChainModal clicked" "misclick" "ApproveMining" 0
                       ]
                )
            <|
                EH.txProcessModal
                    [ Element.text "Mining the initial approve transaction..."
                    , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                        { url = EthHelpers.makeViewTxUrl (Token tokenType) txHash
                        , label = Element.text "See the transaction on Etherscan"
                        }
                    , Element.text "Funds will not leave your wallet until you sign the next transaction."
                    ]
                    NoOp
                    CloseTxModalClicked
                    NoOp

        CreateNeedsSig _ ->
            Element.el
                (confirmAbortAttributes
                    ++ [ Element.centerX
                       , Element.centerY
                       , Element.Events.onClick <|
                            CmdUp <|
                                CmdUp.gTag "txChainModal clicked" "misclick" "CreateNeedsSig" 0
                       ]
                )
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the create call."
                    , Element.text "(check your web3 wallet!)"
                    ]
                    NoOp
                    CloseTxModalClicked
                    NoOp

        CreateMining factoryType txHash ->
            Element.el
                (confirmAbortAttributes
                    ++ [ Element.centerX
                       , Element.centerY
                       , Element.Events.onClick <|
                            CmdUp <|
                                CmdUp.gTag "txChainModal clicked" "misclick" "CreateMining" 0
                       ]
                )
            <|
                EH.txProcessModal
                    [ Element.text "Mining the final create call..."
                    , Element.newTabLink [ Element.Font.underline, Element.Font.color EH.blue ]
                        { url = EthHelpers.makeViewTxUrl factoryType txHash
                        , label = Element.text "See the transaction on Etherscan"
                        }
                    , Element.text "You will be redirected when it's mined."
                    ]
                    NoOp
                    CloseTxModalClicked
                    NoOp


createConfirmModal : DisplayProfile -> Model -> FactoryType -> CTypes.CreateParameters -> Element Msg
createConfirmModal dProfile model factoryType createParameters =
    let
        ( depositAmountStr, totalBurnableStr, confirmButton ) =
            case model.depositAmount of
                Just depositAmount ->
                    let
                        depositAmountText =
                            TokenValue.toConciseString depositAmount
                                ++ " "
                                ++ tokenUnitName factoryType
                    in
                    ( depositAmountText
                    , TokenValue.toConciseString
                        (TokenValue.add
                            depositAmount
                            (CTypes.getResponderDeposit createParameters)
                            |> TokenValue.add (CTypes.calculateDHFee createParameters)
                        )
                        ++ " "
                        ++ tokenUnitName factoryType
                    , EH.redButton dProfile
                        [ Element.width Element.fill
                        , Element.alignBottom
                        ]
                        (case dProfile of
                            Desktop ->
                                [ "Deposit "
                                    ++ depositAmountText
                                    ++ " and create this offer."
                                ]

                            Mobile ->
                                [ "Deposit "
                                    ++ depositAmountText
                                , "and create this offer."
                                ]
                        )
                        (ConfirmCreate factoryType createParameters depositAmount)
                    )

                Nothing ->
                    ( "??"
                    , "??"
                    , EH.disabledButton dProfile
                        [ Element.width Element.fill
                        , Element.alignBottom
                        ]
                        "(loading exact fees...)"
                        Nothing
                    )

        feeAmountStr =
            TokenValue.toConciseString (CTypes.calculateDHFee createParameters)
                ++ " "
                ++ tokenUnitName factoryType

        tradeAmountStr =
            TokenValue.toConciseString createParameters.tradeAmount
                ++ " "
                ++ tokenUnitName factoryType

        -- notYetButton =
        --     Element.el
        --         [ Element.pointer
        --         , Element.Events.onClick AbortCreate
        --         , Element.paddingXY 25 17
        --         , Element.Font.color EH.white
        --         , Element.Font.size 18
        --         , Element.Font.semiBold
        --         ]
        --         (Element.text "I'm not ready yet. Go back.")
        buyerDepositStr =
            TokenValue.toConciseString createParameters.buyerDeposit
                ++ " "
                ++ tokenUnitName factoryType

        totalReleaseableStr =
            TokenValue.toConciseString (TokenValue.add createParameters.tradeAmount createParameters.buyerDeposit)
                ++ " "
                ++ tokenUnitName factoryType

        expiryWindowStr =
            userIntervalToString <|
                getUserInterval Expiry model

        paymentWindowStr =
            userIntervalToString <|
                getUserInterval Payment model

        judgmentWindowStr =
            userIntervalToString <|
                getUserInterval Judgment model

        priceStr =
            Currencies.toString createParameters.price

        emphasizedText =
            Element.el
                [ Element.Font.extraBold
                , Element.Font.color EH.white
                ]
                << Element.text

        counterpartyRoleText =
            case createParameters.initiatorRole of
                Buyer ->
                    "seller"

                Seller ->
                    "buyer"

        ( abortPunishmentStr, abortSellerReturnStr, abortBuyerReturnStr ) =
            let
                abortPunishment =
                    CTypes.defaultAbortPunishment createParameters.tradeAmount
            in
            TupleHelpers.mapTuple3
                (TokenValue.toConciseString
                    >> (\s -> s ++ " " ++ tokenUnitName factoryType)
                )
                ( abortPunishment
                , TokenValue.sub createParameters.tradeAmount abortPunishment
                , TokenValue.sub createParameters.buyerDeposit abortPunishment
                )

        title =
            Element.el
                [ Element.centerX
                , Element.Font.size (24 |> changeForMobile 20 dProfile)
                , Element.Font.semiBold
                ]
            <|
                Element.text <|
                    case model.extraConfirmInfoPlace of
                        0 ->
                            "Are you Ready?"

                        1 ->
                            "1. Offer Creation"

                        2 ->
                            "2. Commitment or Recall"

                        3 ->
                            case createParameters.initiatorRole of
                                Buyer ->
                                    "3. Send Payment"

                                Seller ->
                                    "3. Receive Payment"

                        4 ->
                            "4. Judgment"

                        5 ->
                            "5. Resolution and Reputation"

                        _ ->
                            ""

        arrows =
            let
                maybeLeftArrow =
                    if model.extraConfirmInfoPlace <= 0 then
                        Element.none

                    else
                        Images.toElement
                            [ Element.pointer
                            , Element.Events.onClick TradeTermsLeft
                            , Element.width Element.fill
                            ]
                            Images.navigateLeft

                maybeRightArrow =
                    if model.extraConfirmInfoPlace >= 5 then
                        Element.none

                    else
                        Images.toElement
                            [ Element.pointer
                            , Element.Events.onClick TradeTermsRight
                            , Element.width Element.fill
                            ]
                            Images.navigateRight
            in
            Element.row
                [ Element.spacing 80
                , Element.centerX
                , Element.height <| Element.px 23
                ]
                [ Element.el [ Element.width <| Element.px 46 ]
                    maybeLeftArrow
                , Element.el [ Element.width <| Element.px 46 ]
                    maybeRightArrow
                ]

        infoBody =
            Element.column
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.size (16 |> changeForMobile 14 dProfile)
                        , Element.Font.color (Element.rgba 1 1 1 0.85)
                        , Element.spacing 2
                        ]
                    )
                    (case model.extraConfirmInfoPlace of
                        0 ->
                            [ [ Element.text "DAIHard is different than other exchanges. It can get pretty hardcore. If this is your first trade here, click the right arrow above to get an idea of how a DAIHard trade works." ]
                            , [ Element.text "Once someone commits to your trade, there's no going back!" ]
                            ]

                        1 ->
                            (case createParameters.initiatorRole of
                                Buyer ->
                                    [ [ Element.text <| "Your offer to buy "
                                      , emphasizedText <| tradeAmountStr ++ " for " ++ priceStr
                                      , Element.text <| " will be listed on the marketplace. "
                                      , emphasizedText <| "This requires a Buyer Deposit of " ++ depositAmountStr ++ "."
                                      ]
                                    ]

                                Seller ->
                                    [ [ Element.text <| "Your offer to sell "
                                      , emphasizedText <| tradeAmountStr ++ " for " ++ priceStr
                                      , Element.text <| " will be listed on the marketplace, and an additional " ++ feeAmountStr ++ " will be set aside. "
                                      , emphasizedText <| "This requires a total deposit of " ++ depositAmountStr ++ "."
                                      ]
                                    ]
                            )
                                ++ [ [ Element.text "Your offer will remain valid and listed on the marketplace for "
                                     , emphasizedText expiryWindowStr
                                     , Element.text <| "."
                                     ]
                                   ]

                        2 ->
                            [ [ Element.text "Until the offer expires, another DAIHard user can "
                              , emphasizedText "commit to the trade"
                              , Element.text " by depositing "
                              ]
                                ++ (case createParameters.initiatorRole of
                                        Buyer ->
                                            [ Element.text <| "the full sell amount of " ++ tradeAmountStr
                                            ]

                                        Seller ->
                                            [ Element.text buyerDepositStr
                                            ]
                                   )
                                ++ [ Element.text ". This immediately moves the trade to the "
                                   , emphasizedText "Payment Phase"
                                   , Element.text <| ", with the other user as the " ++ counterpartyRoleText ++ "."
                                   ]
                            , [ Element.text <| "At any point before a " ++ counterpartyRoleText ++ " commits, you can "
                              , emphasizedText "recall the offer for a full refund"
                              , Element.text <| " of the " ++ depositAmountStr ++ " you deposited."
                              ]
                            ]

                        3 ->
                            case createParameters.initiatorRole of
                                Buyer ->
                                    [ [ Element.text "Here you are expected to "
                                      , emphasizedText <| "send the full " ++ priceStr ++ " to the seller"
                                      , Element.text " as you've described in your payment methods, then click \"Confirm Payment\". "
                                      , Element.text "This will move the trade to the "
                                      , emphasizedText "Judgment Phase"
                                      , Element.text ". Encrypted chat will be available to help coordinate."
                                      ]
                                    , [ Element.text <| "If you do not Confirm Payment within "
                                      , emphasizedText paymentWindowStr
                                      , Element.text ", the trade will automatically abort, "
                                      , emphasizedText <| "burning " ++ abortPunishmentStr ++ " from each party"
                                      , Element.text <| " and returning the remainder (" ++ abortBuyerReturnStr ++ " to you and " ++ abortSellerReturnStr ++ " to the seller)."
                                      ]
                                    ]

                                Seller ->
                                    [ [ Element.text "Here, the buyer is expected to "
                                      , emphasizedText <| "send the full " ++ priceStr ++ " to you "
                                      , Element.text "as you've described in your payment methods, then move the trade to the "
                                      , emphasizedText "Judgment Phase"
                                      , Element.text ". Encrypted chat will be available to help coordinate."
                                      ]
                                    , [ Element.text <| "If the buyer does not Confirm Payment within "
                                      , emphasizedText paymentWindowStr
                                      , Element.text ", the trade will automatically abort, "
                                      , emphasizedText <| "burning " ++ abortPunishmentStr ++ " from each party"
                                      , Element.text <| " and returning the remainder (" ++ abortSellerReturnStr ++ " to you and " ++ abortBuyerReturnStr ++ " to the buyer)."
                                      ]
                                    ]

                        4 ->
                            case createParameters.initiatorRole of
                                Buyer ->
                                    [ [ Element.text <| "In this phase, the seller will have the option to "
                                      , emphasizedText <| "burn (i.e. destroy) the full trade balance of " ++ totalReleaseableStr ++ "."
                                      ]
                                    , [ Element.text "The seller is expected to do this "
                                      , emphasizedText <| "if and only if"
                                      , Element.text <| " you did not sent the " ++ priceStr ++ " as described in your payment methods."
                                      ]
                                    , [ Element.text <| "If the seller doesn't make a decision within this burn window of "
                                      , emphasizedText judgmentWindowStr
                                      , Element.text ", "
                                      , Element.text <| "the trade automatically concludes and the " ++ totalReleaseableStr ++ " is yours to claim."
                                      ]
                                    ]

                                Seller ->
                                    [ [ Element.text <| "In this phase, you will have the option to "
                                      , emphasizedText <| "burn (i.e. destroy) the full trade balance of " ++ totalReleaseableStr ++ "."
                                      ]
                                    , [ Element.text "You are expected to do this "
                                      , emphasizedText <| "if and only if"
                                      , Element.text <| " the buyer did not sent the " ++ priceStr ++ " as described in your payment methods."
                                      ]
                                    , [ Element.text <| "In "
                                      , emphasizedText judgmentWindowStr
                                      , Element.text ", you will lose this option, and "
                                      , Element.text <| " buyer can withdraw the full " ++ totalReleaseableStr ++ ". You can also manually release the " ++ totalReleaseableStr ++ " early."
                                      ]
                                    ]

                        5 ->
                            case createParameters.initiatorRole of
                                Buyer ->
                                    [ [ Element.text <| "The trade has concluded. If it was successful, DAIHard takes 1% of the trade amount (" ++ feeAmountStr ++ ") from your initial deposit."
                                      , Element.text <| "If the trade was burned, then this fee is burned as well."
                                      ]
                                    , [ Element.text "The result of this trade (burn, abort, or release) is recorded for both users, and displayed next to any new offers they make." ]
                                    ]

                                Seller ->
                                    [ [ Element.text <| "The trade has concluded. If it was successful, DAIHard takes the 1% fee (" ++ feeAmountStr ++ ") set aside earlier."
                                      , Element.text " If the trade was burned, then this fee is burned as well."
                                      ]
                                    , [ Element.text "The result of this trade (burn, abort, or release) is recorded for both users, and displayed next to any new offers they make." ]
                                    ]

                        _ ->
                            []
                    )
                )

        placeDots =
            let
                smallDot color =
                    Element.el
                        [ Element.width <| Element.px 8
                        , Element.height <| Element.px 8
                        , Element.Border.rounded 4
                        , Element.Background.color color
                        ]
                        Element.none
            in
            Element.row
                [ Element.centerX
                , Element.spacing 6
                ]
                (List.range 0 5
                    |> List.map
                        (\i ->
                            if i == model.extraConfirmInfoPlace then
                                smallDot <| Element.rgba 1 1 1 1

                            else
                                smallDot <| Element.rgba 1 1 1 0.25
                        )
                )
    in
    EH.closeableModalWhiteX
        [ Element.width (Element.px 540 |> changeForMobile Element.fill dProfile)
        , Element.Background.color <| Element.rgb255 10 33 108
        , Element.padding (60 |> changeForMobile 20 dProfile)
        , Element.height (Element.shrink |> changeForMobile Element.fill dProfile)
        ]
        (Element.column
            [ Element.spacing 15
            , Element.width Element.fill
            , Element.Font.color EH.white
            , Element.height (Element.shrink |> changeForMobile Element.fill dProfile)
            ]
            [ Images.toElement
                [ Element.height <| Element.px 70
                , Element.centerX
                ]
                (Images.confirmExtraInfoIcon model.extraConfirmInfoPlace)
            , title
            , placeDots
            , arrows
            , infoBody
            , confirmButton
            ]
        )
        NoOp
        AbortCreate
        False


viewModals : DisplayProfile -> Model -> List (Element Msg)
viewModals dProfile model =
    case model.txChainStatus of
        Just txChainStatus ->
            [ txChainStatusModal dProfile txChainStatus model ]

        Nothing ->
            []
