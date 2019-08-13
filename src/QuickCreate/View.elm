module QuickCreate.View exposing (menuItemElement, recipeSummaryElement, root)

import AppCmd exposing (AppCmd)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FiatValue exposing (FiatValue)
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Images exposing (Image)
import List.Extra
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import QuickCreate.Types exposing (..)
import Time
import TokenValue exposing (TokenValue)


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( case model.state of
        Menu menuState ->
            Element.column
                [ Element.spacing 10
                , Element.centerX
                ]
                (menuItems
                    |> List.map
                        (menuItemElement
                            (EthHelpers.factoryTypeWithDefault model.userInfo)
                            True
                        )
                )

        Spec recipe specState ->
            Element.column
                [ Element.spacing 20
                , Element.centerX
                ]
                [ menuItemElement
                    (EthHelpers.factoryTypeWithDefault model.userInfo)
                    False
                    recipe
                , Element.column
                    [ Element.spacing 10
                    , Element.centerX
                    ]
                    [ textInputPrompt recipe.initiatorRole recipe.fiatValue
                    , textInputElement recipe.initiatorRole model.textInput
                    , openTradeButton (EthHelpers.factoryTypeWithDefault model.userInfo) model.userInfo model.state
                    ]
                ]
    , [ txModalOrNone (EthHelpers.factoryTypeWithDefault model.userInfo) model.userInfo model.state ]
    )


menuItemElement : FactoryType -> Bool -> TradeRecipe -> Element Msg
menuItemElement factoryType showButtons recipe =
    Element.row
        [ Element.spacing 40 ]
        [ recipeSummaryElement factoryType recipe
        , if showButtons then
            startButton factoryType recipe

          else
            Element.none
        ]


startButton : FactoryType -> TradeRecipe -> Element Msg
startButton factoryType recipe =
    EH.blueButton "Start" (StartClicked factoryType recipe)


recipeSummaryElement : FactoryType -> TradeRecipe -> Element Msg
recipeSummaryElement factoryType recipe =
    let
        tradeInputGraphic =
            case recipe.initiatorRole of
                Buyer ->
                    Element.column
                        [ Element.spacing 5 ]
                        [ daiValue factoryType recipe.daiAmountIn
                        , dollarValue recipe.fiatValue
                        ]

                Seller ->
                    daiValue factoryType recipe.daiAmountIn

        tradeOutputGraphic =
            case recipe.initiatorRole of
                Buyer ->
                    daiValue factoryType <|
                        TokenValue.add
                            (recipeTradeAmount recipe)
                            (recipeBuyerDeposit recipe)

                Seller ->
                    dollarValue recipe.fiatValue

        arrowElement =
            Element.text "--->"

        maxTimeElement =
            recipe.intervals
                |> (\( a, b, c ) ->
                        a
                            |> TimeHelpers.add b
                            |> TimeHelpers.add c
                   )
                |> summarizeIntervalElement
    in
    Element.column
        [ Element.spacing 8 ]
        [ Element.row
            [ Element.spacing 4 ]
            [ tradeInputGraphic
            , arrowElement
            , tradeOutputGraphic
            ]
        , maxTimeElement
        ]


summarizeIntervalElement : Time.Posix -> Element Msg
summarizeIntervalElement interval =
    if interval == Time.millisToPosix (1000 * 60 * 60 * 24 * 7) then
        Element.text "1 week"

    else
        Debug.todo "lol bet u forgot about dis. summarizeIntervalElement is a hack! A dirty hack!!"


textInputPrompt : BuyerOrSeller -> FiatValue -> Element Msg
textInputPrompt initiatorRole fiatValue =
    [ case initiatorRole of
        Seller ->
            [ Element.text "How must the "
            , dollarValue fiatValue
            , Element.text " be paid?"
            ]

        Buyer ->
            [ Element.text "How can you deliver the "
            , dollarValue fiatValue
            , Element.text "?"
            ]
    , [ Element.text "You will be able to chat with the "
      , Element.text <|
            case initiatorRole of
                Seller ->
                    "buyer"

                Buyer ->
                    "seller"
      , Element.text " to coordinate further once the trade is in progress."
      ]
    ]
        |> List.map (Element.paragraph [])
        |> Element.column [ Element.spacing 10 ]


textInputElement : BuyerOrSeller -> String -> Element Msg
textInputElement initiatorRole text =
    Element.Input.multiline
        [ Element.width Element.fill
        , Element.height <| Element.px 200
        ]
        { onChange = TextInputChanged
        , text = text
        , placeholder =
            if text == "" then
                Just <| textInputPlaceholder initiatorRole

            else
                Nothing
        , label = Element.Input.labelHidden "payment details"
        , spellcheck = True
        }


textInputPlaceholder : BuyerOrSeller -> Element.Input.Placeholder Msg
textInputPlaceholder initiatorRole =
    (case initiatorRole of
        Buyer ->
            [ Element.text "Some ideas:"
            , Element.el [ Element.Font.italic ] <| Element.text "I can meet in NYC central park for a cash handoff after 6 pm."
            , Element.el [ Element.Font.italic ] <| Element.text "I'll send you $7 in Steam credit."
            , Element.el [ Element.Font.italic ] <| Element.text "TransferWise"
            , Element.el [ Element.Font.italic ] <| Element.text "Cash drop in Hume Park, Bulawayo"
            ]

        Seller ->
            [ Element.text "Some ideas:"
            , Element.el [ Element.Font.italic ] <| Element.text "I accept money orders or cashier checks into my Schwab bank account."
            , Element.el [ Element.Font.italic ] <| Element.text "Meet me at the Bangkok airport (BKK) on Saturday. I'm coming in on flight I453."
            , Element.el [ Element.Font.italic ] <| Element.text "Buy Dig Dog on Steam and gift it to me (syriven)."
            , Element.el [ Element.Font.italic ] <| Element.text "Ship me a potato."
            ]
    )
        |> List.map (Element.paragraph [] << List.singleton)
        |> Element.column [ Element.spacing 5 ]
        |> Element.Input.placeholder []


openTradeButton : FactoryType -> Maybe UserInfo -> State -> Element Msg
openTradeButton factoryType maybeUserInfo state =
    case maybeUserInfo of
        Just userInfo ->
            case state of
                Spec recipe specState ->
                    case specState of
                        ApproveMining txHash ->
                            EH.disabledButton "Mining Prepare tx..." Nothing

                        ReadyToOpen ->
                            EH.redButton "Open Trade" (OpenClicked factoryType userInfo recipe)

                        OpenNeedsSig ->
                            Element.none

                        OpenMining ->
                            Element.none

                Menu _ ->
                    Element.none

        Nothing ->
            EH.disabledButton "Can't find userInfo..." (Just "You shouldn't be seeing this. Maybe your web3 provider changed something just now?")


daiValue : FactoryType -> TokenValue -> Element Msg
daiValue factoryType dai =
    Element.el
        [ Element.Font.color EH.daiYellow
        , Element.Font.size 20
        ]
        (Element.text <|
            daiValueString factoryType dai
        )


daiValueString : FactoryType -> TokenValue -> String
daiValueString factoryType dai =
    TokenValue.toConciseString dai ++ " " ++ Config.tokenUnitName factoryType


dollarValue : FiatValue -> Element Msg
dollarValue dollars =
    Element.el
        [ Element.Font.color EH.dollarGreen
        , Element.Font.size 20
        ]
        (Element.text <| dollarValueString dollars)


dollarValueString : FiatValue -> String
dollarValueString dollars =
    "$" ++ BigInt.toString dollars.amount ++ " USD"


txModalOrNone : FactoryType -> Maybe UserInfo -> State -> Element Msg
txModalOrNone factoryType maybeUserInfo state =
    (case state of
        Menu (StartPrompt tokenType recipe) ->
            Just <|
                EH.closeableModal
                    []
                    (Element.column
                        [ Element.spacing 15
                        , Element.centerX
                        , Element.padding 20
                        ]
                        [ Element.text "Text about approving"
                        , case maybeUserInfo of
                            Just userInfo ->
                                EH.blueButton
                                    ("Prepare "
                                        ++ TokenValue.toConciseString recipe.daiAmountIn
                                        ++ " "
                                        ++ Config.tokenUnitName factoryType
                                        ++ " for deposit"
                                    )
                                    (ApproveClicked tokenType recipe)

                            Nothing ->
                                EH.redButton "Connect to Wallet" Web3Connect
                        ]
                    )
                    (ChangeState <| Menu NoneStarted)

        Menu (ApproveNeedsSig tokenType recipe) ->
            Just <|
                EH.closeableModal []
                    (Element.column
                        [ Element.spacing 15
                        , Element.padding 20
                        , Element.centerX
                        ]
                        [ Element.text "Waiting for sig text"
                        ]
                    )
                    (ChangeState <| Menu NoneStarted)

        Spec recipe OpenNeedsSig ->
            Just <|
                EH.closeableModal []
                    (Element.column
                        [ Element.spacing 15
                        , Element.padding 20
                        , Element.centerX
                        ]
                        [ Element.text "Text about opening"
                        ]
                    )
                    (ChangeState <| Spec recipe ReadyToOpen)

        Spec recipe OpenMining ->
            Just <|
                EH.closeableModal []
                    (Element.column
                        [ Element.spacing 15
                        , Element.padding 20
                        , Element.centerX
                        ]
                        [ Element.text "Opening trade. Note that if you close this modal or navigate away, the transaction could still complete."
                        , Element.text "You will be redirected when the trade is finished opening."
                        ]
                    )
                    (ChangeState <| Menu NoneStarted)

        _ ->
            Nothing
    )
        |> Maybe.withDefault Element.none
