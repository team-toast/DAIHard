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


root : Model -> ( Element Msg, Element Msg )
root model =
    ( case model.state of
        Menu menuState ->
            Element.column
                [ Element.spacing 10
                , Element.centerX
                ]
                (menuItems
                    |> List.map menuItemElement
                )

        Spec recipe specState ->
            Debug.todo ""
    , txModalOrNone model.userInfo model.state
    )


menuItemElement : TradeRecipe -> Element Msg
menuItemElement recipe =
    Element.row
        [ Element.spacing 40 ]
        [ recipeSummaryElement recipe
        , startButton recipe
        ]


startButton : TradeRecipe -> Element Msg
startButton recipe =
    EH.blueButton "Start" (StartClicked recipe)


recipeSummaryElement : TradeRecipe -> Element Msg
recipeSummaryElement recipe =
    let
        tradeInputGraphic =
            case recipe.initiatorRole of
                Buyer ->
                    Element.column
                        [ Element.spacing 5 ]
                        [ daiValue recipe.daiAmountIn
                        , dollarValue recipe.fiatValue
                        ]

                Seller ->
                    daiValue recipe.daiAmountIn

        tradeOutputGraphic =
            case recipe.initiatorRole of
                Buyer ->
                    daiValue <|
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


daiValue : TokenValue -> Element Msg
daiValue dai =
    Element.el
        [ Element.Font.color EH.daiYellow
        , Element.Font.size 20
        ]
        (Element.text <|
            daiValueString dai
        )


daiValueString : TokenValue -> String
daiValueString dai =
    TokenValue.toConciseString dai ++ "D"


dollarValue : FiatValue -> Element Msg
dollarValue dollars =
    Element.el
        [ Element.Font.color EH.dollarGreen
        , Element.Font.size 20
        ]
        (Element.text <| dollarValueString dollars)


dollarValueString : FiatValue -> String
dollarValueString dollars =
    "$" ++ BigInt.toString dollars.amount


txModalOrNone : Maybe UserInfo -> State -> Element Msg
txModalOrNone maybeUserInfo state =
    (case state of
        Menu (StartPrompt recipe) ->
            Just <|
                EH.closeableModal
                    (Element.column
                        [ Element.spacing 15
                        , Element.centerX
                        ]
                        [ Element.text "Text about approving"
                        , case maybeUserInfo of
                            Just userInfo ->
                                EH.blueButton ("Prepare " ++ TokenValue.toConciseString recipe.daiAmountIn ++ " Dai for deposit") (ApproveClicked recipe)

                            Nothing ->
                                EH.redButton "Connect to Wallet" Web3Connect
                        ]
                    )
                    (ChangeState <| Menu NoneStarted)

        Menu (ApproveNeedsSig recipe) ->
            Just <|
                EH.closeableModal
                    (Element.column
                        [ Element.spacing 15
                        , Element.centerX
                        ]
                        [ Element.text "Waiting for sig text"
                        ]
                    )
                    (ChangeState <| Menu NoneStarted)

        Spec recipe OpenNeedsSig ->
            Just <|
                EH.closeableModal
                    (Element.column
                        [ Element.spacing 15
                        , Element.centerX
                        ]
                        [ Element.text "Text about opening"
                        , case maybeUserInfo of
                            Just userInfo ->
                                EH.blueButton
                                    "Open Trade"
                                    (OpenClicked
                                        userInfo
                                        recipe
                                    )

                            Nothing ->
                                EH.disabledButton "Can't find userInfo..." (Just "You shouldn't be seeing this. Maybe your web3 provider changed something just now?")
                        ]
                    )
                    (ChangeState <| Spec recipe ReadyToOpen)

        Spec recipe OpenMining ->
            Just <|
                EH.closeableModal
                    (Element.column
                        [ Element.spacing 15
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
