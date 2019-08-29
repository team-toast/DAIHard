module CryptoSwap.View exposing (root)

import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import CryptoSwap.Types exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Html.Attributes
import Images exposing (Image)
import Prices
import Routing exposing (Route)
import TokenValue exposing (TokenValue)
import Wallet


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( Element.column
        [ Element.spacing 30
        , Element.width Element.fill
        ]
        [ EH.submodelContainer
            1000
            (Just "Trade Dai/xDai for ZEC, XMR, or BTC")
            "CRYPTO SWAP"
            (Element.column
                [ Element.spacing 20
                , Element.padding 15
                , Element.width Element.fill
                ]
                [ fromToElement model
                , Element.row
                    [ Element.width Element.fill ]
                    [ Element.el [ Element.width (Element.fillPortion 1) ] Element.none
                    , Element.el [ Element.width (Element.fillPortion 2) ] <| maybeAddressInput model
                    , Element.el [ Element.width (Element.fillPortion 1) ] Element.none
                    ]
                , Element.el [ Element.centerX ] (placeOrderButton model)
                ]
            )
        , Element.link
            [ Element.Border.rounded 4
            , Element.width Element.fill
            , Element.pointer
            , Element.paddingXY 22 15
            , Element.Background.color EH.blue
            , Element.Font.color EH.white
            , Element.Font.semiBold
            , Element.Font.size 20
            , Element.centerX
            , Element.width Element.shrink
            , Element.height Element.shrink
            ]
            { url = "https://t.me/daihardexchange_group"
            , label =
                Element.paragraph
                    [ Element.Font.center ]
                    [ Element.text "Join the Telegram Group" ]
            }
        ]
    , [ getModalOrNone model ]
    )


inputHeader : String -> Element Msg
inputHeader title =
    Element.el
        [ Element.Font.size 20
        , Element.paddingXY 20 0
        , Element.Font.color EH.red
        ]
        (Element.text title)


fromToElement : Model -> Element Msg
fromToElement model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.row
            [ Element.spacing 10
            , Element.width Element.fill
            ]
            [ Element.column
                [ Element.spacing 5
                , Element.alignBottom
                , Element.width Element.fill
                ]
                [ inputHeader "From:"
                , fromInputBox model
                ]
            , Images.toElement
                [ Element.alignBottom
                , Element.width <| Element.px 24
                , Element.pointer
                , Element.Events.onClick SwapClicked
                , Element.paddingEach
                    { bottom = 18
                    , top = 0
                    , right = 0
                    , left = 0
                    }
                ]
                Images.swapArrows
            , Element.column
                [ Element.spacing 5
                , Element.alignBottom
                , Element.width Element.fill
                ]
                [ inputHeader "To:"
                , toInputBox model
                ]
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Element.el
                [ Element.width Element.fill ]
                (case model.errors.amountIn of
                    Just errStr ->
                        Element.el
                            [ Element.Font.color EH.red
                            , Element.Font.size 14
                            , Element.centerX
                            ]
                            (Element.text errStr)

                    Nothing ->
                        Element.none
                )
            , Element.el
                [ Element.width <| Element.px 24 ]
                Element.none
            , Element.el
                [ Element.width Element.fill ]
                (case model.errors.margin of
                    Just errStr ->
                        Element.el
                            [ Element.Font.color EH.red
                            , Element.Font.size 14
                            , Element.centerX
                            ]
                            (Element.text errStr)

                    Nothing ->
                        Element.none
                )
            ]
        ]


inputBoxStyles : List (Element.Attribute Msg)
inputBoxStyles =
    [ Element.Border.rounded 20
    , Element.paddingXY 20 0
    , Element.Border.width 1
    , Element.Background.color EH.white
    , Element.Border.color <| Element.rgba 0 0 0 0.1
    , Element.height <| Element.px 60
    ]


fromInputBox : Model -> Element Msg
fromInputBox model =
    let
        tokenSelector =
            case model.initiatorRole of
                Buyer ->
                    foreignCryptoTypeElement model.foreignCrypto model.showForeignCryptoDropdown

                Seller ->
                    dhTokenTypeElement model.dhToken model.showDhTokenDropdown
    in
    Element.row
        (inputBoxStyles
            ++ [ Element.spacing 15
               , Element.width Element.fill
               , Element.centerY
               ]
        )
        [ tokenSelector
        , amountInInputElement model.amountInInput
        ]


toInputBox : Model -> Element Msg
toInputBox model =
    let
        tokenSelector =
            case model.initiatorRole of
                Buyer ->
                    dhTokenTypeElement model.dhToken model.showDhTokenDropdown

                Seller ->
                    foreignCryptoTypeElement model.foreignCrypto model.showForeignCryptoDropdown
    in
    Element.row
        (inputBoxStyles
            ++ [ Element.spacing 15
               , Element.width Element.fill
               , Element.centerY
               ]
        )
        [ tokenSelector
        , marginInput model.marginInput
        , amountOutInputElement model
        ]


dhTokenTypeElement : FactoryType -> Bool -> Element Msg
dhTokenTypeElement currentToken showDropdown =
    Element.row
        [ Element.spacing 8
        , Element.pointer
        , EH.onClickNoPropagation TokenTypeClicked
        , Element.centerY
        , Element.inFront <|
            if showDropdown then
                Element.el
                    [ Element.moveUp 15
                    , Element.moveLeft 10
                    , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                    , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
                    ]
                <|
                    EH.dropdownSelector
                        ([ Token EthDai, Native XDai ]
                            |> List.map
                                (\token ->
                                    ( Element.text (tokenUnitName token)
                                    , ChangeTokenType token
                                    )
                                )
                        )

            else
                Element.none
        ]
        [ Element.text <| tokenUnitName currentToken
        , Images.toElement
            [ Element.width <| Element.px 12 ]
            Images.downArrow
        ]


foreignCryptoTypeElement : ForeignCrypto -> Bool -> Element Msg
foreignCryptoTypeElement currentCrypto showDropdown =
    Element.row
        [ Element.spacing 8
        , Element.pointer
        , EH.onClickNoPropagation ForeignCryptoTypeClicked
        , Element.centerY
        , Element.inFront <|
            if showDropdown then
                Element.el
                    [ Element.moveUp 15
                    , Element.moveLeft 10
                    , Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
                    , Element.htmlAttribute <| Html.Attributes.style "z-index" "1000"
                    ]
                <|
                    EH.dropdownSelector
                        (foreignCryptoList
                            |> List.map
                                (\crypto ->
                                    ( Element.text (foreignCryptoName crypto)
                                    , ChangeForeignCrypto crypto
                                    )
                                )
                        )

            else
                Element.none
        ]
        [ Element.text <| foreignCryptoName currentCrypto
        , Images.toElement
            [ Element.width <| Element.px 12 ]
            Images.downArrow
        ]


amountInInputElement : String -> Element Msg
amountInInputElement input =
    Element.Input.text
        [ Element.Border.width 0
        , Element.width Element.fill
        , Element.Font.alignRight
        ]
        { onChange = AmountInChanged
        , text = input
        , placeholder =
            Just <|
                Element.Input.placeholder
                    [ Element.Font.color EH.lightGray
                    , Element.Font.alignRight
                    ]
                    (Element.text "0")
        , label = Element.Input.labelHidden "amount in"
        }


marginInput : String -> Element Msg
marginInput input =
    Element.row
        [ Element.centerY
        , Element.spacing 0
        , Element.Font.size 18
        , Element.width Element.shrink
        ]
        [ Element.text "( -"
        , Element.Input.text
            [ Element.Border.width 0
            , Element.padding 0
            , Element.Font.size 18
            , Element.width <|
                (Element.px (10 * String.length input) |> Element.minimum 10)
            ]
            { onChange = MarginChanged
            , text = input
            , placeholder =
                Nothing
            , label = Element.Input.labelHidden "margin"
            }
        , Element.text "%)"
        ]


maybeAddressInput : Model -> Element Msg
maybeAddressInput model =
    case model.initiatorRole of
        Buyer ->
            Element.none

        Seller ->
            Element.column
                [ Element.spacing 5
                , Element.alignBottom
                , Element.width Element.fill
                , Element.clipX
                ]
                [ inputHeader <|
                    "Send "
                        ++ foreignCryptoName model.foreignCrypto
                        ++ " To:"
                , addressInputElement model.receiveAddress model.foreignCrypto
                ]


amountOutInputElement : Model -> Element Msg
amountOutInputElement model =
    Element.Input.text
        [ Element.Border.width 0
        , Element.width Element.fill
        , Element.Font.alignRight
        ]
        { onChange = AmountOutChanged
        , text = model.amountOutInput
        , placeholder =
            Just <|
                Element.Input.placeholder
                    [ Element.Font.color EH.lightGray
                    , Element.Font.alignRight
                    ]
                    (Element.text "0")
        , label = Element.Input.labelHidden "amount out"
        }


addressInputElement : String -> ForeignCrypto -> Element Msg
addressInputElement input foreignCrypto =
    Element.el
        (inputBoxStyles
            ++ [ Element.width Element.fill
               , Element.paddingXY 10 0
               ]
        )
    <|
        Element.Input.text
            [ Element.centerY
            , Element.width Element.fill
            , Element.Border.width 0
            ]
            { onChange = ReceiveAddressChanged
            , text = input
            , placeholder =
                Just <|
                    Element.Input.placeholder
                        [ Element.Font.color EH.lightGray ]
                        (Element.text <| exampleAddressForForeignCrypto foreignCrypto)
            , label = Element.Input.labelHidden "receive address"
            }


placeOrderButton : Model -> Element Msg
placeOrderButton model =
    case ( Wallet.userInfo model.wallet, maybeUserParameters model ) of
        ( Just userInfo, Just userParameters ) ->
            if Wallet.factory model.wallet == Just model.dhToken then
                EH.redButton "Place Order" (PlaceOrderClicked model.dhToken userInfo userParameters)

            else
                EH.disabledButton "Place Order"
                    (Just <|
                        "You must switch your wallet to the "
                            ++ networkNameForFactory model.dhToken
                            ++ " network to create a trade with "
                            ++ tokenUnitName model.dhToken
                    )

        ( Nothing, _ ) ->
            EH.redButton "Connect to Wallet" (CmdUp CmdUp.Web3Connect)

        ( _, Nothing ) ->
            EH.disabledButton "Place Order" Nothing


getModalOrNone : Model -> Element Msg
getModalOrNone model =
    case model.txChainStatus of
        Nothing ->
            Element.none

        Just txChainStatus ->
            txChainStatusModal txChainStatus model


txChainStatusModal : TxChainStatus -> Model -> Element Msg
txChainStatusModal txChainStatus model =
    case txChainStatus of
        Confirm factoryType createParameters receiveAddress ->
            let
                ( depositAmountEl, totalBurnableEl, confirmButton ) =
                    case model.depositAmount of
                        Just depositAmount ->
                            let
                                depositAmountText =
                                    (TokenValue.toConciseString <| TokenValue.tokenValue depositAmount)
                                        ++ " "
                                        ++ tokenUnitName factoryType

                                totalBurnableText =
                                    TokenValue.toConciseString
                                        (TokenValue.add
                                            (TokenValue.tokenValue depositAmount)
                                            (CTypes.getResponderDeposit createParameters)
                                            |> TokenValue.add (CTypes.calculateDHFee createParameters)
                                        )
                                        ++ " "
                                        ++ tokenUnitName factoryType
                            in
                            ( blueText depositAmountText
                            , blueText totalBurnableText
                            , EH.redButton
                                ("Yes. Deposit "
                                    ++ depositAmountText
                                    ++ " and open this trade."
                                )
                                (ConfirmCreate factoryType createParameters depositAmount)
                            )

                        Nothing ->
                            ( blueText "??"
                            , blueText "??"
                            , EH.disabledButton "(loading exact fees...)" Nothing
                            )

                feeAmountEl =
                    blueText <|
                        TokenValue.toConciseString (CTypes.calculateDHFee createParameters)
                            ++ " "
                            ++ tokenUnitName factoryType

                tradeAmountEl =
                    blueText <|
                        TokenValue.toConciseString createParameters.tradeAmount
                            ++ " "
                            ++ tokenUnitName factoryType

                customizeButton =
                    EH.blueButton
                        "Customize Contract Terms"
                        (CmdUp <| CmdUp.GotoRoute <| Routing.Create (Just <| CTypes.createParametersToUserParameters createParameters))

                notYetButton =
                    EH.blueButton
                        "Not yet. Go back."
                        AbortCreate

                buyerDepositEl =
                    blueText <|
                        TokenValue.toConciseString createParameters.buyerDeposit
                            ++ " "
                            ++ tokenUnitName factoryType

                totalReleaseableEl =
                    blueText
                        (TokenValue.toConciseString (TokenValue.add createParameters.tradeAmount createParameters.buyerDeposit)
                            ++ " "
                            ++ tokenUnitName factoryType
                        )

                priceEl =
                    blueText <| Prices.toString createParameters.price

                emphasizedText =
                    Element.el
                        [ Element.Font.bold
                        , Element.Font.color EH.black
                        ]
                        << Element.text

                blueText =
                    Element.el
                        [ Element.Font.semiBold
                        , Element.Font.color EH.blue
                        ]
                        << Element.text
            in
            EH.closeableModal
                []
                (Element.column
                    [ Element.spacing 20
                    , Element.padding 20
                    , Element.centerX
                    , Element.height Element.fill
                    , Element.width Element.fill
                    , Element.Font.center
                    ]
                    [ Element.el
                        [ Element.Font.size 26
                        , Element.Font.semiBold
                        , Element.centerX
                        , Element.centerY
                        ]
                        (case createParameters.initiatorRole of
                            Buyer ->
                                Element.text "Opening a DAIHard Buy Offer"

                            Seller ->
                                Element.text "Opening a DAIHard Sell Offer"
                        )
                    , Element.column
                        [ Element.spacing 20
                        , Element.centerX
                        , Element.centerY
                        ]
                        (List.map
                            (Element.paragraph
                                [ Element.centerX
                                , Element.Font.size 18
                                , Element.Font.medium
                                , Element.Font.color EH.permanentTextColor
                                ]
                            )
                            (case createParameters.initiatorRole of
                                Buyer ->
                                    [ [ Element.text "To open this offer, you must deposit "
                                      , depositAmountEl
                                      , Element.text ". Your offer to buy "
                                      , tradeAmountEl
                                      , Element.text " for "
                                      , priceEl
                                      , Element.text " will then be listed on the marketplace."
                                      ]
                                    , [ Element.text "You can abort the offer any time before a Seller commits for a full refund. If no Seller commits within "
                                      , emphasizedText "24 hours"
                                      , Element.text " your offer will automatically expire."
                                      ]
                                    , [ Element.text "A Seller can commit to the trade by depositing the full "
                                      , tradeAmountEl
                                      , Element.text " into the contract, and is expected to immediately post his "
                                      , blueText <| createParameters.price.symbol
                                      , Element.text " address in the DAIHard chat."
                                      ]
                                    , [ Element.text "You will then have "
                                      , emphasizedText "24 hours"
                                      , Element.text " to send "
                                      , priceEl
                                      , Element.text " to that address and click \"Confirm Payment\"."
                                      ]
                                    , [ Element.text "Once you've confirmed payment, for "
                                      , emphasizedText "24 hours"
                                      , Element.text ", the Seller has the option of burning the trade's full balance of "
                                      , totalBurnableEl
                                      , Element.text ". He is expected to do this if and only if you failed to send the "
                                      , priceEl
                                      , Element.text " to the address he posted."
                                      ]
                                    , [ Element.text "If the Seller has not burned the "
                                      , Element.text <| tokenUnitName factoryType
                                      , Element.text " within 24 hours, "
                                      , totalReleaseableEl
                                      , Element.text " is yours to claim and we take a 1% fee ("
                                      , feeAmountEl
                                      , Element.text ")."
                                      ]
                                    , [ Element.text <| "Are you ready?" ]
                                    ]
                                        ++ (case factoryType of
                                                Token _ ->
                                                    [ [ Element.text <| "(Trade creation ususally requires two Metamask signatures. Your " ++ tokenUnitName factoryType ++ " will not be deposited until the final transaction has been mined.)" ] ]

                                                Native _ ->
                                                    []
                                           )

                                Seller ->
                                    [ [ Element.text "Your "
                                      , tradeAmountEl
                                      , Element.text " will be listed as selling for "
                                      , priceEl
                                      , Element.text "."
                                      ]
                                    , [ Element.text "You can abort the offer at any time before a Buyer commits for a full refund. If no Buyer commits within "
                                      , emphasizedText "24 hours"
                                      , Element.text " your offer will automatically expire."
                                      ]
                                    , [ Element.text "A Buyer must deposit "
                                      , buyerDepositEl
                                      , Element.text <| " into this contract to commit. He is then expected to send the "
                                      , priceEl
                                      , Element.text <| " to your receive address "
                                      , blueText receiveAddress
                                      , Element.text ", and mark the payment as complete, all within "
                                      , emphasizedText "1 hour."
                                      ]
                                    , [ emphasizedText "Make sure the above address is correct! DAIHard does not do refunds!" ]
                                    , [ Element.text <| "When the Buyer marks the payment complete, for "
                                      , emphasizedText "24 hours"
                                      , Element.text " you will have the option to burn the trade's balance of "
                                      , totalBurnableEl
                                      , Element.text <| ", which you are expected to do if and only if the Buyer has not sent the payment."
                                      ]
                                    , [ Element.text "If the trade has resolved successfully, we take a 1% fee of "
                                      , feeAmountEl
                                      , Element.text "."
                                      ]
                                    , [ Element.text <| "Are you ready?" ]
                                    ]
                                        ++ (case factoryType of
                                                Token _ ->
                                                    [ [ Element.text <| "(Trade creation ususally requires two Metamask signatures. Your " ++ tokenUnitName factoryType ++ " will not be deposited until the final transaction has been mined.)" ] ]

                                                Native _ ->
                                                    []
                                           )
                            )
                        )
                    , Element.column
                        [ Element.centerX
                        , Element.spacing 15
                        ]
                        ([ confirmButton
                         , notYetButton
                         , customizeButton
                         ]
                            |> List.map (Element.el [ Element.centerX ])
                        )
                    ]
                )
                NoOp
                AbortCreate

        ApproveNeedsSig tokenType ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Events.onClick <|
                    CmdUp <|
                        CmdUp.gTag "txChainModal clicked" "misclick" "ApproveNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the approve call."
                    , Element.text "(check Metamask!)"
                    , Element.text "Note that there will be a second transaction to sign after this."
                    ]
                    NoOp
                    NoOp

        ApproveMining tokenType createParameters txHash ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Events.onClick <|
                    CmdUp <|
                        CmdUp.gTag "txChainModal clicked" "misclick" "ApproveMining" 0
                ]
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
                    NoOp

        CreateNeedsSig _ ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Events.onClick <|
                    CmdUp <|
                        CmdUp.gTag "txChainModal clicked" "misclick" "CreateNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the create call."
                    , Element.text "(check Metamask!)"
                    ]
                    NoOp
                    NoOp

        CreateMining factoryType txHash ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Events.onClick <|
                    CmdUp <|
                        CmdUp.gTag "txChainModal clicked" "misclick" "CreateMining" 0
                ]
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
                    NoOp
