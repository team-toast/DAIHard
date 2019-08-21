module CryptoSwap.View exposing (root)

import AppCmd exposing (AppCmd)
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
import Images exposing (Image)
import Routing exposing (Route)
import TokenValue exposing (TokenValue)
import Wallet


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( Element.column
        [ Element.spacing 40
        , Element.padding 10
        , Element.Background.color EH.white
        , Element.Border.rounded 8
        , Element.centerX
        , Element.inFront <|
            getModalOrNone model
        ]
        [ titleElement model
        , Element.column
            [ Element.spacing 20 ]
            [ fromToElement model
            , tradeOutputAndMaybeAddressInput model
            , placeOrderButton model
            , additionalSettingsElement model.showAdditionalSettings
            ]
        ]
    , []
    )


titleElement : Model -> Element Msg
titleElement model =
    let
        swapTitle =
            Element.el
                [ Element.Font.size 24
                , Element.Font.medium
                , Element.Font.underline
                ]
                (Element.text "Swap")

        customLink =
            Element.el
                [ Element.Font.size 24
                , Element.Font.medium
                , Element.pointer
                , Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.GotoRoute Routing.Create
                ]
                (Element.text "Custom...")
    in
    Element.el
        [ Element.centerX
        , Element.onRight <|
            Element.el [ Element.moveRight 10 ] customLink
        ]
        swapTitle


fromToElement : Model -> Element Msg
fromToElement model =
    Element.row
        [ Element.spacing 10 ]
        [ Element.column
            [ Element.spacing 5
            , Element.alignBottom
            ]
            [ Element.el [ Element.Font.size 20 ] <| Element.text "From:"
            , fromInputBox model
            ]
        , Images.toElement
            [ Element.alignBottom
            , Element.width <| Element.px 30
            ]
            Images.swapArrows
        , Element.column
            [ Element.spacing 5
            , Element.alignBottom
            ]
            [ Element.el [ Element.Font.size 20 ] <| Element.text "To:"
            , toInputBox model
            ]
        ]


fromInputBox : Model -> Element Msg
fromInputBox model =
    let
        tokenSelector =
            case model.initiatorRole of
                Buyer ->
                    foreignCryptoTypeElement model.foreignCrypto

                Seller ->
                    dhTokenTypeElement model.dhToken
    in
    Element.row
        [ Element.Border.width 1
        , Element.Border.color EH.black
        , Element.Border.rounded 5
        , Element.padding 5
        , Element.spacing 3
        ]
        [ tokenSelector
        , amountInInputElement model.amountInInput
        ]


toInputBox : Model -> Element Msg
toInputBox model =
    let
        tokenSelector =
            case model.initiatorRole of
                Buyer ->
                    dhTokenTypeElement model.dhToken

                Seller ->
                    foreignCryptoTypeElement model.foreignCrypto
    in
    Element.row
        [ Element.Border.width 1
        , Element.Border.color EH.black
        , Element.Border.rounded 5
        , Element.padding 5
        , Element.spacing 3
        ]
        [ tokenSelector
        , Element.text "@"
        , marginInput model.marginInput
        ]


dhTokenTypeElement : FactoryType -> Element Msg
dhTokenTypeElement currentToken =
    Element.row
        [ Element.spacing 3
        , Element.pointer
        , Element.Events.onClick TokenTypeClicked
        ]
        [ Element.text <| tokenUnitName currentToken
        , Images.toElement
            [ Element.width <| Element.px 5 ]
            Images.downArrow
        ]


foreignCryptoTypeElement : ForeignCrypto -> Element Msg
foreignCryptoTypeElement currentCrypto =
    Element.row
        [ Element.spacing 3
        , Element.pointer
        , Element.Events.onClick ForeignCryptoTypeClicked
        ]
        [ Element.text <| foreignCryptoName currentCrypto
        , Images.toElement
            [ Element.width <| Element.px 5 ]
            Images.downArrow
        ]


amountInInputElement : String -> Element Msg
amountInInputElement input =
    Element.Input.text
        [ Element.Border.width 0
        , Element.width <| Element.px 100
        ]
        { onChange = AmountInChanged
        , text = input
        , placeholder =
            Just <|
                Element.Input.placeholder
                    [ Element.Font.color EH.darkGray ]
                    (Element.text "0")
        , label = Element.Input.labelHidden "amount in"
        }


marginInput : String -> Element Msg
marginInput input =
    Element.Input.text
        [ Element.Border.width 0
        , Element.width <| Element.px 100
        ]
        { onChange = MarginChanged
        , text = input
        , placeholder =
            if input == "" then
                Just <|
                    Element.Input.placeholder
                        [ Element.Font.color EH.darkGray ]
                        (Element.text "0")

            else
                Nothing
        , label = Element.Input.labelHidden "margin"
        }


tradeOutputAndMaybeAddressInput : Model -> Element Msg
tradeOutputAndMaybeAddressInput model =
    case model.initiatorRole of
        Buyer ->
            tradeOutputElement model

        Seller ->
            Element.row
                [ Element.spacing 20 ]
                [ tradeOutputElement model
                , Element.text "to"
                , addressInputElement model.receiveAddressInput model.foreignCrypto
                ]


tradeOutputElement : Model -> Element Msg
tradeOutputElement model =
    case model.amountOut of
        Just amount ->
            Element.row
                [ Element.spacing 10 ]
                [ Element.text "="
                , Element.text <| TokenValue.toConciseString amount
                , Element.text <|
                    case model.initiatorRole of
                        Buyer ->
                            tokenUnitName model.dhToken

                        Seller ->
                            foreignCryptoName model.foreignCrypto
                ]

        Nothing ->
            Element.none


addressInputElement : String -> ForeignCrypto -> Element Msg
addressInputElement input foreignCrypto =
    Element.Input.text []
        { onChange = ReceiveAddressChanged
        , text = input
        , placeholder =
            if input == "" then
                Just <|
                    Element.Input.placeholder
                        [ Element.Font.color EH.darkGray ]
                        (Element.text <| exampleAddressForForeignCrypto foreignCrypto)

            else
                Nothing
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
            EH.redButton "Connect to Wallet" (AppCmd AppCmd.Web3Connect)

        ( _, Nothing ) ->
            EH.disabledButton "Place Order" Nothing


additionalSettingsElement : Bool -> Element Msg
additionalSettingsElement showAdditionalSettings =
    if showAdditionalSettings then
        Debug.todo ""

    else
        Element.row
            [ Element.spacing 5
            , Element.pointer
            , Element.Events.onClick ToggleAdditionalSettings
            ]
            [ Element.text "Trade Lifecycle Settings"
            , Images.toElement
                [ Element.width <| Element.px 5 ]
                Images.downArrow
            ]


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
        Confirm factoryType createParameters ->
            let
                ( depositAmountEl, confirmButton ) =
                    case model.depositAmount of
                        Just depositAmount ->
                            ( TokenValue.tokenValue depositAmount
                                |> TokenValue.toConciseString
                                |> Element.text
                            , EH.redButton "Yes, I definitely want to open this trade." (ConfirmCreate factoryType createParameters depositAmount)
                            )

                        Nothing ->
                            ( Element.text "??"
                            , EH.disabledButton "(loading exact fees...)" Nothing
                            )
            in
            EH.closeableModal
                []
                (Element.column
                    [ Element.spacing 20
                    , Element.padding 20
                    , Element.centerX
                    , Element.height Element.fill
                    , Element.Font.center
                    ]
                    [ Element.el
                        [ Element.Font.size 26
                        , Element.Font.semiBold
                        , Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Just to Confirm...")
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
                            ([ [ Element.text <| "You will deposit "
                               , depositAmountEl
                               , Element.text <| " " ++ tokenUnitName factoryType ++ " (including the 1% dev fee) to open this trade."
                               ]
                             ]
                                ++ (case factoryType of
                                        Token _ ->
                                            [ [ Element.text <| "This ususally requires two Metamask signatures. Your " ++ tokenUnitName factoryType ++ " will not be deposited until the final transaction has been mined." ] ]

                                        Native _ ->
                                            []
                                   )
                            )
                        )
                    , Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        ]
                        confirmButton
                    ]
                )
                AbortCreate

        ApproveNeedsSig tokenType ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "ApproveNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the approve call."
                    , Element.text "(check Metamask!)"
                    , Element.text "Note that there will be a second transaction to sign after this."
                    ]

        ApproveMining tokenType createParameters txHash ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "ApproveMining" 0
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

        CreateNeedsSig _ ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "CreateNeedsSig" 0
                ]
            <|
                EH.txProcessModal
                    [ Element.text "Waiting for user signature for the create call."
                    , Element.text "(check Metamask!)"
                    ]

        CreateMining factoryType txHash ->
            Element.el
                [ Element.Events.onClick <|
                    AppCmd <|
                        AppCmd.gTag "txChainModal clicked" "misclick" "CreateMining" 0
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
