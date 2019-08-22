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
        [ Element.paddingEach
            { top = 40
            , bottom = 0
            , right = 0
            , left = 0
            }
        , Element.spacing 60
        , Element.width Element.fill
        ]
        [ Element.column
            [ Element.spacing 20
            , Element.Font.color EH.white
            , Element.width Element.fill
            ]
            [ Element.el
                [ Element.Font.size 34
                , Element.Font.semiBold
                , Element.centerX
                ]
                (Element.text "Dai is boring. Get that sexy sexy zCash instead!")
            , Element.el
                [ Element.Font.size 18
                , Element.centerX
                ]
                (Element.text "Use this 100% visually unique interface to get started.")
            ]
        , Element.column
            [ Element.Background.color <| Element.rgb 0.95 0.98 1
            , Element.spacing 20
            , Element.Border.rounded 8
            , Element.clip
            , Element.centerX
            , Element.width (Element.fill |> Element.maximum 1000)
            , Element.Border.shadow
                { offset = ( 0, 0 )
                , size = 1
                , blur = 3
                , color = Element.rgba 0 0 0 0.2
                }
            , Element.inFront <|
                getModalOrNone model
            ]
            [ titleElement model
            , Element.column
                [ Element.spacing 20
                , Element.padding 15
                , Element.width Element.fill
                ]
                [ fromToElement model
                , tradeOutputAndMaybeAddressInput model
                , Element.el [ Element.centerX ] (placeOrderButton model)
                ]
            ]
        ]
    , []
    )


titleElement : Model -> Element Msg
titleElement model =
    Element.el
        [ Element.width Element.fill
        , Element.padding 15
        , Element.Background.color EH.white
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 30
            , color = Element.rgba 0 0 0 0.15
            }
        ]
    <|
        Element.row
            [ Element.spacing 40
            , Element.centerX
            ]
            [ headerTab
                True
                "QUICKSWAP"
                NoOp
            , headerTab
                False
                "CUSTOM"
                (AppCmd <| AppCmd.GotoRoute Routing.Create)
            ]


headerTab : Bool -> String -> Msg -> Element Msg
headerTab selected title onClickMsg =
    Element.el
        ([ Element.Font.size 16
         , Element.Events.onClick onClickMsg
         , Element.pointer
         , Element.centerY
         ]
            ++ (if selected then
                    [ Element.Font.bold
                    , Element.Font.color EH.red
                    ]

                else
                    [ Element.Font.regular
                    , Element.Font.color <| Element.rgb 0.2 0.2 0.2
                    ]
               )
        )
        (Element.text title)


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
    Element.row
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
                    foreignCryptoTypeElement model.foreignCrypto

                Seller ->
                    dhTokenTypeElement model.dhToken
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
                    dhTokenTypeElement model.dhToken

                Seller ->
                    foreignCryptoTypeElement model.foreignCrypto
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
        ]


dhTokenTypeElement : FactoryType -> Element Msg
dhTokenTypeElement currentToken =
    Element.row
        [ Element.spacing 8
        , Element.pointer
        , Element.Events.onClick TokenTypeClicked
        , Element.centerY
        ]
        [ Element.text <| tokenUnitName currentToken
        , Images.toElement
            [ Element.width <| Element.px 12 ]
            Images.downArrow
        ]


foreignCryptoTypeElement : ForeignCrypto -> Element Msg
foreignCryptoTypeElement currentCrypto =
    Element.row
        [ Element.spacing 8
        , Element.pointer
        , Element.Events.onClick ForeignCryptoTypeClicked
        , Element.centerY
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


tradeOutputAndMaybeAddressInput : Model -> Element Msg
tradeOutputAndMaybeAddressInput model =
    case model.initiatorRole of
        Buyer ->
            Element.el
                [ Element.centerX ]
                (tradeOutputElement model)

        Seller ->
            Element.row
                [ Element.spacing 10
                , Element.width Element.fill
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.alignBottom
                    ]
                  <|
                    tradeOutputElement model
                , Element.el
                    [ Element.width <| Element.px 24 ]
                    Element.none
                , Element.column
                    [ Element.spacing 5
                    , Element.alignBottom
                    , Element.width Element.fill
                    ]
                    [ inputHeader <|
                        "Send "
                            ++ foreignCryptoName model.foreignCrypto
                            ++ " To:"
                    , addressInputElement model.receiveAddressInput model.foreignCrypto
                    ]
                ]


tradeOutputElement : Model -> Element Msg
tradeOutputElement model =
    case model.amountOut of
        Just amount ->
            Element.row
                [ Element.spacing 10
                , Element.alignBottom
                , Element.alignRight
                , Element.padding 20
                , Element.Font.size 24
                , Element.Font.color <| Element.rgb 0.2 0.2 0.2
                ]
                [ Element.text "="
                , Element.text <| TokenValue.toFloatString (Just 6) amount
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
                               , Element.text <| " " ++ tokenUnitName factoryType ++ " to open this trade."
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
