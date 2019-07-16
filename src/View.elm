module View exposing (root)

import AgentHistory.View
import Browser
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Create.View
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FiatValue
import Helpers.Element as EH
import Helpers.Tuple exposing (mapTuple2)
import Landing.View
import Marketplace.Types
import Marketplace.View
import QuickCreate.View
import Routing
import Trade.View
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)


root : Model -> Browser.Document Msg
root model =
    { title = "DAIHard"
    , body =
        [ let
            ( pageEl, modalEl ) =
                pageElementAndModal model.screenWidth model

            mainElementAttributes =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Font.family
                    [ Element.Font.typeface "Soleil"
                    , Element.Font.sansSerif
                    ]
                , Element.inFront modalEl
                ]
          in
          Element.layout
            mainElementAttributes
            pageEl
        ]
    }


pageElementAndModal : Int -> Model -> ( Element Msg, Element Msg )
pageElementAndModal screenWidth model =
    let
        ( submodelEl, modalEl ) =
            submodelElementAndModal screenWidth model
    in
    ( Element.column
        [ Element.behindContent <| headerBackground
        , Element.inFront <| headerContent model
        , Element.inFront <| userNotices model.userNotices
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        ]
        [ Element.el
            [ Element.height (Element.px 50) ]
            Element.none
        , submodelEl
        ]
    , modalEl
    )


headerBackground : Element Msg
headerBackground =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 150
        , Element.Background.color EH.headerBackgroundColor
        ]
        Element.none


headerContent : Model -> Element Msg
headerContent model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.paddingXY 30 17
        ]
        [ headerLink
            ("Buy " ++ Config.tokenUnitName model.web3Context.factoryType)
            (GotoRoute <| Routing.Marketplace Buyer)
            (case model.submodel of
                MarketplaceModel marketplaceModel ->
                    if marketplaceModel.browsingRole == Buyer then
                        Active

                    else
                        Normal

                _ ->
                    Normal
            )
        , headerLink
            ("Sell " ++ Config.tokenUnitName model.web3Context.factoryType)
            (GotoRoute <| Routing.Marketplace Seller)
            (case model.submodel of
                MarketplaceModel marketplaceModel ->
                    if marketplaceModel.browsingRole == Seller then
                        Active

                    else
                        Normal

                _ ->
                    Normal
            )
        , headerLink
            "Create a New Offer"
            (GotoRoute Routing.Create)
            (case model.submodel of
                CreateModel _ ->
                    Active

                _ ->
                    Normal
            )
        , case model.userInfo of
            Just userInfo ->
                headerLink
                    "My Trades"
                    (GotoRoute <| Routing.AgentHistory userInfo.address Seller)
                    (case model.submodel of
                        AgentHistoryModel agentHistoryModel ->
                            if agentHistoryModel.agentAddress == userInfo.address then
                                Active

                            else
                                Normal

                        _ ->
                            Normal
                    )

            Nothing ->
                headerLink
                    "Connect to Wallet"
                    ConnectToWeb3
                    Important
        , Element.column
            [ Element.alignRight
            , Element.spacing 0
            , Element.paddingXY 8 0
            ]
            [ logoElement
            , networkModeElement model
            ]
        ]


networkModeElement : Model -> Element Msg
networkModeElement model =
    Element.el
        [ Element.Font.size 18
        , Element.Font.color <| Element.rgb 0.8 0.8 1
        , Element.Font.semiBold
        , Element.Font.italic
        , Element.centerX
        ]
        (Element.text (networkModeText model))


networkModeText : Model -> String
networkModeText model =
    case model.web3Context.factoryType of
        Native Eth ->
            "Mainnet ETH"

        Native Kovan ->
            "Testnet ETH"

        Native Rootstock ->
            "Rootstock SBTC"

        Native RootstockTest ->
            "RskTest SBTC"

        Native XDai ->
            "xDai"

        Token EthDai ->
            "Mainnet Dai"

        Token KovanDai ->
            "Testnet Dai"


type HeaderLinkStyle
    = Normal
    | Active
    | Important


headerLink : String -> Msg -> HeaderLinkStyle -> Element Msg
headerLink title onClick style =
    let
        extraStyles =
            case style of
                Normal ->
                    []

                Active ->
                    [ Element.Border.rounded 4
                    , Element.Background.color <| Element.rgb 0 0 1
                    ]

                Important ->
                    [ Element.Border.rounded 4
                    , Element.Background.color <| Element.rgb 0.9 0 0
                    ]
    in
    Element.el
        ([ Element.paddingXY 23 12
         , Element.Font.size 22
         , Element.Font.semiBold
         , Element.Font.color EH.white
         , Element.pointer
         , Element.Events.onClick onClick
         ]
            ++ extraStyles
        )
        (Element.text title)


logoElement : Element Msg
logoElement =
    Element.el
        [ Element.Font.size 29
        , Element.Font.color EH.white
        , Element.Font.bold
        , Element.centerX
        , Element.pointer
        , Element.Events.onClick <| GotoRoute Routing.Home
        ]
        (Element.paragraph []
            [ Element.text "DAI"
            , Element.el [ Element.Font.color EH.red ] <| Element.text "Hard"
            ]
        )


userNotices : List (UserNotice Msg) -> Element Msg
userNotices notices =
    Element.column
        [ Element.padding 20
        , Element.spacing 10
        , Element.alignRight
        , Element.alignBottom
        , Element.width <| Element.px 300
        , Element.Font.size 15
        ]
        (notices |> List.indexedMap userNotice)


userNotice : Int -> UserNotice Msg -> Element Msg
userNotice id notice =
    let
        color =
            case notice.noticeType of
                UN.Update ->
                    Element.rgb255 100 200 255

                UN.Caution ->
                    Element.rgb255 255 188 0

                UN.Error ->
                    Element.rgb255 255 70 70

                UN.ShouldBeImpossible ->
                    Element.rgb255 200 200 200

        textColor =
            case notice.noticeType of
                UN.Error ->
                    Element.rgb 1 1 1

                _ ->
                    Element.rgb 0 0 0

        closeElement =
            Element.el
                [ Element.alignRight
                , Element.alignTop
                , Element.moveUp 5
                , Element.moveRight 5
                ]
                (EH.closeButton (DismissNotice id))
    in
    Element.el
        [ Element.Background.color color
        , Element.Border.rounded 10
        , Element.padding 8
        , Element.width Element.fill
        , Element.Border.width 1
        , Element.Border.color <| Element.rgba 0 0 0 0.15
        , EH.subtleShadow
        ]
        (notice.mainParagraphs
            |> List.indexedMap
                (\pNum paragraphLines ->
                    Element.paragraph
                        [ Element.width Element.fill
                        , Element.Font.color textColor
                        ]
                        (if pNum == 0 then
                            closeElement :: paragraphLines

                         else
                            paragraphLines
                        )
                )
            |> Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
        )


dropdownStyles : List (Attribute Msg)
dropdownStyles =
    [ Element.padding 10
    , Element.spacing 10
    , Element.Border.rounded 4
    , Element.Background.color <| Element.rgb 0.5 0.5 1
    ]


myOffersElement =
    Element.el
        headerMenuAttributes
        (Element.text "My Trades")


headerMenuAttributes : List (Attribute Msg)
headerMenuAttributes =
    [ Element.Font.size 19
    , Element.Font.color EH.white
    , Element.Font.semiBold
    , Element.padding 20
    , Element.pointer
    ]


submodelElementAndModal : Int -> Model -> ( Element Msg, Element Msg )
submodelElementAndModal screenWidth model =
    let
        ( submodelEl, modalEl ) =
            case model.submodel of
                BetaLandingPage ->
                    ( Landing.View.root (GotoRoute <| Routing.Marketplace Buyer)
                    , Element.none
                    )

                CreateModel createModel ->
                    ( Element.map CreateMsg (Create.View.root createModel)
                    , Element.none
                    )

                QuickCreateModel quickCreateModel ->
                    QuickCreate.View.root quickCreateModel
                        |> mapTuple2 (Element.map QuickCreateMsg)

                TradeModel tradeModel ->
                    Trade.View.root model.time model.tradeCache tradeModel
                        |> mapTuple2 (Element.map TradeMsg)

                MarketplaceModel marketplaceModel ->
                    ( Element.map MarketplaceMsg (Marketplace.View.root model.time model.tradeCache marketplaceModel)
                    , Element.none
                    )

                AgentHistoryModel agentHistoryModel ->
                    ( Element.map AgentHistoryMsg (AgentHistory.View.root model.time model.tradeCache agentHistoryModel)
                    , Element.none
                    )
    in
    ( Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Border.rounded 10
        ]
        submodelEl
    , modalEl
    )
