module View exposing (root)

import AgentHistory.View
import Browser
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Create.View
import CryptoSwap.View
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH
import Helpers.Tuple exposing (mapTuple2)
import Landing.View
import Marketplace.Types
import Marketplace.View
import Prices
import Routing
import Trade.View
import Types exposing (..)
import UserNotice as UN exposing (UserNotice)
import Wallet


root : Model -> Browser.Document Msg
root model =
    { title = "DAIHard"
    , body =
        [ let
            ( pageEl, modalEls ) =
                pageElementAndModal model.screenWidth model

            mainElementAttributes =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.onClick ClickHappened
                , Element.Font.family
                    [ Element.Font.typeface "Soleil"
                    , Element.Font.sansSerif
                    ]
                ]
                    ++ List.map Element.inFront modalEls
          in
          Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            mainElementAttributes
            pageEl
        ]
    }


pageElementAndModal : Int -> Model -> ( Element Msg, List (Element Msg) )
pageElementAndModal screenWidth model =
    let
        ( submodelEl, modalEls ) =
            submodelElementAndModal screenWidth model
    in
    ( Element.column
        [ Element.behindContent <| headerBackground
        , Element.inFront <| headerContent model
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        ]
        [ Element.el
            [ Element.height (Element.px 50) ]
            Element.none
        , submodelEl
        ]
    , modalEls ++ userNoticeEls model.userNotices
    )


headerBackground : Element Msg
headerBackground =
    let
        bottomBackgroundColor =
            Element.rgb255 255 144 0
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 400
        , Element.Background.color bottomBackgroundColor
        , Element.Border.shadow
            { offset = ( 0, 0 )
            , size = 30
            , blur = 30
            , color = bottomBackgroundColor
            }
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height <| Element.px 80
                , Element.Background.color <| Element.rgb255 10 33 108
                , Element.Border.shadow
                    { offset = ( 0, 0 )
                    , size = 8
                    , blur = 20
                    , color = Element.rgba 0 0 0 0.4
                    }
                ]
                Element.none
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
            "Crypto Swap"
            (GotoRoute <| Routing.CryptoSwap)
            (case model.submodel of
                CryptoSwapModel _ ->
                    Active

                _ ->
                    Normal
            )
        , headerLink
            "Custom Trade"
            (GotoRoute <| Routing.Create Nothing)
            (case model.submodel of
                CreateModel _ ->
                    Active

                _ ->
                    Normal
            )
        , headerLink
            "Marketplace"
            (GotoRoute Routing.Marketplace)
            (case model.submodel of
                MarketplaceModel marketplaceModel ->
                    Active

                _ ->
                    Normal
            )
        , case Wallet.userInfo model.wallet of
            Just userInfo ->
                headerLink
                    "My Trades"
                    (GotoRoute <| Routing.AgentHistory userInfo.address)
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
            ]
        ]


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


userNoticeEls : List (UserNotice Msg) -> List (Element Msg)
userNoticeEls notices =
    if notices == [] then
        []

    else
        [ Element.column
            [ Element.moveLeft 20
            , Element.moveUp 20
            , Element.spacing 10
            , Element.alignRight
            , Element.alignBottom
            , Element.width <| Element.px 300
            , Element.Font.size 15
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.BottomRight)
                |> List.map userNotice
            )
        , Element.column
            [ Element.moveRight 20
            , Element.moveDown 100
            , Element.spacing 10
            , Element.alignLeft
            , Element.alignTop
            , Element.width <| Element.px 300
            , Element.Font.size 15
            ]
            (notices
                |> List.indexedMap (\id notice -> ( id, notice ))
                |> List.filter (\( _, notice ) -> notice.align == UN.TopLeft)
                |> List.map userNotice
            )
        ]


userNotice : ( Int, UserNotice Msg ) -> Element Msg
userNotice ( id, notice ) =
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


submodelElementAndModal : Int -> Model -> ( Element Msg, List (Element Msg) )
submodelElementAndModal screenWidth model =
    let
        ( submodelEl, modalEls ) =
            case model.submodel of
                BetaLandingPage ->
                    ( Landing.View.root
                    , []
                    )

                CreateModel createModel ->
                    Create.View.root createModel
                        |> Tuple.mapBoth
                            (Element.map CreateMsg)
                            (List.map (Element.map CreateMsg))

                CryptoSwapModel cryptoSwapModel ->
                    CryptoSwap.View.root cryptoSwapModel
                        |> Tuple.mapBoth
                            (Element.map CryptoSwapMsg)
                            (List.map (Element.map CryptoSwapMsg))

                TradeModel tradeModel ->
                    Trade.View.root screenWidth model.time model.tradeCaches tradeModel
                        |> Tuple.mapBoth
                            (Element.map TradeMsg)
                            (List.map (Element.map TradeMsg))

                MarketplaceModel marketplaceModel ->
                    Marketplace.View.root model.time model.tradeCaches marketplaceModel
                        |> Tuple.mapBoth
                            (Element.map MarketplaceMsg)
                            (List.map (Element.map MarketplaceMsg))

                AgentHistoryModel agentHistoryModel ->
                    ( Element.map AgentHistoryMsg (AgentHistory.View.root model.time model.tradeCaches agentHistoryModel)
                    , []
                    )
    in
    ( Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Border.rounded 10
        ]
        submodelEl
    , modalEls
    )
