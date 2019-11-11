module View exposing (root)

import AgentHistory.View
import Browser
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Create.View
import Currencies
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH
import Helpers.Tuple exposing (mapTuple2)
import Images exposing (Image)
import Marketplace.Types
import Marketplace.View
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
                pageElementAndModal model.dProfile model

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


pageElementAndModal : DisplayProfile -> Model -> ( Element Msg, List (Element Msg) )
pageElementAndModal dProfile model =
    let
        ( submodelEl, modalEls ) =
            submodelElementAndModal dProfile model
    in
    ( Element.column
        [ Element.behindContent <| headerBackground dProfile
        , Element.inFront <| headerContent dProfile model
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.padding (30 |> changeForMobile 10 dProfile)
        ]
        [ Element.el
            [ Element.height (Element.px (80 |> changeForMobile 120 dProfile)) ]
            Element.none
        , submodelEl
        ]
    , modalEls ++ userNoticeEls model.userNotices
    )


headerBackground : DisplayProfile -> Element Msg
headerBackground dProfile =
    let
        bottomBackgroundColor =
            Element.rgb255 10 33 108

        headerColor =
            Element.rgb255 7 27 92
    in
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 600
        , Element.Background.color bottomBackgroundColor
        , Element.inFront <|
            Element.el
                [ Element.width Element.fill
                , Element.height <| Element.px (80 |> changeForMobile 120 dProfile)
                , Element.Background.color headerColor
                ]
                Element.none
        ]
        Element.none


headerContent : DisplayProfile -> Model -> Element Msg
headerContent dProfile model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing (30 |> changeForMobile 10 dProfile)
        , Element.paddingXY 30 17 |> changeForMobile (Element.padding 10) dProfile
        ]
        [ (Element.row |> changeForMobile Element.column dProfile)
            [ Element.spacing (30 |> changeForMobile 10 dProfile)
            , Element.alignTop
            ]
            [ headerLink
                dProfile
                (Just Images.marketplace)
                "Marketplace"
                (GotoRoute Routing.Marketplace)
                (case model.submodel of
                    MarketplaceModel marketplaceModel ->
                        Active

                    _ ->
                        Normal
                )
            , headerLink
                dProfile
                (Just Images.newTrade)
                "Create New Trade"
                (GotoRoute <| Routing.CreateFiat)
                (case model.submodel of
                    CreateModel _ ->
                        Active

                    _ ->
                        Normal
                )
            , case Wallet.userInfo model.wallet of
                Just userInfo ->
                    headerLink
                        dProfile
                        (Just Images.myTrades)
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
                        dProfile
                        Nothing
                        "Connect to Wallet"
                        ConnectToWeb3
                        Important
            ]
        , let
            smLinks =
                [ Element.el
                    [ Element.centerY
                    , Element.alignRight
                    ]
                  <|
                    headerExternalLink dProfile "Blog" "https://medium.com/daihard-buidlers"
                , Element.el
                    [ Element.centerY
                    , Element.alignRight
                    ]
                  <|
                    headerExternalLink dProfile "Reddit" "https://reddit.com/r/DAIHard"
                , Element.el
                    [ Element.centerY
                    , Element.alignRight
                    ]
                  <|
                    headerExternalLink dProfile "Telegram" "https://t.me/daihardexchange_group"
                ]
          in
          case dProfile of
            Desktop ->
                Element.column
                    [ Element.spacing 5
                    , Element.alignRight
                    , Element.alignTop
                    ]
                    [ Element.el [ Element.alignRight ] <| logoElement dProfile
                    , Element.row
                        [ Element.spacing 10
                        ]
                        smLinks
                    ]

            Mobile ->
                Element.column
                    [ Element.spacing 10
                    , Element.alignTop
                    , Element.alignRight
                    ]
                    ([ logoElement dProfile ] ++ smLinks)
        ]


type HeaderLinkStyle
    = Normal
    | Active
    | Important


headerLinkBaseStyles : DisplayProfile -> List (Attribute Msg)
headerLinkBaseStyles dProfile =
    (case dProfile of
        Desktop ->
            [ Element.paddingXY 23 12
            , Element.Font.size 21
            , Element.Font.semiBold
            , Element.spacing 13
            ]

        Mobile ->
            [ Element.paddingXY 10 5
            , Element.Font.size 16
            , Element.spacing 6
            ]
    )
        ++ [ Element.Font.color EH.white
           , Element.pointer
           , EH.noSelectText
           ]


headerExternalLink : DisplayProfile -> String -> String -> Element Msg
headerExternalLink dProfile title url =
    Element.link
        [ Element.Font.size 16
        , Element.Font.semiBold
        , Element.Font.color EH.white
        , Element.pointer
        , EH.noSelectText
        ]
        { url = url
        , label =
            Element.el
                [ Element.centerY
                , Element.height <| Element.px (26 |> changeForMobile 14 dProfile)
                ]
            <|
                Element.text title
        }


headerLink : DisplayProfile -> Maybe Image -> String -> Msg -> HeaderLinkStyle -> Element Msg
headerLink dProfile maybeIcon title onClick style =
    let
        height =
            26 |> changeForMobile 18 dProfile

        extraStyles =
            case style of
                Normal ->
                    []

                Active ->
                    [ Element.Border.rounded 4
                    , Element.Background.color <| Element.rgb255 2 172 214
                    ]

                Important ->
                    [ Element.Border.rounded 4
                    , Element.Background.color EH.softRed
                    ]
    in
    Element.row
        (headerLinkBaseStyles dProfile
            ++ [ Element.Events.onClick onClick ]
            ++ extraStyles
        )
        [ Maybe.map
            (Images.toElement
                [ Element.height <| Element.px height ]
            )
            maybeIcon
            |> Maybe.withDefault Element.none
        , Element.el
            [ Element.centerY
            , Element.height <| Element.px height
            ]
          <|
            Element.text title
        ]


logoElement : DisplayProfile -> Element Msg
logoElement dProfile =
    Element.el
        [ Element.Font.size (29 |> changeForMobile 20 dProfile)
        , Element.Font.color EH.white
        , Element.Font.bold
        , Element.centerX
        , Element.pointer
        , Element.Events.onClick <| GotoRoute Routing.CreateFiat
        , EH.noSelectText
        ]
        (Element.paragraph []
            [ Element.text "DAI"
            , Element.el [ Element.Font.color EH.softRed ] <| Element.text "Hard"
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


submodelElementAndModal : DisplayProfile -> Model -> ( Element Msg, List (Element Msg) )
submodelElementAndModal dProfile model =
    let
        ( submodelEl, modalEls ) =
            case model.submodel of
                InitialBlank ->
                    ( Element.none
                    , []
                    )

                CreateModel createModel ->
                    Create.View.root dProfile createModel
                        |> Tuple.mapBoth
                            (Element.map CreateMsg)
                            (List.map (Element.map CreateMsg))

                FetchingRedeployForCreate _ ->
                    ( EH.simpleSubmodelContainer
                        800
                        (Element.el
                            [ Element.centerX
                            , Element.padding 30
                            , Element.Font.size 30
                            ]
                            (Element.text "Loading trade info...")
                        )
                    , []
                    )

                TradeModel tradeModel ->
                    Trade.View.root dProfile model.time model.tradeCaches tradeModel
                        |> Tuple.mapBoth
                            (Element.map TradeMsg)
                            (List.map (Element.map TradeMsg))

                MarketplaceModel marketplaceModel ->
                    Marketplace.View.root model.time dProfile model.tradeCaches marketplaceModel
                        |> Tuple.mapBoth
                            (Element.map MarketplaceMsg)
                            (List.map (Element.map MarketplaceMsg))

                AgentHistoryModel agentHistoryModel ->
                    ( Element.map AgentHistoryMsg (AgentHistory.View.root model.time dProfile model.tradeCaches agentHistoryModel)
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
