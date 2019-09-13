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
                , Element.height <| Element.px 80
                , Element.Background.color headerColor
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
            (Just Images.marketplace)
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
                    Nothing
                    "Connect to Wallet"
                    ConnectToWeb3
                    Important
        , headerLink
            (Just Images.newTrade)
            "Create New Trade"
            (GotoRoute <| Routing.CreateFiat)
            (case model.submodel of
                CreateModel _ ->
                    Active

                _ ->
                    Normal
            )
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


headerLink : Maybe Image -> String -> Msg -> HeaderLinkStyle -> Element Msg
headerLink maybeIcon title onClick style =
    let
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
        ([ Element.paddingXY 23 12
         , Element.Font.size 21
         , Element.Font.semiBold
         , Element.Font.color EH.white
         , Element.pointer
         , Element.Events.onClick onClick
         , Element.spacing 13
         ]
            ++ extraStyles
        )
        [ Maybe.map
            (Images.toElement
                [ Element.height <| Element.px 26 ]
            )
            maybeIcon
            |> Maybe.withDefault Element.none
        , Element.el
            [ Element.centerY
            , Element.height <| Element.px 26
            ]
          <|
            Element.text title
        ]


logoElement : Element Msg
logoElement =
    Element.el
        [ Element.Font.size 29
        , Element.Font.color EH.white
        , Element.Font.bold
        , Element.centerX
        , Element.pointer
        , Element.Events.onClick <| GotoRoute Routing.CreateFiat
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


submodelElementAndModal : Int -> Model -> ( Element Msg, List (Element Msg) )
submodelElementAndModal screenWidth model =
    let
        ( submodelEl, modalEls ) =
            case model.submodel of
                InitialBlank ->
                    ( Element.none
                    , []
                    )

                CreateModel createModel ->
                    Create.View.root createModel
                        |> Tuple.mapBoth
                            (Element.map CreateMsg)
                            (List.map (Element.map CreateMsg))

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
