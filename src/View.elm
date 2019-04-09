module View exposing (root)

import Browser
import Contracts.Types as CTypes
import Create.View
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue
import Marketplace.Types
import Marketplace.View
import MyTrades.View
import Routing
import Trade.View
import Types exposing (..)


root : Model -> Browser.Document Msg
root maybeValidModel =
    { title = "DAIHard"
    , body =
        [ let
            mainElementAttributes =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                , Element.Font.family
                    [ Element.Font.typeface "Soleil"
                    , Element.Font.sansSerif
                    ]
                ]
          in
          Element.layout
            mainElementAttributes
            (pageElement maybeValidModel)
        ]
    }


pageElement : Model -> Element Msg
pageElement maybeValidModel =
    Element.column
        [ Element.behindContent <| headerBackground
        , Element.inFront <| headerContent maybeValidModel
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color EH.pageBackgroundColor
        , Element.padding 30
        , Element.scrollbarY
        ]
        [ Element.el
            [ Element.height (Element.px 50) ]
            Element.none
        , subModelElement maybeValidModel
        ]


headerBackground : Element Msg
headerBackground =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 150
        , Element.Background.color EH.headerBackgroundColor
        ]
        Element.none


headerContent : Model -> Element Msg
headerContent maybeValidModel =
    let
        ( maybeCurrentSubmodel, maybeUserInfo ) =
            case maybeValidModel of
                Running model ->
                    ( Just model.submodel, model.userInfo )

                Failed _ ->
                    ( Nothing, Nothing )
    in
    Element.el
        [ Element.width Element.fill
        ]
        (Element.row
            [ Element.width Element.fill
            , Element.spacing 30
            , Element.paddingXY 0 9
            ]
            [ logoElement
            , headerLink
                "Marketplace"
                (GotoRoute <| Routing.Marketplace CTypes.SellerOpened)
                (Maybe.map
                    (\submodel ->
                        case submodel of
                            MarketplaceModel _ ->
                                True

                            _ ->
                                False
                    )
                    maybeCurrentSubmodel
                    |> Maybe.withDefault False
                )
            , headerLink
                "Create a New Offer"
                (GotoRoute Routing.Create)
                (Maybe.map
                    (\submodel ->
                        case submodel of
                            CreateModel _ ->
                                True

                            _ ->
                                False
                    )
                    maybeCurrentSubmodel
                    |> Maybe.withDefault False
                )
            , case maybeUserInfo of
                Just userInfo ->
                    headerLink
                        "My Trades"
                        (GotoRoute <| Routing.MyTrades)
                        (Maybe.map
                            (\submodel ->
                                case submodel of
                                    MyTradesModel _ ->
                                        True

                                    _ ->
                                        False
                            )
                            maybeCurrentSubmodel
                            |> Maybe.withDefault False
                        )

                Nothing ->
                    Element.none
            ]
        )


headerLink : String -> Msg -> Bool -> Element Msg
headerLink title onClick selected =
    let
        extraStyles =
            if selected then
                [ Element.Border.rounded 4
                , Element.Background.color <| Element.rgba255 0 177 255 0.63
                ]

            else
                []
    in
    Element.el
        ([ Element.paddingXY 23 12
         , Element.Font.size 20
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
        [ Element.Font.size 22
        , Element.Font.color EH.white
        , Element.Font.bold
        , Element.pointer
        , Element.padding 20
        , Element.Events.onClick <| GotoRoute Routing.Home
        ]
        (Element.text "DAI | HARD")


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


subModelElement : Model -> Element Msg
subModelElement maybeValidModel =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Border.rounded 10
        ]
        (case maybeValidModel of
            Running model ->
                case model.submodel of
                    BetaLandingPage ->
                        betaLandingPage

                    CreateModel createModel ->
                        Element.map CreateMsg (Create.View.root createModel)

                    TradeModel tradeModel ->
                        Element.map TradeMsg (Trade.View.root model.time tradeModel)

                    MarketplaceModel marketplaceModel ->
                        Element.map MarketplaceMsg (Marketplace.View.root model.time marketplaceModel)

                    MyTradesModel myTradesModel ->
                        Element.map MyTradesMsg (MyTrades.View.root model.time myTradesModel)

            Failed errorMessageString ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.paragraph
                        []
                        [ Element.text errorMessageString ]
                    )
        )


betaLandingPage : Element Msg
betaLandingPage =
    Element.column
        [ Element.centerX ]
        [ Element.el
            [ Element.height (Element.px 100) ]
            Element.none
        , Element.column
            [ Element.centerX
            , Element.width <| Element.px 800
            , Element.Background.color <| Element.rgba 0.9 0.9 0.5 0.5
            , Element.Border.width 1
            , Element.Border.color <| Element.rgba 0.5 0.5 0.3 0.5
            , Element.Border.shadow
                { offset = ( 0, 3 )
                , size = 0
                , blur = 20
                , color = Element.rgba255 0 0 0 0.1
                }
            , Element.padding 20
            , Element.Border.rounded 10
            , Element.spacing 20
            , Element.paddingXY 50 30
            ]
            [ Element.column [ Element.width Element.fill ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.center
                        , Element.Font.size 24
                        , Element.Font.semiBold
                        , Element.spacing 17
                        ]
                    )
                    [ [ Element.el [ Element.Font.size 40 ] <| Element.text "Welcome to DAIHard!" ]

                    --, [ Element.text "Just a few notes for this beta version:" ]
                    ]
                )
            , Element.el [ Element.height <| Element.px 1 ] Element.none
            , EH.coolCurrencyHbreak
            , Element.el [ Element.height <| Element.px 1 ] Element.none
            , [ "Make sure Metamask is set up and unlocked, and keep an eye on its icon for pending notifications or mining transactions."
              , "DAIHard sometimes appears stuck, but generally it is Metamask being too meek with its notifications. When in doubt, check Metamask."
              ]
                |> (\lines ->
                        Element.column
                            [ Element.width Element.fill
                            , Element.spacing 20
                            ]
                            (lines
                                |> List.map
                                    (\line ->
                                        Element.row
                                            [ Element.spacing 20 ]
                                            [ Element.el
                                                [ Element.Font.size 30
                                                , Element.centerY
                                                ]
                                                (Element.text EH.bulletPointString)
                                            , Element.paragraph [] [ Element.text line ]
                                            ]
                                    )
                            )
                   )
            , Element.el [ Element.centerX ]
                (EH.blueButton "Okay, let's go!" (GotoRoute <| Routing.Marketplace CTypes.SellerOpened))
            , Element.el [ Element.height <| Element.px 1 ] Element.none
            , EH.coolCurrencyHbreak
            ]
        ]
