module View exposing (root)

import Browse.View
import Browser
import Create.View
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Interact.View
import Routing
import Types exposing (..)


root : Model -> Browser.Document Msg
root maybeValidModel =
    { title = "Toastytrade"
    , body =
        [ case maybeValidModel of
            Running model ->
                let
                    mainElementAttributes =
                        [ Element.width Element.fill
                        , Element.Background.color EH.pageBackgroundColor
                        ]

                    mainColumnAttributes =
                        [ Element.width Element.fill
                        ]
                in
                Element.layout mainElementAttributes
                    (Element.column mainColumnAttributes
                        [ headerElement
                        , bodyElement model
                        ]
                    )

            Failed str ->
                Element.layout []
                    (Element.text ("ERROR: " ++ str))
        ]
    }


headerElement : Element.Element Msg
headerElement =
    Element.row
        [ Element.Background.color EH.headerBackgroundColor
        , Element.width Element.fill
        , Element.padding 15
        , Element.spacing 30
        , Element.Font.size 24
        ]
        [ Element.Input.button [ Element.centerX ]
            { onPress = Just (GotoRoute Routing.Create)
            , label = Element.text "Create"
            }
        , Element.Input.button [ Element.centerX ]
            { onPress = Just (GotoRoute Routing.Browse)
            , label = Element.text "Browse"
            }
        ]


bodyElement : ValidModel -> Element.Element Msg
bodyElement model =
    Element.el [ Element.paddingXY 100 25, Element.width Element.fill ]
        (subModelElement model)


subModelElement : ValidModel -> Element.Element Msg
subModelElement model =
    let
        subModelStyles =
            [ Element.width Element.fill
            , Element.spacing 20
            ]

        bodyStyles =
            [ Element.Border.rounded 15
            , Element.Background.color EH.subpageBackgroundColor
            , Element.padding 20
            , Element.spacing 50
            , Element.width Element.fill
            ]
    in
    (\( title, element ) ->
        Element.column subModelStyles
            [ EH.pageTitle title
            , Element.el bodyStyles element
            ]
    )
        (case model.submodel of
            HomeModel ->
                ( "Home", Element.none )

            CreateModel createModel ->
                ( "Create", Element.map CreateMsg (Create.View.root createModel) )

            InteractModel interactModel ->
                ( "Interact", Element.map InteractMsg (Interact.View.root model.time interactModel) )

            BrowseModel browseModel ->
                ( "Browse", Element.map BrowseMsg (Browse.View.root model.time browseModel) )

            None ->
                ( "none", Element.none )
        )
