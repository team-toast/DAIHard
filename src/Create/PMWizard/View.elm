module Create.PMWizard.View exposing (root)

import Create.PMWizard.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import ElementHelpers as EH


root : Model -> Element Msg
root model =
    Element.column
        [ Element.width <| Element.px 800
        , Element.centerX
        ]
        [ Element.el [ Element.height <| Element.px 60 ] Element.none
        , Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundTopCorners 8
            , Element.Border.color EH.darkGray
            , Element.Border.widthEach
                { bottom = 1
                , top = 0
                , right = 0
                , left = 0
                }
            , Element.paddingXY 48 39
            ]
            (headerElement model)
        , Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundBottomCorners 8
            , Element.paddingXY 48 39
            ]
            (bodyElement model)
        ]


headerElement : Model -> Element Msg
headerElement model =
    Element.row [ Element.width Element.fill ]
        [ Element.el
            [ Element.Font.size 24
            , Element.Font.semiBold
            , Element.alignLeft
            ]
            (Element.text <| getTitle model)
        , Element.el
            [ Element.Font.size 40
            , Element.alignRight
            , Element.pointer
            , Element.padding 4
            , Element.Events.onClick CloseClicked
            ]
            (Element.text "x")
        ]


bodyElement : Model -> Element Msg
bodyElement model =
    Element.none
