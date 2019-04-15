module Landing.View exposing (root)

import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import ElementHelpers as EH
import Images exposing (Image)


root : msg -> Element msg
root letsGoMsg =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color EH.white
        , Element.Border.rounded 6
        , Element.Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 20
            , color = Element.rgba255 0 0 0 0.1
            }
        , Element.Border.rounded 10
        , Element.spaceEvenly
        , Element.padding 20
        ]
        [ EH.coolCurrencyHbreak False Element.fill
        , Element.column
            [ Element.centerX
            , Element.spacing 25
            , Element.Font.center
            ]
            [ Element.paragraph
                [ Element.Font.size 35
                , Element.Font.semiBold
                ]
                [ Element.text "Welcome to DAIHard!" ]
            , Element.column
                [ Element.centerX
                , Element.spacing 5
                ]
                (List.map
                    (Element.paragraph
                        [ Element.Font.size 17
                        , Element.Font.medium
                        ]
                    )
                    [ [ Element.text "Just a couple things to note below before you proceed." ]
                    , [ Element.text "This should make things go a bit smoother." ]
                    ]
                )
            ]
        , Element.row
            [ Element.spaceEvenly
            , Element.width Element.fill
            ]
            [ Element.el [ Element.width <| Element.px 1 ] Element.none
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            , numberedNoteElement
                1
                "Confused?"
                [ Element.text "See our "
                , Element.newTabLink
                    [ Element.Font.color EH.blue
                    , Element.Font.underline
                    ]
                    { url = "NEEDLINK"
                    , label = Element.text "announcement post"
                    }
                , Element.text " for an overview of DAIHard, or the video "
                , Element.newTabLink
                    [ Element.Font.color EH.blue
                    , Element.Font.underline
                    ]
                    { url = "https://www.youtube.com/watch?v=TaI1fCzhSt4"
                    , label = Element.text "Using DAIHard to Buy and Sell DAI"
                    }
                , Element.text " for a quick usage demo."
                ]
            , numberedNoteElement
                2
                "Keep an Eye Out"
                [ Element.text "Make sure you keep an eye on MetaMask for pending notifications or mining transactions; it doesn’t always pop up when it needs your attention." ]
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            ]
        , Element.row
            [ Element.spaceEvenly
            , Element.width Element.fill
            ]
            [ Element.el [ Element.width <| Element.px 1 ] Element.none
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            , numberedNoteElement
                3
                "Under Rapid Construction"
                [ Element.text "We’re busy refining DaiHard and are regularly adding new functionality, so things might change quickly. Feel free to join our "
                , Element.newTabLink
                    [ Element.Font.color EH.blue
                    , Element.Font.underline
                    ]
                    { url = "https://t.me/daihard_exchange"
                    , label = Element.text "Telegram channel"
                    }
                , Element.text " for announcements."
                ]
            , numberedNoteElement
                4
                "Feedback, Please!"
                [ Element.text "We’d love any feedback or critique you can possibly give us! We'll respond fastest if you use the "
                , Element.newTabLink
                    [ Element.Font.color EH.blue
                    , Element.Font.underline
                    ]
                    { url = "https://t.me/daihardexchange_group"
                    , label = Element.text "Telegram group"
                    }
                , Element.text "."
                ]
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            , Element.el [ Element.width <| Element.px 1 ] Element.none
            ]
        , Element.el [ Element.centerX ]
            (EH.redButton "Okay, got it. let's go!" letsGoMsg)
        , EH.coolCurrencyHbreak True Element.fill
        ]


numberedNoteElement : Int -> String -> List (Element msg) -> Element msg
numberedNoteElement number title textElList =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 400)
        , Element.spacing 10
        , Element.alignTop
        ]
        [ Images.toElement [ Element.centerX ] <| Images.numberOnCircleEl number
        , Element.el
            [ Element.Font.size 20
            , Element.Font.semiBold
            , Element.centerX
            ]
            (Element.text title)
        , Element.paragraph
            [ Element.width Element.fill
            , Element.Font.size 17
            , Element.Font.medium
            , Element.Font.color EH.permanentTextColor
            , Element.Font.center
            ]
            textElList
        ]
