module Landing.View exposing (root)

import AppCmd
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Helpers.Element as EH
import Images exposing (Image)
import Types exposing (..)


root : Element Msg
root =
    let
        redText =
            Element.el [ Element.Font.color <| Element.rgb 0.8 0.2 0.2 ] << Element.text

        boldText =
            Element.el [ Element.Font.bold ] << Element.text
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color EH.white
        , Element.Border.rounded 6
        , Element.spacing 50
        , Element.Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 20
            , color = Element.rgba255 0 0 0 0.1
            }
        , Element.Border.rounded 10
        , Element.padding 20
        ]
        [ EH.coolCurrencyHbreak False Element.fill
        , Element.column
            [ Element.centerX
            , Element.width (Element.fill |> Element.maximum 700)
            , Element.spacing 20
            ]
            [ Element.paragraph
                [ Element.Font.size 70
                , Element.Font.bold
                , Element.centerX
                , Element.Font.center
                ]
                [ Element.text "DAI"
                , redText "Hard"
                ]
            , Element.row
                [ Element.Font.size 30
                , Element.Font.semiBold
                , Element.centerX
                , Element.Font.center
                , Element.Font.italic
                ]
                [ Element.text "The "
                , redText "Global"
                , Element.text ", "
                , redText "Unkillable"
                , Element.text " Crypto Gateway"
                ]
            ]
        , Element.row
            [ Element.Background.color EH.darkGray
            , Element.spacing 2
            , Element.centerX
            , Element.alignTop
            , Element.width Element.fill
            ]
            [ Element.el
                [ Element.paddingXY 40 10
                , Element.Background.color EH.white
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.column
                    [ Element.spacing 60
                    , Element.alignRight
                    ]
                    [ Element.el
                        [ Element.Font.size 40
                        , Element.Font.semiBold
                        , Element.centerX
                        ]
                        (Element.text "Want to learn more?")
                    
                    , Element.column
                        [ Element.spacing 15]
                        [ Element.link
                            [ Element.Border.rounded 4
                            , Element.width Element.fill
                            , Element.pointer
                            , Element.paddingXY 25 17
                            , Element.Background.color EH.blue
                            , Element.Font.color EH.white
                            , Element.Font.bold
                            , Element.Font.size 24
                            , Element.centerX
                            ]
                            { url = "https://daihardhome.wpcomstaging.com/2019/07/24/re-launch-test-post/"
                            , label =
                                Element.paragraph
                                    [ Element.Font.center ]
                                    [ Element.text "Go to the DAIHard Info page" ]
                            }
                        , Element.link
                            [ Element.Border.rounded 4
                            , Element.width Element.fill
                            , Element.pointer
                            , Element.paddingXY 25 17
                            , Element.Background.color EH.blue
                            , Element.Font.color EH.white
                            , Element.Font.bold
                            , Element.Font.size 24
                            , Element.centerX
                            ]
                            { url = "https://t.me/daihardexchange_group"
                            , label =
                                Element.paragraph
                                    [ Element.Font.center ]
                                    [ Element.text "Join the Telegram Group" ]
                            }
                        ]
                    ]
                )
            , Element.el
                [ Element.paddingXY 40 10
                , Element.Background.color EH.white
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.column
                    [ Element.alignLeft
                    , Element.spacing 60
                    ]
                    [ Element.el
                        [ Element.Font.size 40
                        , Element.Font.semiBold
                        , Element.centerX
                        ]
                        (Element.text "Ready to try it out?")
                    , Element.column
                        [ Element.spacing 10
                        , Element.width <| Element.px 400
                        , Element.centerX
                        ]
                        [ Element.paragraph
                            [ Element.Font.size 24
                            , Element.width Element.fill
                            ]
                            [ Element.text "Make sure your web3 wallet is unlocked (we recommend using Nifty Wallet over Metamask)."
                            ]
                        , Element.paragraph
                            [ Element.Font.size 24
                            , Element.width Element.fill
                            ]
                            [ Element.text "Then follow the links at the top of this page to browse "
                            , boldText "Sell Offers"
                            , Element.text ", browse "
                            , boldText "Buy Offers"
                            , Element.text ", or "
                            , boldText "Create Your Own Offer"
                            , Element.text "."
                            ]
                        ]
                    ]
                )
            ]
        , Element.el
            [ Element.alignBottom
            , Element.width Element.fill
            ]
          <|
            EH.coolCurrencyHbreak True Element.fill
        ]
