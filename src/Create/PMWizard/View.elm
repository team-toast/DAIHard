module Create.PMWizard.View exposing (root)

import Contracts.Types as CTypes
import Create.PMWizard.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import Images exposing (Image)
import PaymentMethods exposing (PaymentMethod)


root : Model -> CTypes.OpenMode -> Element Msg
root model openMode =
    Element.column
        [ Element.width <| Element.px 1050
        , Element.centerX
        ]
        [ Element.el [ Element.height <| Element.px 60 ] Element.none
        , Element.el
            [ Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundTopCorners 8
            , Element.Border.color EH.mediumGray
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
            , Element.height <| Element.px 610
            , Element.Background.color EH.lightGray
            , EH.roundBottomCorners 8
            , Element.paddingXY 48 39
            ]
            (bodyElement model openMode)
        ]


headerElement : Model -> Element Msg
headerElement model =
    let
        maybeBackButtonEl =
            case model of
                ChooseType ->
                    Element.none

                Details pm ->
                    Images.toElement
                        [ Element.pointer
                        , Element.Events.onClick Back
                        ]
                        Images.backButton
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 22
        ]
        [ maybeBackButtonEl
        , Element.el
            [ Element.Font.size 24
            , Element.Font.semiBold
            , Element.centerX
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


bodyElement : Model -> CTypes.OpenMode -> Element Msg
bodyElement model openMode =
    case model of
        ChooseType ->
            chooseTypeElement openMode

        Details paymentMethod ->
            detailsElement paymentMethod openMode


chooseTypeElement : CTypes.OpenMode -> Element Msg
chooseTypeElement openMode =
    Element.column [ Element.spacing 32 ]
        [ Element.row [ Element.spacing 32 ]
            [ cashTypeElement openMode
            , bankTypeElement openMode
            ]
        , customTypeElement openMode
        ]


cashTypeElement : CTypes.OpenMode -> Element Msg
cashTypeElement openMode =
    pmTypeElement
        Images.pmCash
        "Cash Drop/Handoff"
        "Indicate specific or general locations where you’re happy to meet with the buyer or drop of the cash."
        (SelectType PaymentMethods.Cash)


bankTypeElement : CTypes.OpenMode -> Element Msg
bankTypeElement openMode =
    pmTypeElement
        Images.pmBank
        "Bank Transfer"
        "Indicate where you will accept bank transfers from, either nationally or with specific banks."
        (SelectType PaymentMethods.Bank)


customTypeElement : CTypes.OpenMode -> Element Msg
customTypeElement openMode =
    pmTypeElement
        Images.pmCustom
        "Other"
        "Indicate specific, custom terms for this trade."
        (SelectType PaymentMethods.Custom)


pmTypeElement : Image -> String -> String -> Msg -> Element Msg
pmTypeElement icon title summary onClick =
    Element.column
        [ Element.width <| Element.px 450
        , Element.height <| Element.px 250
        , Element.Border.rounded 8
        , Element.Background.color EH.white
        , Element.centerX
        , Element.spacing 6
        , Element.padding 50
        , Element.pointer
        , Element.mouseOver [ Element.Background.color EH.lightBlue ]
        , Element.Events.onClick onClick
        ]
        [ Images.toElement [ Element.centerX ] icon
        , Element.el
            [ Element.Font.size 20
            , Element.Font.semiBold
            , Element.centerX
            ]
            (Element.text title)
        , Element.paragraph
            [ Element.Font.size 17
            , Element.Font.color EH.permanentTextColor
            , Element.centerX
            , Element.width <| Element.px 350
            , Element.Font.center
            ]
            [ Element.text summary ]
        ]


detailsElement : PaymentMethod -> CTypes.OpenMode -> Element Msg
detailsElement paymentMethod openMode =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 1
        ]
        [ Element.column
            [ Element.spacing 5
            , Element.padding 10
            , Element.width Element.fill
            , Element.Background.color EH.white
            , EH.roundTopCorners 5
            ]
            [ Element.el
                [ Element.Font.size 20
                , Element.Font.semiBold
                ]
                (Element.text "Transfer Details")
            , Element.el
                [ Element.Font.size 17
                , Element.Font.color EH.permanentTextColor
                ]
                (Element.text <| detailInputSubtitleText paymentMethod openMode)
            ]
        , Element.el
            [ Element.padding 10
            , Element.Background.color EH.white
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            (Element.Input.multiline
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                { onChange = ChangeDetails
                , text = paymentMethod.info
                , placeholder =
                    if paymentMethod.info == "" then
                        Just <| inputPlaceholder paymentMethod openMode

                    else
                        Nothing
                , label = Element.Input.labelHidden "details"
                , spellcheck = True
                }
            )
        ]


detailInputSubtitleText : PaymentMethod -> CTypes.OpenMode -> String
detailInputSubtitleText paymentMethod openMode =
    "don't be dumb"


inputPlaceholder : PaymentMethod -> CTypes.OpenMode -> Element.Input.Placeholder Msg
inputPlaceholder paymentMethod openMode =
    Element.Input.placeholder
        []
        (Element.text "i.e. hand it to me")
