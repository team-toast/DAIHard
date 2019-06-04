module Create.PMWizard.View exposing (root)

import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Create.PMWizard.Types exposing (..)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Helpers.Element as EH
import Images exposing (Image)
import PaymentMethods exposing (PaymentMethod)


root : Model -> BuyerOrSeller -> Element Msg
root model userRole =
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
            (bodyElement model userRole)
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
        [ Element.el
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


getTitle : Model -> String
getTitle model =
    "Add Payment Methods"


bodyElement : Model -> BuyerOrSeller -> Element Msg
bodyElement model userRole =
    case model of
        ChooseType ->
            chooseTypeElement userRole

        Details paymentMethod ->
            detailsElement paymentMethod userRole


chooseTypeElement : BuyerOrSeller -> Element Msg
chooseTypeElement userRole =
    Element.column [ Element.spacing 32 ]
        [ Element.row [ Element.spacing 32 ]
            [ pmTypeElement PaymentMethods.Cash userRole
            , pmTypeElement PaymentMethods.Bank userRole
            ]
        , pmTypeElement PaymentMethods.Custom userRole
        ]


pmTypeElement : PaymentMethods.Type -> BuyerOrSeller -> Element Msg
pmTypeElement pmType userRole =
    let
        icon =
            PaymentMethods.typeToIcon pmType

        title =
            typeTitle pmType

        summary =
            typeSummary pmType userRole

        onClick =
            SelectType pmType
    in
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


detailsElement : PaymentMethod -> BuyerOrSeller -> Element Msg
detailsElement paymentMethod userRole =
    Element.el
        [ Element.padding 20
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        , EH.roundBottomCorners 5
        ]
        (Element.column
            [ Element.spacing 20
            , Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.Input.multiline
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                { onChange = ChangeDetails
                , text = paymentMethod.info
                , placeholder =
                    if paymentMethod.info == "" then
                        Just <| inputPlaceholder paymentMethod userRole

                    else
                        Nothing
                , label = Element.Input.labelHidden "details"
                , spellcheck = True
                }
            , Element.row
                [ Element.alignRight
                , Element.spacing 18
                ]
                [ EH.blueButton "Save" Save
                ]
            ]
        )


typeTitle : PaymentMethods.Type -> String
typeTitle pmType =
    case pmType of
        PaymentMethods.Cash ->
            "Cash Drop/Handoff"

        PaymentMethods.Bank ->
            "Bank Transfer"

        PaymentMethods.Custom ->
            "Custom"


typeSummary : PaymentMethods.Type -> BuyerOrSeller -> String
typeSummary pmType userRole =
    case ( pmType, userRole ) of
        ( PaymentMethods.Cash, Buyer ) ->
            "Indicate specific or general locations where you’re happy to meet with the seller or drop off the cash."

        ( PaymentMethods.Cash, Seller ) ->
            "Indicate specific or general locations where you’re happy to meet with the buyer or pick up the cash."

        ( PaymentMethods.Bank, Buyer ) ->
            "Indicate where you can make bank transfers to, either nationally or with specific banks."

        ( PaymentMethods.Bank, Seller ) ->
            "Indicate where you will accept bank transfers from, either nationally or with specific banks."

        ( PaymentMethods.Custom, _ ) ->
            "Indicate specific, custom terms for this trade."


inputPlaceholder : PaymentMethod -> BuyerOrSeller -> Element.Input.Placeholder Msg
inputPlaceholder paymentMethod userRole =
    Element.Input.placeholder
        []
        (Element.text """Put lots of detail here. You can provide multiple options.
If you are accepting a bank transfer, include a routing/ABA/IBAN number, to avoid ambiguity.

Some examples for Sellers:

I can accept transfers to a Schwab bank account (routing 121202211)
I can meet in person to accept cash in London, weekdays after 6, with a day of notice.

Some examples for Buyers:

I can deliver cash anywhere within an hour drive of Phoneix, AZ, with 2 days of notice.
I can send money via TransferWise
""")
