module Images exposing
    ( Image
    , addButton
    , backButton
    , daiSymbol
    , downArrow
    , image
    , marginSymbol
    , none
    , openWindowIcon
    , paymentWindowIcon
    , pmBank
    , pmCash
    , pmCustom
    , qmarkCircle
    , releaseWindowIcon
    , toElement
    , upArrow
    )

import Element exposing (Attribute, Element)


type Image
    = None
    | Just
        { src : String
        , description : String
        }


toElement : List (Attribute msg) -> Image -> Element msg
toElement attributes image_ =
    case image_ of
        None ->
            Element.none

        Just img ->
            Element.image attributes img


none =
    None


image =
    Just


marginSymbol : Bool -> Bool -> Image
marginSymbol isUp isGreen =
    Just
        { src =
            case ( isUp, isGreen ) of
                ( True, True ) ->
                    "static/img/margin-up-green.svg"

                ( True, False ) ->
                    "static/img/margin-up-red.svg"

                ( False, True ) ->
                    "static/img/margin-down-green.svg"

                ( False, False ) ->
                    "static/img/margin-down-red.svg"
        , description = ""
        }


daiSymbol : Image
daiSymbol =
    Just
        { src = "static/img/dai-symbol.png"
        , description = "DAI"
        }


downArrow : Image
downArrow =
    Just
        { src = "static/img/arrow-down.svg"
        , description = "down"
        }


upArrow : Image
upArrow =
    Just
        { src = "static/img/arrow-up.svg"
        , description = "up"
        }


qmarkCircle : Image
qmarkCircle =
    Just
        { src = "static/img/qmark-circle.svg"
        , description = ""
        }


openWindowIcon : Image
openWindowIcon =
    Just
        { src = "static/img/open-window-icon.svg"
        , description = ""
        }


paymentWindowIcon : Image
paymentWindowIcon =
    Just
        { src = "static/img/payment-window-icon.svg"
        , description = ""
        }


releaseWindowIcon : Image
releaseWindowIcon =
    Just
        { src = "static/img/release-window-icon.svg"
        , description = ""
        }


addButton : Image
addButton =
    Just
        { src = "static/img/add-button.svg"
        , description = "add"
        }


pmCash : Image
pmCash =
    Just
        { src = "static/img/pm-cash.svg"
        , description = "cash"
        }


pmBank : Image
pmBank =
    Just
        { src = "static/img/pm-bank.svg"
        , description = "bank"
        }


pmCustom : Image
pmCustom =
    Just
        { src = "static/img/pm-custom.svg"
        , description = "custom"
        }


backButton : Image
backButton =
    Just
        { src = "static/img/back-button.svg"
        , description = "back"
        }
