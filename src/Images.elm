module Images exposing
    ( Image
    , addButton
    , backButton
    , chatIcon
    , daiSymbol
    , downArrow
    , image
    , loadingArrows
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
import Time


type Image
    = None
    | JustImage
        { src : String
        , description : String
        }


toElement : List (Attribute msg) -> Image -> Element msg
toElement attributes image_ =
    case image_ of
        None ->
            Element.none

        JustImage img ->
            Element.image attributes img


none =
    None


image =
    JustImage


marginSymbol : Bool -> Maybe Bool -> Image
marginSymbol isUp maybeIsGreen =
    JustImage
        { src =
            case ( isUp, maybeIsGreen ) of
                ( True, Just True ) ->
                    "/static/img/margin-up-green.svg"

                ( True, Just False ) ->
                    "/static/img/margin-up-red.svg"

                ( True, Nothing ) ->
                    "/static/img/margin-up.svg"

                ( False, Just True ) ->
                    "/static/img/margin-down-green.svg"

                ( False, Just False ) ->
                    "/static/img/margin-down-red.svg"

                ( False, Nothing ) ->
                    "/static/img/margin-down.svg"
        , description = ""
        }


daiSymbol : Image
daiSymbol =
    JustImage
        { src = "/static/img/dai-symbol.png"
        , description = "DAI"
        }


downArrow : Image
downArrow =
    JustImage
        { src = "/static/img/arrow-down.svg"
        , description = "down"
        }


upArrow : Image
upArrow =
    JustImage
        { src = "/static/img/arrow-up.svg"
        , description = "up"
        }


qmarkCircle : Image
qmarkCircle =
    JustImage
        { src = "/static/img/qmark-circle.svg"
        , description = ""
        }


openWindowIcon : Image
openWindowIcon =
    JustImage
        { src = "/static/img/open-window-icon.svg"
        , description = ""
        }


paymentWindowIcon : Image
paymentWindowIcon =
    JustImage
        { src = "/static/img/payment-window-icon.svg"
        , description = ""
        }


releaseWindowIcon : Image
releaseWindowIcon =
    JustImage
        { src = "/static/img/release-window-icon.svg"
        , description = ""
        }


addButton : Image
addButton =
    JustImage
        { src = "/static/img/add-button.svg"
        , description = "add"
        }


pmCash : Image
pmCash =
    JustImage
        { src = "/static/img/pm-cash.svg"
        , description = "cash"
        }


pmBank : Image
pmBank =
    JustImage
        { src = "/static/img/pm-bank.svg"
        , description = "bank"
        }


pmCustom : Image
pmCustom =
    JustImage
        { src = "/static/img/pm-custom.svg"
        , description = "custom"
        }


backButton : Image
backButton =
    JustImage
        { src = "/static/img/back-button.svg"
        , description = "back"
        }


chatIcon : Image
chatIcon =
    JustImage
        { src = "/static/img/chat-icon.svg"
        , description = "chat"
        }


loadingArrows : Image
loadingArrows =
    JustImage
        { src = "/static/img/loading-arrows.svg"
        , description = "waiting"
        }
