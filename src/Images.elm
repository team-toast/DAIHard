module Images exposing
    ( Image
    , addButton
    , backButton
    , chatIcon
    , closeIcon
    , daiSymbol
    , downArrow
    , flame
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
                    "/DAIHard/static/img/margin-up-green.svg"

                ( True, Just False ) ->
                    "/DAIHard/static/img/margin-up-red.svg"

                ( True, Nothing ) ->
                    "/DAIHard/static/img/margin-up.svg"

                ( False, Just True ) ->
                    "/DAIHard/static/img/margin-down-green.svg"

                ( False, Just False ) ->
                    "/DAIHard/static/img/margin-down-red.svg"

                ( False, Nothing ) ->
                    "/DAIHard/static/img/margin-down.svg"
        , description = ""
        }


daiSymbol : Image
daiSymbol =
    JustImage
        { src = "/DAIHard/static/img/dai-symbol.png"
        , description = "DAI"
        }


downArrow : Image
downArrow =
    JustImage
        { src = "/DAIHard/static/img/arrow-down.svg"
        , description = "down"
        }


upArrow : Image
upArrow =
    JustImage
        { src = "/DAIHard/static/img/arrow-up.svg"
        , description = "up"
        }


qmarkCircle : Image
qmarkCircle =
    JustImage
        { src = "/DAIHard/static/img/qmark-circle.svg"
        , description = ""
        }


openWindowIcon : Image
openWindowIcon =
    JustImage
        { src = "/DAIHard/static/img/open-window-icon.svg"
        , description = ""
        }


paymentWindowIcon : Image
paymentWindowIcon =
    JustImage
        { src = "/DAIHard/static/img/payment-window-icon.svg"
        , description = ""
        }


releaseWindowIcon : Image
releaseWindowIcon =
    JustImage
        { src = "/DAIHard/static/img/release-window-icon.svg"
        , description = ""
        }


addButton : Image
addButton =
    JustImage
        { src = "/DAIHard/static/img/add-button.svg"
        , description = "add"
        }


pmCash : Image
pmCash =
    JustImage
        { src = "/DAIHard/static/img/pm-cash.svg"
        , description = "cash"
        }


pmBank : Image
pmBank =
    JustImage
        { src = "/DAIHard/static/img/pm-bank.svg"
        , description = "bank"
        }


pmCustom : Image
pmCustom =
    JustImage
        { src = "/DAIHard/static/img/pm-custom.svg"
        , description = "custom"
        }


backButton : Image
backButton =
    JustImage
        { src = "/DAIHard/static/img/back-button.svg"
        , description = "back"
        }


chatIcon : Image
chatIcon =
    JustImage
        { src = "/DAIHard/static/img/chat-icon.svg"
        , description = "chat"
        }


loadingArrows : Image
loadingArrows =
    JustImage
        { src = "/DAIHard/static/img/loading-arrows.svg"
        , description = "waiting"
        }


closeIcon : Image
closeIcon =
    JustImage
        { src = "/DAIHard/static/img/remove-circle.svg"
        , description = "close"
        }


flame : Image
flame =
    JustImage
        { src = "/DAIHard/static/img/flame.png"
        , description = "flame"
        }
