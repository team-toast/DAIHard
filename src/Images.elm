module Images exposing (Image(..), aborted, addButton, backButton, burned, chatIcon, closeIcon, committedPhase, daiSymbol, downArrow, fiatBag, flame, image, judgmentPhase, loadingArrows, marginSymbol, marketplace, myTrades, newTrade, none, numberOnCircleEl, openPhase, openWindowIcon, pmBank, pmCash, pmCustom, qmarkCircle, release, releaseWindowIcon, released, searchIcon, stopWhite, swapArrows, toElement, upArrow, verticalSwapArrows)

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
            Element.el attributes Element.none

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


fiatBag : Image
fiatBag =
    JustImage
        { src = "/DAIHard/static/img/fiat-bag-white.svg"
        , description = "fiat"
        }


stopWhite : Image
stopWhite =
    JustImage
        { src = "/DAIHard/static/img/stop-white.svg"
        , description = "stop"
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
        { src = "/DAIHard/static/img/chat.svg"
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


release : Image
release =
    JustImage
        { src = "/DAIHard/static/img/release.png"
        , description = "release"
        }


released : Image
released =
    JustImage
        { src = "/DAIHard/static/img/released.svg"
        , description = "released"
        }


aborted : Image
aborted =
    JustImage
        { src = "/DAIHard/static/img/aborted.svg"
        , description = "aborted"
        }


burned : Image
burned =
    JustImage
        { src = "/DAIHard/static/img/burned.svg"
        , description = "burned"
        }


openPhase : Image
openPhase =
    JustImage
        { src = "/DAIHard/static/img/diamond-give-white.svg"
        , description = "open phase"
        }


committedPhase : Image
committedPhase =
    JustImage
        { src = "/DAIHard/static/img/phase-committed.svg"
        , description = "committed phase"
        }


judgmentPhase : Image
judgmentPhase =
    JustImage
        { src = "/DAIHard/static/img/time-clock-fire.svg"
        , description = "judgment phase"
        }


numberOnCircleEl : Int -> Image
numberOnCircleEl number =
    JustImage
        { src =
            "/DAIHard/static/img/circle-"
                ++ String.fromInt number
                ++ ".svg"
        , description = String.fromInt number
        }


swapArrows : Image
swapArrows =
    JustImage
        { src = "/DAIHard/static/img/swap-arrows.svg"
        , description = "switch order type"
        }


searchIcon : Image
searchIcon =
    JustImage
        { src = "/DAIHard/static/img/search.svg"
        , description = "search"
        }


verticalSwapArrows : Image
verticalSwapArrows =
    JustImage
        { src = "/DAIHard/static/img/vertical-swap.svg"
        , description = "swap"
        }


marketplace : Image
marketplace =
    JustImage
        { src = "/DAIHard/static/img/farmer-s-market-kiosk-1.svg"
        , description = "marketplace"
        }


myTrades : Image
myTrades =
    JustImage
        { src = "/DAIHard/static/img/diamond.svg"
        , description = "myTrades"
        }


newTrade : Image
newTrade =
    JustImage
        { src = "/DAIHard/static/img/add-circle-bold.svg"
        , description = "newTrade"
        }
