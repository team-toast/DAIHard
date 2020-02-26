module Images exposing (Image(..), aborted, addButton, backButton, burned, chatIcon, closeIconBlack, closeIconWhite, committedPhaseBlack, committedPhaseWhite, confirmExtraInfoIcon, daiSymbol, downArrow, fiatBag, flame, image, judgmentPhaseBlack, judgmentPhaseWhite, loadingArrows, marginSymbol, marketplace, myTrades, navigateLeft, navigateRight, newTrade, none, numberOnCircleEl, openPhaseBlack, openPhaseWhite, openWindowIcon, pmBank, pmCash, pmCustom, qmarkCircle, release, releaseWindowIcon, released, searchIcon, stopWhite, swapArrows, threeDotsHorizontal, threeDotsVertical, toElement, upArrow, verticalSwapArrows)

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
                    "/img/margin-up-green.svg"

                ( True, Just False ) ->
                    "/img/margin-up-red.svg"

                ( True, Nothing ) ->
                    "/img/margin-up.svg"

                ( False, Just True ) ->
                    "/img/margin-down-green.svg"

                ( False, Just False ) ->
                    "/img/margin-down-red.svg"

                ( False, Nothing ) ->
                    "/img/margin-down.svg"
        , description = ""
        }


daiSymbol : Image
daiSymbol =
    JustImage
        { src = "/img/dai-symbol.png"
        , description = "DAI"
        }


downArrow : Image
downArrow =
    JustImage
        { src = "/img/arrow-down.svg"
        , description = "down"
        }


upArrow : Image
upArrow =
    JustImage
        { src = "/img/arrow-up.svg"
        , description = "up"
        }


qmarkCircle : Image
qmarkCircle =
    JustImage
        { src = "/img/qmark-circle.svg"
        , description = ""
        }


openWindowIcon : Image
openWindowIcon =
    JustImage
        { src = "/img/open-window-icon.svg"
        , description = ""
        }


fiatBag : Image
fiatBag =
    JustImage
        { src = "/img/fiat-bag-white.svg"
        , description = "fiat"
        }


stopWhite : Image
stopWhite =
    JustImage
        { src = "/img/stop-white.svg"
        , description = "stop"
        }


releaseWindowIcon : Image
releaseWindowIcon =
    JustImage
        { src = "/img/release-window-icon.svg"
        , description = ""
        }


addButton : Image
addButton =
    JustImage
        { src = "/img/add-button.svg"
        , description = "add"
        }


pmCash : Image
pmCash =
    JustImage
        { src = "/img/pm-cash.svg"
        , description = "cash"
        }


pmBank : Image
pmBank =
    JustImage
        { src = "/img/pm-bank.svg"
        , description = "bank"
        }


pmCustom : Image
pmCustom =
    JustImage
        { src = "/img/pm-custom.svg"
        , description = "custom"
        }


backButton : Image
backButton =
    JustImage
        { src = "/img/back-button.svg"
        , description = "back"
        }


chatIcon : Image
chatIcon =
    JustImage
        { src = "/img/chat.svg"
        , description = "chat"
        }


loadingArrows : Image
loadingArrows =
    JustImage
        { src = "/img/loading-arrows.svg"
        , description = "waiting"
        }


closeIconBlack : Image
closeIconBlack =
    JustImage
        { src = "/img/remove-circle-black.svg"
        , description = "close"
        }


closeIconWhite : Image
closeIconWhite =
    JustImage
        { src = "/img/remove-circle-white.svg"
        , description = "close"
        }


flame : Image
flame =
    JustImage
        { src = "/img/flame.png"
        , description = "flame"
        }


release : Image
release =
    JustImage
        { src = "/img/release.png"
        , description = "release"
        }


released : Image
released =
    JustImage
        { src = "/img/released.svg"
        , description = "released"
        }


aborted : Image
aborted =
    JustImage
        { src = "/img/aborted.svg"
        , description = "aborted"
        }


burned : Image
burned =
    JustImage
        { src = "/img/burned.svg"
        , description = "burned"
        }


openPhaseWhite : Image
openPhaseWhite =
    JustImage
        { src = "/img/phase-open-white.svg"
        , description = "open phase"
        }


committedPhaseWhite : Image
committedPhaseWhite =
    JustImage
        { src = "/img/phase-committed-white.svg"
        , description = "committed phase"
        }


judgmentPhaseWhite : Image
judgmentPhaseWhite =
    JustImage
        { src = "/img/phase-judgment-white.svg"
        , description = "judgment phase"
        }


openPhaseBlack : Image
openPhaseBlack =
    JustImage
        { src = "/img/phase-open-black.svg"
        , description = "open phase"
        }


committedPhaseBlack : Image
committedPhaseBlack =
    JustImage
        { src = "/img/phase-committed-black.svg"
        , description = "committed phase"
        }


judgmentPhaseBlack : Image
judgmentPhaseBlack =
    JustImage
        { src = "/img/phase-judgment-black.svg"
        , description = "judgment phase"
        }


numberOnCircleEl : Int -> Image
numberOnCircleEl number =
    JustImage
        { src =
            "/img/circle-"
                ++ String.fromInt number
                ++ ".svg"
        , description = String.fromInt number
        }


swapArrows : Image
swapArrows =
    JustImage
        { src = "/img/swap-arrows.svg"
        , description = "switch order type"
        }


searchIcon : Image
searchIcon =
    JustImage
        { src = "/img/search.svg"
        , description = "search"
        }


verticalSwapArrows : Image
verticalSwapArrows =
    JustImage
        { src = "/img/vertical-swap.svg"
        , description = "swap"
        }


marketplace : Image
marketplace =
    JustImage
        { src = "/img/farmer-s-market-kiosk-1.svg"
        , description = "marketplace"
        }


myTrades : Image
myTrades =
    JustImage
        { src = "/img/diamond.svg"
        , description = "myTrades"
        }


newTrade : Image
newTrade =
    JustImage
        { src = "/img/add-circle-bold.svg"
        , description = "newTrade"
        }


threeDotsHorizontal : Image
threeDotsHorizontal =
    JustImage
        { src = "/img/3-dots-big-horizontal.png"
        , description = "options"
        }


threeDotsVertical : Image
threeDotsVertical =
    JustImage
        { src = "/img/3-dots-small-vertical.svg"
        , description = "options"
        }


confirmExtraInfoIcon : Int -> Image
confirmExtraInfoIcon place =
    case place of
        0 ->
            JustImage
                { src = "/img/rocket.svg"
                , description = "info"
                }

        1 ->
            JustImage
                { src = "/img/open-window-white.svg"
                , description = "info"
                }

        2 ->
            JustImage
                { src = "/img/fishing-fish.svg"
                , description = "info"
                }

        3 ->
            JustImage
                { src = "/img/phase-committed-white.svg"
                , description = "info"
                }

        4 ->
            JustImage
                { src = "/img/legal-hammer.svg"
                , description = "info"
                }

        5 ->
            JustImage
                { src = "/img/flags.svg"
                , description = "info"
                }

        _ ->
            None


navigateLeft : Image
navigateLeft =
    JustImage
        { src = "/img/keyboard-arrow-left.svg"
        , description = "left"
        }


navigateRight : Image
navigateRight =
    JustImage
        { src = "/img/keyboard-arrow-right.svg"
        , description = "right"
        }
