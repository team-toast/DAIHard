module Images exposing (Image, daiSymbol, marginSymbol)

import Element exposing (Attribute, Element)


type alias Image =
    { src : String
    , description : String
    }


marginSymbol : Bool -> Bool -> Image
marginSymbol isUp isGreen =
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
    { src = "static/img/dai-symbol.png"
    , description = "DAI"
    }
