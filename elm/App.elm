module App exposing (main)

import Browser.Hash as Hash
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Hash.application
        { init = State.init
        , view = View.root
        , update = State.update
        , subscriptions = State.subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
