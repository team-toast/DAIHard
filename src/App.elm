module App exposing (main)

import Browser
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Browser.application
        { init = State.initialState
        , view = View.root
        , update = State.update
        , subscriptions = State.subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
