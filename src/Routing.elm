module Routing exposing (urlToRoute)

import Types exposing (..)
import Url exposing (Url)


urlToRoute : Url -> Route
urlToRoute url =
    Home
