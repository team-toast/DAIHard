----------------------------------------------------------------------
--
-- (yanked from https://github.com/billstclair/elm-system-notification/blob/1.0.4/SystemNotification.elm)
-- SystemNotification.elm
-- Module for using the JavaScript Notification API.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Notifications exposing (createNotification, NotifyPort)

{-| A simple module for creating system notifications using the JavaScript [Notifications API](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API).

@docs createNotification, NotifyPort

-}

import Json.Encode as JE exposing (Value)


{-| You need to define a `port` with this type. E.g.:
port notify : NotifyPort msg
-}
type alias NotifyPort msg =
    Value -> Cmd msg


{-| Send a notification out through the NotifyPort with the given title and optional body and imageUrl.
createNotification notify
title
(Just body)
(Just imageUrl)
Requests permission first. Most browsers cache your answer, or allow you to say whether you want them to remember it. If permission is denied, no notifications will appear, and your program won't know that.
-}
createNotification : NotifyPort msg -> String -> Maybe String -> Maybe String -> Cmd msg
createNotification notifyPort title body imageUrl =
    let
        value =
            JE.object
                [ ( "title", JE.string title )
                , ( "body"
                  , case body of
                        Nothing ->
                            JE.null

                        Just b ->
                            JE.string b
                  )
                , ( "image"
                  , case imageUrl of
                        Nothing ->
                            JE.null

                        Just i ->
                            JE.string i
                  )
                ]
    in
    notifyPort value
