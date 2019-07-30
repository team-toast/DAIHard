//////////////////////////////////////////////////////////////////////
//
// (yanked and modified from https://github.com/billstclair/elm-system-notification/blob/master/site/js/elmNotifications.js)
// elmNotifications.js
// JavaScript code for billstclair/elm-system-notifications
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

'use strict';

var commonModule = (function () {

    var pub = {};

    pub.isAvailable = function () {
        return typeof (Notification) == "function";
    };

    pub.requestPermission = function (callback) {
        var permission = Notification.permission;
        if (permission != "default") {
            callback(permission);
        } else {
            Notification.requestPermission(callback)
        }
    }

    pub.notify = function (title, body, img) {
        if (pub.isAvailable()) {
            pub.requestPermission(function (permission) {
                if (permission == "granted") {
                    var options = {};
                    if (body) { options.body = body };
                    if (img) { options.icon = img };
                    var notification = new Notification(title, options);
                    setTimeout(function () { notification.close.bind(notification)() }, 4000);
                }
            });
        }
    }

    return pub;
})();

module.exports = commonModule;