'use strict';

var commonModule = (function () {
    var lastNetworkId;

    var notifyPort;
    var web3;

    var pub = {};

    pub.startWatching = function (_notifyPort, _web3) {
        notifyPort = _notifyPort;
        web3 = _web3;
        // setInterval(checkNetworkId, 100);
    }

    // var checkNetworkId = function () {
    //     web3.version.getNetwork(function (e, networkId) {
    //         if (e) {
    //             console.log("error with web3.version.getNetwork: ", e);
    //         }
    //         else if (networkId != lastNetworkId) {
    //             notifyPort.send(parseInt(networkId));
    //             lastNetworkId = networkId;
    //         }
    //     });
    // }

    return pub;
})();

module.exports = commonModule;