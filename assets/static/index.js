var elm_ethereum_ports = require('elm-ethereum-ports');

import { Elm } from '../../src/Main'

window.addEventListener('load', function () {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            window.app = Elm.Main.init({
                node: document.getElementById('elm'),
                flags: [parseInt(networkId), 18]
            });
            elm_ethereum_ports.txSentry(window.app.ports.txOut, app.ports.txIn, web3);
            elm_ethereum_ports.walletSentry(window.app.ports.walletSentryPort, web3);
        });
    } else {
        window.app = Elm.Main.init({
            node: document.getElementById('elm'),
            flags: [0, 18]
        });
        console.log("Metamask not detected.");
    }
});
