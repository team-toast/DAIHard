var elm_ethereum_ports = require('elm-ethereum-ports');

var Elm = require( '../../src/Main' );

window.addEventListener('load', function () {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            app = Elm.Main.fullscreen([parseInt(networkId), "0x66655D8A18d89463d68Be89bEf512FCC84872cC9"]);
            elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
            elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
        });
    } else {
        app = Elm.Main.fullscreen([0, "0x0"]);
        console.log("Metamask not detected.");
    }
});
