var elm_ethereum_ports = require('elm-ethereum-ports');

import { Elm } from '../../src/Main'

window.addEventListener('load', function () {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            window.app = Elm.Main.init({
                node: document.getElementById('elm'),
                flags: {
                    tokenContractDecimals: 18,
                    networkId: parseInt(networkId),
                    tokenContractAddressString: "0xC4375B7De8af5a38a93548eb8453a498222C4fF2",
                    factoryAddressString: "0xe3608bafbb88782e3e4e119f03f3b6fc8c3cf975"
                }
            });
            elm_ethereum_ports.txSentry(window.app.ports.txOut, app.ports.txIn, web3);
            elm_ethereum_ports.walletSentry(window.app.ports.walletSentryPort, web3);
        });
    } else {
        window.app = Elm.Main.init({
            node: document.getElementById('elm'),
            flags: {
                tokenContractDecimals: 18,
                networkId: parseInt(networkId),
                tokenContractAddressString: "0xC4375B7De8af5a38a93548eb8453a498222C4fF2",
                factoryAddressString: "0xe3608bafbb88782e3e4e119f03f3b6fc8c3cf975"
            }
        });
        console.log("Metamask not detected.");
    }
});
