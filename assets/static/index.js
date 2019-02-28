var elm_ethereum_ports = require('elm-ethereum-ports');
var secureComms = require('./secureComms');
window.forge = require('node-forge');

import { Elm } from '../../src/App'

window.factoryAddress = "0x9acEC8566208c6118780abE3E2086097875662e4";
window.tokenContractAddress = "0xC4375B7De8af5a38a93548eb8453a498222C4fF2";

window.testStuff = secureComms.testStuff;

window.addEventListener('load', function () {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            window.app = Elm.App.init({
                node: document.getElementById('elm'),
                flags: {
                    tokenContractDecimals: 18,
                    networkId: parseInt(networkId),
                    tokenContractAddressString: tokenContractAddress,
                    factoryAddressString: factoryAddress
                }
            });
            elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
            elm_ethereum_ports.walletSentry(app.ports.walletSentryPort, web3);
            app.ports.genPrivkey.subscribe(function (signSeedMsg) {
                secureComms.genKeypair(signSeedMsg, web3.eth.accounts[0], function (err, res) {
                    app.ports.userPubkeyResult.send(res);
                })
            });
            app.ports.encryptToPubkeys.subscribe(function (data) {
                var encryptedMessages = secureComms.encryptToPubkeys(data.message, data.pubkeyHexStrings);

                app.ports.encryptionFinished.send(encryptedMessages);
            });
            app.ports.decryptMessage.subscribe(function (data) {
                var id = data.id;

                var result = secureComms.decryptForUser(data.encapsulation, data.iv, data.tag, data.encrypted);
                if (!result) {
                    console.log("Uh oh! Decryption didn't work...");
                }
                console.log("decrypt result: ", result);

                app.ports.decryptionFinished.send({ id: id, message: result })
            });
        });
    } else {
        window.app = Elm.App.init({
            node: document.getElementById('elm'),
            flags: {
                tokenContractDecimals: 18,
                networkId: parseInt(networkId),
                tokenContractAddressString: tokenContractAddress,
                factoryAddressString: factoryAddress
            }
        });
        console.log("Metamask not detected.");
    }
});
