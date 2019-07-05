var elm_ethereum_ports = require('elm-ethereum-ports');
var secureComms = require('./secureComms');
var networkChangeNotifier = require('./networkChangeNotifier');
window.forge = require('node-forge');

import { Elm } from '../../src/App'

//window.testStuff = secureComms.testStuff;
window.web3Connected = false;

window.addEventListener('load', function () {
    startDapp();
});

function startDapp() {
    if (typeof web3 !== 'undefined') {
        if (window.ethereum) {
            window.ethereum.autoRefreshOnNetworkChange = false;
        }

        web3.version.getNetwork(function (e, networkId) {
            window.app = Elm.App.init({
                node: document.getElementById('elm'),
                flags: {
                    networkId: parseInt(networkId),
                    width: window.innerWidth,
                    height: window.innerHeight
                }
            });

            gtagPortStuff(app);

            web3PortStuff(app, web3);
        });
    } else {
        window.app = Elm.App.init({
            node: document.getElementById('elm'),
            flags: {
                networkId: parseInt(0), // 0 indicates no network set by provider
                width: window.innerWidth,
                height: window.innerHeight
            }
        });

        gtagPortStuff(app);

        web3PortStuff(app, web3);

        console.log("Metamask not detected.");
    }
}

function gtagPortStuff(app) {
    app.ports.gTagOut.subscribe(function (data) {
        gtag('event', data.event, {
            'event_category': data.category,
            'event_label': data.label,
            'value': data.value
        });
    });
}

function web3PortStuff(app, web3) {
    prepareWeb3PortsPreConnect(app, web3);

    web3.eth.getAccounts(function (e, res) {
        if (res && res.length > 0) {
            connectAndPrepareRemainingWeb3Ports(app, web3);
        }
    });
}

function prepareWeb3PortsPreConnect(app, web3) {
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    app.ports.connectToWeb3.subscribe(function (data) {
        connectAndPrepareRemainingWeb3Ports(app, web3);
    });
}

function connectAndPrepareRemainingWeb3Ports(app, web3) {
    if (window.ethereum && !window.web3Connected) {
        window.web3 = new Web3(ethereum);
    }

    elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
    elm_ethereum_ports.walletSentry(app.ports.walletSentryPort, web3);
    networkChangeNotifier.startWatching(app.ports.networkSentryPort, web3);

    app.ports.genPrivkey.subscribe(function (args) {
        secureComms.prepareKeypair(args.signSeedMsg, args.address, function (err, res) {
            console.log("pubkey: ", res)
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

        app.ports.decryptionFinished.send({ id: id, message: result })
    });

    if (window.ethereum && !window.web3Connected) {
        ethereum.enable();
        window.web3Connected = true;
    }
}
