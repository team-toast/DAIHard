var elm_ethereum_ports = require('elm-ethereum-ports');
var secureComms = require('./secureComms');
var networkChangeNotifier = require('./networkChangeNotifier');
var elmNotifications = require('./elmNotifications');
window.forge = require('node-forge');

import { Elm } from '../../elm/App'

//window.testStuff = secureComms.testStuff;
window.web3Connected = false;

window.addEventListener('load', function () {
    startDapp();
});

function startDapp() {
    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            var id;
            if (e) {
                console.log("Error initializing web3: " + e);
                id = 0; // 0 indicates no network set by provider
            }
            else {
                id = parseInt(networkId);
            }
            window.app = Elm.App.init({
                node: document.getElementById('elm'),
                flags: {
                    networkId: id,
                    width: window.innerWidth,
                    height: window.innerHeight,
                    nowInMillis: Date.now()
                }
            });

            gtagPortStuff(app);
            notificationPortStuff(app);

            web3PortStuff(app, web3);
        });
    } else {
        window.app = Elm.App.init({
            node: document.getElementById('elm'),
            flags: {
                networkId: 0, // 0 indicates no network set by provider
                width: window.innerWidth,
                height: window.innerHeight,
                nowInMillis: Date.now()
            }
        });

        gtagPortStuff(app);
        notificationPortStuff(app);

        console.log("Metamask not detected.");
    }
}

function notificationPortStuff(app) {
    app.ports.notifyPort.subscribe(function (obj) {
        elmNotifications.notify(obj.title, obj.body, obj.image);
    });

    app.ports.requestNotifyPermissionPort.subscribe(function () {
        elmNotifications.requestPermission(function (callback) { });
    });
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
