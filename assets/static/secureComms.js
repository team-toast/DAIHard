'use strict';

var forge = require('node-forge');

var commonModule = (function () {
    var userKeypair;

    var pub = {};

    var genCommKeySeedWithWeb3 = function (signSeedMsg, web3Account, seedCallback) {
        web3.personal.sign(web3.fromUtf8(signSeedMsg), web3Account, function (err, sig) {
            seedCallback(err, sig);
        });
    }

    var genKeypairFromSeed = function (seed) {
        var prng = forge.random.createInstance();
        prng.seedFileSync = function (needed) {
            return repeatAndTruncateToLength(seed, needed);
        }

        return forge.rsa.generateKeyPair({ bits: 1024, prng: prng, algorithm: 'PRIMEINC' });
    };

    pub.prepareKeypair = function (signSeedMsg, web3Account, pubkeyCallback) {
        //first try loading seed for the web3Account
        var storageKey = "commKeySeed:" + web3Account;

        if (typeof (Storage) !== "undefined") {
            var loadedSeed = localStorage.getItem(storageKey);
            if (loadedSeed != null) {
                userKeypair = genKeypairFromSeed(loadedSeed);
                console.log("generated comm key from saved seed.");

                return pubkeyCallback(null, userKeypair.publicKey.n.toString(16));
            }
        }

        //If we couldn't load it, generate and store it.
        genCommKeySeedWithWeb3(signSeedMsg, web3Account, function (err, generatedSeed) {
            if (err || generatedSeed == null) { // TFW you don't trust your JS function's error reporting
                return pubkeyCallback("error generating comm key seed: " + err, null);
            }
            localStorage.setItem(storageKey, generatedSeed);
            userKeypair = genKeypairFromSeed(generatedSeed);
            console.log("generated comm key from new seed.");

            return pubkeyCallback(null, userKeypair.publicKey.n.toString(16));
        });
    }

    pub.encryptToPubkeys = function (message, pubkeyStrings) {
        var encryptResults = [];

        for (var i = 0; i < pubkeyStrings.length; i++) {
            var pubkeyHexString = pubkeyStrings[i];
            var n = new forge.jsbn.BigInteger(pubkeyHexString, 16);
            var e = new forge.jsbn.BigInteger("65537");
            var pubkey = forge.pki.setRsaPublicKey(n, e);

            var kdf1 = new forge.kem.kdf1(forge.md.sha1.create());
            var kem = forge.kem.rsa.create(kdf1);

            var result = kem.encrypt(pubkey, 16);

            // encrypt some bytes
            var iv = forge.random.getBytesSync(12);
            var cipher = forge.cipher.createCipher('AES-GCM', result.key);
            cipher.start({ iv: iv });
            cipher.update(forge.util.createBuffer(message));
            cipher.finish();
            var encrypted = cipher.output.getBytes();
            var tag = cipher.mode.tag.getBytes();

            var encryptedData = { encrypted: encrypted, iv: iv, tag: tag, encapsulation: result.encapsulation }

            encryptResults.push(encryptedData);
        }

        return encryptResults;
    };

    pub.decryptForUser = function (encapsulation, iv, tag, encrypted) {
        var kdf1 = new forge.kem.kdf1(forge.md.sha1.create());
        var kem = forge.kem.rsa.create(kdf1);
        var key = kem.decrypt(userKeypair.privateKey, encapsulation, 16);

        // decrypt some bytes
        var decipher = forge.cipher.createDecipher('AES-GCM', key);
        decipher.start({ iv: iv, tag: tag });
        decipher.update(forge.util.createBuffer(encrypted));
        var pass = decipher.finish();

        if (pass) {
            return decipher.output.getBytes();
        } else return null;
    };

    pub.testStuff = function () {
        var kp1 = forge.rsa.generateKeyPair({ bits: 1024 });
        var kp2 = forge.rsa.generateKeyPair({ bits: 1024 });
        console.log(pub.encryptToPubkeys("hi there", [kp1.publicKey.n.toString(16), kp2.publicKey.n.toString(16)]));
    };

    return pub;
}());

function repeatAndTruncateToLength(str, len) {
    while (str.length < len) {
        str += str;
    }
    return str.substr(0, len);
}

module.exports = commonModule;