'use strict';

var forge = require('node-forge');

var commonModule = (function () {
    var userKeypair;

    var pub = {};

    pub.genKeypair = function (signSeedMsg, web3Account, callback) {
        web3.personal.sign(web3.fromUtf8(signSeedMsg), web3Account, function (err, sigString) {
            if (err) {
                return callback("error signing signSeedMsg: " + toString(err), null);
            }
            var prng = forge.random.createInstance();
            prng.seedFileSync = function (needed) {
                return repeatAndTruncateToLength(sigString, needed);
            }

            userKeypair = forge.rsa.generateKeyPair({ bits: 1024, prng: prng, algorithm: 'PRIMEINC' });

            console.log("hexed pubkey n", userKeypair.publicKey.n.toString(16));

            callback(null, userKeypair.publicKey.n.toString(16));
        });
    };

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