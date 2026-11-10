// The paste-back code of a browser sign-in (the backend's Paste_code opens
// it): the grant, sealed to the waiting program's one-time X25519 key.
//
//   shared = X25519(ephemeral secret, program's public key)
//   key    = HKDF-SHA256(shared, salt = ephemeral public || program public,
//                        info = "prigh1 cli-signin"), 32 bytes
//   sealed = AES-256-GCM(key, 12-byte random nonce, plaintext,
//                        associated data = the state)
//   code   = "prigh1:" base64url(ephemeral public || nonce || sealed), no padding
//
// Plain ES5 plus Promises: js_of_ocaml links it into the page (an external),
// and the tests load the same file into Node and each browser to check the
// recorded vector.

//Provides: prigh_paste_seal
function prigh_paste_seal(programPublic, state, plaintext, fixed) {
  function fromB64(s) {
    var bin = atob(s.replace(/-/g, "+").replace(/_/g, "/"));
    var out = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
  }
  function toB64(bytes) {
    var bin = "";
    for (var i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
    return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  }
  function concat(parts) {
    var length = 0;
    for (var i = 0; i < parts.length; i++) length += parts[i].length;
    var out = new Uint8Array(length);
    var at = 0;
    for (var j = 0; j < parts.length; j++) { out.set(parts[j], at); at += parts[j].length; }
    return out;
  }
  var subtle = globalThis.crypto && globalThis.crypto.subtle;
  if (!subtle) {
    var insecure = new Error("WebCrypto is unavailable (not a secure context)");
    insecure.name = "InsecureContextError";
    return Promise.reject(insecure);
  }
  var programKey = fromB64(programPublic);
  var encoder = new TextEncoder();
  var ephemeral = fixed
    ? subtle.importKey(
        "jwk",
        { kty: "OKP", crv: "X25519", d: fixed.secret, x: fixed.public, ext: true },
        { name: "X25519" },
        true,
        ["deriveBits"]
      ).then(function (privateKey) {
        return { privateKey: privateKey, publicRaw: fromB64(fixed.public) };
      })
    : subtle.generateKey({ name: "X25519" }, true, ["deriveBits"]).then(function (pair) {
        return subtle.exportKey("raw", pair.publicKey).then(function (raw) {
          return { privateKey: pair.privateKey, publicRaw: new Uint8Array(raw) };
        });
      });
  return ephemeral.then(function (eph) {
    return subtle.importKey("raw", programKey, { name: "X25519" }, false, [])
      .then(function (programPublicKey) {
        return subtle.deriveBits({ name: "X25519", public: programPublicKey }, eph.privateKey, 256);
      })
      .then(function (shared) {
        return subtle.importKey("raw", shared, "HKDF", false, ["deriveBits"]);
      })
      .then(function (ikm) {
        return subtle.deriveBits(
          {
            name: "HKDF",
            hash: "SHA-256",
            salt: concat([eph.publicRaw, programKey]),
            info: encoder.encode("prigh1 cli-signin"),
          },
          ikm,
          256
        );
      })
      .then(function (keyBytes) {
        return subtle.importKey("raw", keyBytes, { name: "AES-GCM" }, false, ["encrypt"]);
      })
      .then(function (key) {
        var nonce = fixed ? fromB64(fixed.nonce) : globalThis.crypto.getRandomValues(new Uint8Array(12));
        return subtle.encrypt(
          { name: "AES-GCM", iv: nonce, additionalData: encoder.encode(state) },
          key,
          encoder.encode(plaintext)
        ).then(function (sealed) {
          return "prigh1:" + toB64(concat([eph.publicRaw, nonce, new Uint8Array(sealed)]));
        });
      });
  });
}
