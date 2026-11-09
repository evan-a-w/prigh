// The cross-implementation vector for a browser sign-in's paste-back code:
// fixed keys and nonce, sealed by the page's own paste_seal.js (in Node here,
// in each browser in cli_signin.mjs). The backend's tests open
// the code this prints (backend/test/paste_vector.ml); keep them in step.
//
//   node frontend/web/e2e/paste_vector.mjs        prints the vector as JSON
//   import { vector, sealSource } from "./paste_vector.mjs"
import { createPrivateKey, createPublicKey } from "node:crypto";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const sealSource = readFileSync(
  join(dirname(fileURLToPath(import.meta.url)), "..", "app", "paste_seal.js"),
  "utf8",
);

const b64u = bytes => Buffer.from(bytes).toString("base64url");
const bytes = (from, n) => Uint8Array.from({ length: n }, (_, i) => from + i);
const x25519Public = secret =>
  createPublicKey(
    createPrivateKey({
      key: Buffer.concat([Buffer.from("302e020100300506032b656e04220420", "hex"), Buffer.from(secret)]),
      format: "der",
      type: "pkcs8",
    }),
  ).export({ format: "jwk" }).x;

const programSecret = bytes(1, 32);
const ephemeralSecret = bytes(33, 32);
export const inputs = {
  programSecret: b64u(programSecret),
  programPublic: x25519Public(programSecret),
  state: "0123456789abcdef0123456789abcdef",
  plaintext: JSON.stringify({ cookie: "proxy_session=s3cret", token: "sekrit", user: "alice" }),
  fixed: { secret: b64u(ephemeralSecret), public: x25519Public(ephemeralSecret), nonce: b64u(bytes(0xa0, 12)) },
};

// What the code must be: the backend's test checks the same string.
export const vector =
  "prigh1:WGmv9FBUlzLLqu1eXfmzCm2jHLDldCutWtShp2jxpnugoaKjpKWmp6ipqqsX8VRaKw-Xbo3_3ST_iBU4rzNOgjGkmVsO3vPMlN6CcJgIOXlZCLis7BV1Z4g11IagVBAlcqXyYNFdJTxsEEiSbI8nHMyx90rt-3nTY04i3nY";

if (import.meta.url === `file://${process.argv[1]}`) {
  const seal = new Function(`${sealSource}\nreturn prigh_paste_seal;`)();
  const { programPublic, state, plaintext, fixed } = inputs;
  const code = await seal(programPublic, state, plaintext, fixed);
  console.log(JSON.stringify({ ...inputs, code }, null, 2));
  if (code !== vector) {
    console.error(`the recorded vector differs: update it here and in backend/test/paste_vector.ml`);
    process.exitCode = 1;
  }
}
