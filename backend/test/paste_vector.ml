open! Core

(* The cross-implementation vector of a browser sign-in's paste-back code:
   what the page's own paste_seal.js makes from these inputs
   (frontend/web/e2e/paste_vector.mjs prints it, and cli_signin.mjs checks
   that every browser makes the same). *)

let program_secret =
  Base64.decode_exn
    ~pad:false
    ~alphabet:Base64.uri_safe_alphabet
    "AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyA"
;;

let state = "0123456789abcdef0123456789abcdef"

let code =
  "prigh1:WGmv9FBUlzLLqu1eXfmzCm2jHLDldCutWtShp2jxpnugoaKjpKWmp6ipqqsX8VRaKw-Xbo3_3ST_iBU4rzNOgjGkmVsO3vPMlN6CcJgIOXlZCLis7BV1Z4g11IagVBAlcqXyYNFdJTxsEEiSbI8nHMyx90rt-3nTY04i3nY"
;;
