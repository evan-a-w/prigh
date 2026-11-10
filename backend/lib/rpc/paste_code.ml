open! Core
open! Import
module X25519 = Mirage_crypto_ec.X25519

type t =
  { secret : X25519.secret
  ; public : string
  ; state : string
  ; expires : Time_ns.t
  ; mutable used : bool
  }

let prefix = "prigh1:"
let info = "prigh1 cli-signin"

let base64url_encode s =
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet s
;;

let base64url_decode s =
  Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s |> Result.ok
;;

let create ?secret ~state ~expires () =
  let secret, public =
    match secret with
    | Some secret ->
      (match X25519.secret_of_octets secret with
       | Ok key -> key
       | Error _ -> raise_s [%message "Paste_code.create: not an X25519 key"])
    | None ->
      Mirage_crypto_rng_unix.use_default ();
      X25519.gen_key ()
  in
  { secret; public; state; expires; used = false }
;;

let public_key t = base64url_encode t.public

module Error = struct
  type t =
    | Not_a_code
    | Not_for_this_signin
    | Bad_contents
    | Expired
    | Used
  [@@deriving sexp_of]

  let to_string_hum = function
    | Not_a_code ->
      "that is not a sign-in code (it starts with prigh1:): copy it with the \
       page's Copy button and paste it again"
    | Not_for_this_signin ->
      "that code is not for this sign-in (from an earlier one, or changed on \
       the way): paste the code the page shows for this link, or open the link \
       again"
    | Bad_contents ->
      "the code holds no sign-in cookie: update prigh-web, or start the \
       sign-in again"
    | Expired -> "this sign-in expired: start it again"
    | Used -> "this sign-in already took a code: start it again"
  ;;
end

let key_size = 32
let nonce_size = 12
let tag_size = 16

let grant_of_json plaintext =
  match Jsonaf.parse plaintext with
  | Error _ -> None
  | Ok json ->
    let string name =
      match Jsonaf.member name json with
      | Some (`String s) when not (String.is_empty s) -> Some s
      | _ -> None
    in
    Option.map (string "cookie") ~f:(fun proxy_cookie ->
      { Browser_signin_grant.proxy_cookie
      ; token = string "token"
      ; user = string "user"
      })
;;

let open_ t ~now code =
  let code = String.filter code ~f:(Fn.non Char.is_whitespace) in
  let%bind.Result raw =
    match String.chop_prefix code ~prefix with
    | None -> Error Error.Not_a_code
    | Some rest ->
      (match base64url_decode rest with
       | Some raw when String.length raw >= key_size + nonce_size + tag_size ->
         Ok raw
       | Some _ | None -> Error Error.Not_a_code)
  in
  if t.used
  then Error Error.Used
  else if Time_ns.( >= ) now t.expires
  then Error Error.Expired
  else (
    let ephemeral = String.sub raw ~pos:0 ~len:key_size in
    let nonce = String.sub raw ~pos:key_size ~len:nonce_size in
    let sealed = String.subo raw ~pos:(key_size + nonce_size) in
    let opened =
      match X25519.key_exchange t.secret ephemeral with
      | Error _ -> None
      | Ok shared ->
        let prk =
          Hkdf.extract ~hash:`SHA256 ~salt:(ephemeral ^ t.public) shared
        in
        let key =
          Mirage_crypto.AES.GCM.of_secret
            (Hkdf.expand ~hash:`SHA256 ~prk ~info key_size)
        in
        Mirage_crypto.AES.GCM.authenticate_decrypt
          ~key
          ~nonce
          ~adata:t.state
          sealed
    in
    match opened with
    | None -> Error Error.Not_for_this_signin
    | Some plaintext ->
      (match grant_of_json plaintext with
       | None -> Error Error.Bad_contents
       | Some grant ->
         t.used <- true;
         Ok grant))
;;
