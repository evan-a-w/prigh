open! Core
open! Import

type t =
  { code : string
  ; reply : string
  ; mutable taken : bool
  }

let create ~user ~token ~prefer_host =
  Mirage_crypto_rng_unix.use_default ();
  let string = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  { code = Ohex.encode (Mirage_crypto_rng.generate 16)
  ; reply =
      Json.to_string
        (`Object
            [ "user", string user
            ; "token", string token
            ; "prefer_host", string prefer_host
            ])
  ; taken = false
  }
;;

let code t = t.code
let path = "/signin-handoff"

(* Compared by digest, so that the time taken says nothing about the code. *)
let matches t code =
  String.equal
    (Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string t.code))
    (Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string code))
;;

(* Typed, so that a browser does not try the body as XML. *)
let text flow ~status ~reason body =
  let response = Static_site.Response.text ~status ~reason body in
  Web_server.respond
    flow
    { response with
      headers =
        ("Content-Type", "text/plain; charset=utf-8") :: response.headers
    }
;;

let refuse flow =
  text
    flow
    ~status:403
    ~reason:"Forbidden"
    "this sign-in link was used already: sign in on the page, or run prigh web \
     again for a new link\n"
;;

let handle t (request : Web_server.Request.t) ~reader flow =
  match request.meth with
  | "POST" ->
    let length =
      Option.bind
        (Web_server.Request.header request "Content-Length")
        ~f:(fun n -> Int.of_string_opt (String.strip n))
    in
    (match length with
     | Some length when length > 0 && length <= 256 ->
       let code = String.strip (Eio.Buf_read.take length reader) in
       if (not t.taken) && matches t code
       then (
         t.taken <- true;
         Web_server.respond
           flow
           { status = 200
           ; reason = "OK"
           ; headers = [ "Content-Type", "application/json" ]
           ; content_length = Some (String.length t.reply)
           ; body = t.reply
           })
       else refuse flow
     | Some _ | None -> refuse flow)
  | _ ->
    text flow ~status:405 ~reason:"Method Not Allowed" "method not allowed\n"
;;

let route t (request : Web_server.Request.t) =
  Option.some_if (String.equal request.path path) (handle t)
;;
