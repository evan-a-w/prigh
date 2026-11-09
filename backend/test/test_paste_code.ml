open! Core
open! Prigh

(* The paste-back code of a browser sign-in, against the vector the page's
   own paste_seal.js made ({!Paste_vector}). *)

let program_secret = Paste_vector.program_secret
let state = Paste_vector.state
let vector = Paste_vector.code
let start = Time_ns.of_string_with_utc_offset "2026-11-09 12:00:00Z"
let expires = Time_ns.add start (Time_ns.Span.of_min 30.)

let key ?(secret = program_secret) ?(state = state) () =
  Paste_code.create ~secret ~state ~expires ()
;;

let show ?(now = start) t code =
  let result = Paste_code.open_ t ~now code in
  print_s
    [%sexp
      (Result.map_error result ~f:(fun e ->
         Paste_code.Error.sexp_of_t e, Paste_code.Error.to_string_hum e)
       : (Browser_signin_grant.t, Sexp.t * string) Result.t)];
  (* The secrets are opaque in the sexp: show that they came through. *)
  Result.iter result ~f:(fun grant ->
    print_s
      [%message (grant.proxy_cookie : string) (grant.token : string option)])
;;

let%expect_test "the page's code opens with the program's key, once" =
  let t = key () in
  print_endline (Paste_code.public_key t);
  show t vector;
  show t vector;
  [%expect
    {|
    B6N8vBQgk8i3VdwbEOhstCY3StFqqFPtC9_AsrhtHHw
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    (Error (Used "this sign-in already took a code: start it again"))
    |}]
;;

let%expect_test "a paste wrapped by the terminal, with spaces and newlines" =
  let wrapped =
    String.to_list vector
    |> List.chunks_of ~length:40
    |> List.map ~f:String.of_char_list
    |> String.concat ~sep:"\n  "
  in
  show (key ()) ("  " ^ wrapped ^ "\r\n");
  [%expect
    {|
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    |}]
;;

let%expect_test "refused: another sign-in, another key, changed, not a code" =
  let flip_at code i =
    String.mapi code ~f:(fun j c ->
      if i = j then if Char.equal c 'A' then 'B' else 'A' else c)
  in
  let tail = String.length vector - 1 in
  List.iter
    [ "wrong state", key ~state:"fedcba9876543210fedcba9876543210" (), vector
    ; "wrong key", key ~secret:(String.make 32 '\x07') (), vector
    ; "ephemeral key changed", key (), flip_at vector 10
    ; "nonce changed", key (), flip_at vector 50
    ; "ciphertext changed", key (), flip_at vector 80
    ; "tag changed", key (), flip_at vector (tail - 2)
    ; "truncated", key (), String.prefix vector 60
    ; "no prefix", key (), String.chop_prefix_exn vector ~prefix:"prigh1:"
    ; "another version", key (), "prigh2:" ^ String.drop_prefix vector 7
    ; "not base64url", key (), "prigh1:not*base64"
    ; "empty", key (), ""
    ]
    ~f:(fun (what, t, code) ->
      print_string (what ^ ": ");
      show t code;
      (* A refused code does not use up the sign-in. *)
      if not (String.equal what "wrong key" || String.equal what "wrong state")
      then show t vector);
  [%expect
    {|
    wrong state: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    wrong key: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    ephemeral key changed: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    nonce changed: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    ciphertext changed: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    tag changed: (Error
     (Not_for_this_signin
      "that code is not for this sign-in (from an earlier one, or changed on the way): paste the code the page shows for this link, or open the link again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    truncated: (Error
     (Not_a_code
      "that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    no prefix: (Error
     (Not_a_code
      "that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    another version: (Error
     (Not_a_code
      "that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    not base64url: (Error
     (Not_a_code
      "that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    empty: (Error
     (Not_a_code
      "that is not a sign-in code (it starts with prigh1:): copy it with the page's Copy button and paste it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    |}]
;;

let%expect_test "expired" =
  let t = key () in
  show t vector ~now:expires;
  show t vector ~now:(Time_ns.sub expires (Time_ns.Span.of_sec 1.));
  [%expect
    {|
    (Error (Expired "this sign-in expired: start it again"))
    (Ok ((proxy_cookie <opaque>) (token (<opaque>)) (user (alice))))
    ((grant.proxy_cookie proxy_session=s3cret) (grant.token (sekrit)))
    |}]
;;
