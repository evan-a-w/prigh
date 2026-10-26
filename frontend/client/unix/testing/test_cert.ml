open! Core

type t =
  { authenticator : X509.Authenticator.t
  ; server : Tls.Config.server
  }

let ok_exn = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m
;;

let name cn =
  [ X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)))
  ]
;;

let validity () =
  let now = Ptime_clock.now () in
  let day = Ptime.Span.of_int_s 86_400 in
  ( Option.value_exn (Ptime.sub_span now day)
  , Option.value_exn (Ptime.add_span now day) )
;;

let time () = Some (Ptime_clock.now ())

let ca cn =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate `P256 in
  let csr = X509.Signing_request.create (name cn) key |> ok_exn in
  let valid_from, valid_until = validity () in
  let extensions =
    X509.Extension.(
      singleton Basic_constraints (true, (true, None))
      |> add Key_usage (true, [ `Key_cert_sign ]))
  in
  let cert =
    X509.Signing_request.sign
      csr
      ~valid_from
      ~valid_until
      ~extensions
      key
      (name cn)
    |> Result.map_error ~f:(fun e ->
      `Msg (Format.asprintf "%a" X509.Validation.pp_signature_error e))
    |> ok_exn
  in
  key, cert
;;

let create () =
  let ca_key, ca_cert = ca "prigh test CA" in
  let key = X509.Private_key.generate `P256 in
  let csr = X509.Signing_request.create (name "localhost") key |> ok_exn in
  let valid_from, valid_until = validity () in
  let extensions =
    X509.Extension.(
      singleton
        Subject_alt_name
        ( false
        , X509.General_name.(
            singleton DNS [ "localhost" ]
            |> add IP [ Ipaddr.V4.to_octets Ipaddr.V4.localhost ]) )
      |> add Ext_key_usage (false, [ `Server_auth ])
      |> add Basic_constraints (true, (false, None)))
  in
  let cert =
    X509.Signing_request.sign
      csr
      ~valid_from
      ~valid_until
      ~extensions
      ca_key
      (X509.Certificate.subject ca_cert)
    |> Result.map_error ~f:(fun e ->
      `Msg (Format.asprintf "%a" X509.Validation.pp_signature_error e))
    |> ok_exn
  in
  { authenticator = X509.Authenticator.chain_of_trust ~time [ ca_cert ]
  ; server =
      Tls.Config.server ~certificates:(`Single ([ cert; ca_cert ], key)) ()
      |> ok_exn
  }
;;

let stranger () =
  let _, cert = ca "someone else's CA" in
  X509.Authenticator.chain_of_trust ~time [ cert ]
;;
