open! Core
open! Async

let rng = lazy (Mirage_crypto_rng_unix.use_default ())

let system_authenticator () =
  match Ca_certs.authenticator () with
  | Ok authenticator -> Ok authenticator
  | Error (`Msg m) ->
    Or_error.errorf
      "no CA certificates to check the server's TLS certificate with: %s \
       (set $SSL_CERT_FILE to a PEM bundle)"
      m
;;

let describe_failure ~host (failure : Tls.Engine.failure) =
  match failure with
  | `Error (`AuthenticationFailure error) ->
    sprintf
      "the TLS certificate of %s is not trusted: %s. prigh checks it against \
       the system's CAs ($SSL_CERT_FILE overrides them): use a certificate \
       they trust (e.g. Let's Encrypt), or an SSH tunnel to plain HOST:PORT"
      host
      (Format.asprintf "%a" X509.Validation.pp_validation_error error)
  | failure ->
    sprintf
      "TLS with %s failed: %s"
      host
      (Tls.Engine.string_of_failure failure)
;;

let run ~host ~(raw : Channel.t) ~state ~first =
  let state = ref state in
  let closed = ref false in
  let handshake = Ivar.create () in
  let app_r, app_w = Pipe.create () in
  let stop () =
    if not !closed
    then (
      closed := true;
      Pipe.close app_w;
      raw.close ())
  in
  let fail message =
    Ivar.fill_if_empty handshake (Error (Error.of_string message));
    stop ()
  in
  Option.iter first ~f:raw.write;
  let buf = Bytes.create 16384 in
  let rec loop () =
    match%bind Monitor.try_with (fun () -> Reader.read raw.reader buf) with
    | Error _ | Ok `Eof ->
      fail
        (sprintf
           "%s closed the connection during the TLS handshake (is it serving \
            TLS? for plain HTTP use ws:// or http://)"
           host);
      return ()
    | Ok (`Ok n) ->
      (match
         Tls.Engine.handle_tls !state (Bytes.To_string.sub buf ~pos:0 ~len:n)
       with
       | Error (failure, `Response response) ->
         raw.write response;
         fail (describe_failure ~host failure);
         return ()
       | Ok (next, eof, `Response response, `Data data) ->
         state := next;
         Option.iter response ~f:raw.write;
         if not (Tls.Engine.handshake_in_progress next)
         then Ivar.fill_if_empty handshake (Ok ());
         Option.iter data ~f:(fun data ->
           if not (Pipe.is_closed app_w)
           then Pipe.write_without_pushback app_w data);
         (match eof with
          | Some `Eof ->
            fail
              (sprintf "%s closed the connection during the TLS handshake" host);
            return ()
          | None -> loop ()))
  in
  don't_wait_for (loop ());
  match%bind Ivar.read handshake with
  | Error _ as e -> return e
  | Ok () ->
    let%map reader = Reader.of_pipe (Info.of_string ("tls " ^ host)) app_r in
    Ok
      { Channel.reader
      ; write =
          (fun data ->
            if not !closed
            then (
              match Tls.Engine.send_application_data !state [ data ] with
              | Some (next, out) ->
                state := next;
                raw.write out
              | None -> ()))
      ; close =
          (fun () ->
            if not !closed
            then (
              let next, out = Tls.Engine.send_close_notify !state in
              state := next;
              raw.write out;
              stop ());
            don't_wait_for (Reader.close reader))
      }
;;

let client ?authenticator ~host raw =
  force rng;
  match
    match authenticator with
    | Some a -> Ok a
    | None -> system_authenticator ()
  with
  | Error _ as e ->
    raw.Channel.close ();
    return e
  | Ok authenticator ->
    let peer_name =
      Result.ok (Result.bind (Domain_name.of_string host) ~f:Domain_name.host)
    in
    let ip = Result.ok (Ipaddr.of_string host) in
    let peer_name = if Option.is_some ip then None else peer_name in
    (match Tls.Config.client ~authenticator ?peer_name ?ip () with
     | Error (`Msg m) ->
       raw.close ();
       return (Or_error.errorf "bad TLS configuration: %s" m)
     | Ok config ->
       let state, hello = Tls.Engine.client config in
       run ~host ~raw ~state ~first:(Some hello))
;;

let server config raw =
  force rng;
  run ~host:"the client" ~raw ~state:(Tls.Engine.server config) ~first:None
;;
