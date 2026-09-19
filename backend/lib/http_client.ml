open! Core
open! Import

module Response = struct
  type t =
    { status : int
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  let header t name =
    List.find_map t.headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) v)
  ;;
end

module Error = struct
  type t =
    | Connection_failed of string
    | Cancelled
    | Timed_out
  [@@deriving sexp_of]

  let to_string = function
    | Connection_failed s -> "connection failed: " ^ s
    | Cancelled -> "cancelled"
    | Timed_out -> "timed out"
  ;;
end

let tls_config =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     let authenticator =
       match Ca_certs.authenticator () with
       | Ok a -> a
       | Error (`Msg m) -> failwith ("no CA certificates: " ^ m)
     in
     match Tls.Config.client ~authenticator () with
     | Ok config -> config
     | Error (`Msg m) -> failwith ("bad TLS config: " ^ m))
;;

let https uri raw =
  let host =
    Option.map (Uri.host uri) ~f:(fun h ->
      Domain_name.host_exn (Domain_name.of_string_exn h))
  in
  Tls_eio.client_of_flow (force tls_config) ?host raw
;;

let read_body body ~on_chunk =
  let buf = Cstruct.create 65536 in
  try
    while true do
      let n = Eio.Flow.single_read body buf in
      on_chunk (Cstruct.to_string buf ~len:n)
    done
  with
  | End_of_file -> ()
;;

let post_stream
      ~(env : Env.t)
      ?(cancel = Cancellation.never)
      ?timeout
      ?(on_response = ignore)
      ~url
      ~headers
      ~body
      ~on_chunk
      ()
  =
  let request () =
    Switch.run
    @@ fun sw ->
    let client =
      Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env)
    in
    match
      Cohttp_eio.Client.post
        client
        ~sw
        ~headers:(Http.Header.of_list headers)
        ~body:(Cohttp_eio.Body.of_string body)
        (Uri.of_string url)
    with
    | exception
        (( Eio.Io _
         | Failure _
         | Core_unix.Unix_error _
         | Tls_eio.Tls_alert _
         | Tls_eio.Tls_failure _ ) as exn) ->
      Error (Error.Connection_failed (Exn.to_string exn))
    | resp, body ->
      let response =
        { Response.status = Http.Status.to_int (Http.Response.status resp)
        ; headers = Http.Header.to_list (Http.Response.headers resp)
        }
      in
      on_response response;
      read_body body ~on_chunk;
      Ok response
  in
  let with_timeout f =
    match timeout with
    | None -> f ()
    | Some span ->
      (match
         Eio.Time.with_timeout
           (Eio.Stdenv.clock env)
           (Time_ns.Span.to_sec span)
           (fun () -> Ok (f ()))
       with
       | Error `Timeout -> Error Error.Timed_out
       | Ok result -> result)
  in
  match Cancellation.protect cancel ~f:(fun () -> with_timeout request) with
  | None -> Error Error.Cancelled
  | Some result -> result
;;

let post ~env ?cancel ?timeout ~url ~headers ~body () =
  let buf = Buffer.create 1024 in
  Result.map
    (post_stream
       ~env
       ?cancel
       ?timeout
       ~url
       ~headers
       ~body
       ~on_chunk:(Buffer.add_string buf)
       ())
    ~f:(fun response -> response, Buffer.contents buf)
;;
