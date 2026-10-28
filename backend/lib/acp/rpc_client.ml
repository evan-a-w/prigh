open! Core
open! Import

type t =
  { request : string -> (string * Json.t) list -> Json.t Or_error.t
  ; close : unit -> unit
  }

let call t method_ params = t.request method_ params
let close t = t.close ()

(* A [{type: "response", ok, result | error}]. *)
let of_response response =
  match Json.member "ok" response with
  | Some `True ->
    Ok (Option.value (Json.member "result" response) ~default:`Null)
  | _ ->
    Or_error.error_string
      (Option.value
         (Option.bind (Json.member "error" response) ~f:Json.string)
         ~default:"request failed")
;;

let request_json ~id method_ params =
  `Object [ "id", id; "method", `String method_; "params", `Object params ]
;;

module Connector = struct
  type nonrec t =
    on_event:(Json.t -> unit) -> on_close:(string -> unit) -> t Or_error.t

  let in_process server ~on_event ~on_close =
    let closed = ref false in
    let client =
      Rpc_server.connect
        server
        ~close:(fun reason ->
          if not !closed
          then (
            closed := true;
            on_close reason))
        ~send:on_event
    in
    let seq = ref 0 in
    Ok
      { request =
          (fun method_ params ->
            if !closed
            then Or_error.error_string "the connection to prigh is closed"
            else (
              incr seq;
              of_response
                (Rpc_server.handle
                   server
                   client
                   (request_json
                      ~id:(`String (sprintf "c%d" !seq))
                      method_
                      params))))
      ; close =
          (fun () ->
            if not !closed
            then (
              closed := true;
              Rpc_server.disconnect server client))
      }
  ;;

  let tcp ~env ~sw ~host ~port ~on_event ~on_close =
    let net = Eio.Stdenv.net env in
    match Eio.Net.getaddrinfo_stream net host ~service:(Int.to_string port) with
    | exception exn ->
      Or_error.errorf "cannot resolve %s: %s" host (Net_error.to_string_hum exn)
    | [] -> Or_error.errorf "cannot resolve %s" host
    | addr :: _ ->
      (match Eio.Net.connect ~sw net addr with
       | exception exn ->
         Or_error.errorf
           "cannot connect to %s:%d: %s"
           host
           port
           (Net_error.to_string_hum exn)
       | flow ->
         let pending = Int.Table.create () in
         let seq = ref 0 in
         let closed = ref None in
         let lines = Eio.Stream.create Int.max_value in
         let finish reason ~notify =
           if Option.is_none !closed
           then (
             closed := Some reason;
             Eio.Stream.add lines None;
             Hashtbl.iter pending ~f:(fun resolver ->
               Promise.resolve resolver (Or_error.error_string reason));
             Hashtbl.clear pending;
             (try Eio.Flow.shutdown flow `All with
              | _ -> ());
             if notify then on_close reason)
         in
         Fiber.fork_daemon ~sw (fun () ->
           let rec loop () =
             match Eio.Stream.take lines with
             | None -> `Stop_daemon
             | Some line ->
               (match Eio.Flow.copy_string (line ^ "\n") flow with
                | () -> loop ()
                | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
                | exception exn ->
                  finish
                    (sprintf "writing to prigh failed: %s" (Exn.to_string exn))
                    ~notify:true;
                  `Stop_daemon)
           in
           loop ());
         Fiber.fork_daemon ~sw (fun () ->
           let reader =
             Eio.Buf_read.of_flow flow ~max_size:(256 * 1024 * 1024)
           in
           let rec loop () =
             match Eio.Buf_read.line reader with
             | exception (End_of_file | Eio.Io _) ->
               finish
                 (sprintf "prigh at %s:%d closed the connection" host port)
                 ~notify:true
             | line ->
               (match Json.parse line with
                | Error _ -> ()
                | Ok json ->
                  (match Json.member "type" json with
                   | Some (`String "response") ->
                     Option.iter
                       (Option.bind (Json.member "id" json) ~f:Json.int)
                       ~f:(fun id ->
                         Option.iter
                           (Hashtbl.find_and_remove pending id)
                           ~f:(fun resolver ->
                             Promise.resolve resolver (of_response json)))
                   | Some (`String "event") -> on_event json
                   | _ -> ()));
               loop ()
           in
           loop ();
           `Stop_daemon);
         Ok
           { request =
               (fun method_ params ->
                 match !closed with
                 | Some reason -> Or_error.error_string reason
                 | None ->
                   incr seq;
                   let id = !seq in
                   let promise, resolver = Promise.create () in
                   Hashtbl.set pending ~key:id ~data:resolver;
                   Eio.Stream.add
                     lines
                     (Some
                        (Json.to_string
                           (request_json
                              ~id:(`Number (Int.to_string id))
                              method_
                              params)));
                   Promise.await promise)
           ; close = (fun () -> finish "closed" ~notify:false)
           })
  ;;
end
