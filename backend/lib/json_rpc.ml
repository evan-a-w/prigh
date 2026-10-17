open! Core
open! Import

module Error = struct
  type t =
    { code : int
    ; message : string
    }
  [@@deriving sexp_of]

  let parse_error message = { code = -32700; message }
  let invalid_request message = { code = -32600; message }
  let method_not_found message = { code = -32601; message }
  let invalid_params message = { code = -32602; message }
  let internal message = { code = -32603; message }
  let auth_required message = { code = -32000; message }
  let resource_not_found message = { code = -32002; message }
  let request_cancelled message = { code = -32800; message }

  let to_json t =
    `Object
      [ "code", `Number (Int.to_string t.code); "message", `String t.message ]
  ;;

  let of_json json =
    { code =
        Option.value (Option.bind (Json.member "code" json) ~f:Json.int) ~default:0
    ; message =
        Option.value
          (Option.bind (Json.member "message" json) ~f:Json.string)
          ~default:"error"
    }
  ;;
end

module Reply = struct
  type t =
    | Result of Json.t
    | Result_then of Json.t * (unit -> unit)
    | Error of Error.t
end

type t =
  { sw : Switch.t
  ; out : Json.t Eio.Stream.t
  ; pending : (Json.t, Error.t) Result.t Promise.u Int.Table.t
  ; mutable next_id : int
  ; mutable closed : bool
  }

let create ~sw ~write_line =
  let t =
    { sw
    ; out = Eio.Stream.create Int.max_value
    ; pending = Int.Table.create ()
    ; next_id = 0
    ; closed = false
    }
  in
  Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      write_line (Json.to_string (Eio.Stream.take t.out));
      loop ()
    in
    loop ());
  t
;;

let send t fields = Eio.Stream.add t.out (`Object (("jsonrpc", `String "2.0") :: fields))

let respond t ~id = function
  | Ok result -> send t [ "id", id; "result", result ]
  | Error error -> send t [ "id", id; "error", Error.to_json error ]
;;

let notify t method_ params =
  send t [ "method", `String method_; "params", params ]
;;

let cancel_request t id = notify t "$/cancel_request" (`Object [ "requestId", id ])

let request ?(on_id = ignore) t method_ params =
  if t.closed
  then Error (Error.internal "the client has disconnected")
  else (
    let id = t.next_id in
    t.next_id <- id + 1;
    let promise, resolver = Promise.create () in
    Hashtbl.set t.pending ~key:id ~data:resolver;
    let id_json = `Number (Int.to_string id) in
    on_id id_json;
    send t [ "id", id_json; "method", `String method_; "params", params ];
    Promise.await promise)
;;

let handle_request t ~id ~method_ ~params ~on_request =
  match on_request ~method_ ~params with
  | Reply.Result result -> respond t ~id (Ok result)
  | Result_then (result, then_) ->
    respond t ~id (Ok result);
    then_ ()
  | Error error -> respond t ~id (Error error)
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    eprintf "prigh acp: %s failed: %s\n%!" method_ (Exn.to_string exn);
    respond t ~id (Error (Error.internal (Exn.to_string exn)))
;;

let serve t ~read_line ~on_request ~on_notification =
  let rec loop () =
    match read_line () with
    | None -> ()
    | Some line when String.is_empty (String.strip line) -> loop ()
    | Some line ->
      (match Json.parse line with
       | Error e ->
         respond
           t
           ~id:`Null
           (Error (Error.parse_error ("invalid JSON: " ^ Core.Error.to_string_hum e)))
       | Ok json ->
         let id = Json.member "id" json in
         let method_ = Option.bind (Json.member "method" json) ~f:Json.string in
         let params = Option.value (Json.member "params" json) ~default:(`Object []) in
         (match method_, id with
          | Some method_, Some id ->
            Fiber.fork ~sw:t.sw (fun () ->
              handle_request t ~id ~method_ ~params ~on_request)
          | Some method_, None ->
            Fiber.fork ~sw:t.sw (fun () ->
              match on_notification ~method_ ~params with
              | () -> ()
              | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
              | exception exn ->
                eprintf "prigh acp: %s failed: %s\n%!" method_ (Exn.to_string exn))
          | None, Some id ->
            (match Option.bind (Some id) ~f:Json.int with
             | Some n ->
               Option.iter (Hashtbl.find_and_remove t.pending n) ~f:(fun resolver ->
                 Promise.resolve
                   resolver
                   (match Json.member "error" json with
                    | Some (`Object _ as error) -> Error (Error.of_json error)
                    | _ ->
                      Ok (Option.value (Json.member "result" json) ~default:`Null)))
             | None -> ())
          | None, None ->
            respond
              t
              ~id:`Null
              (Error (Error.invalid_request "a message needs a method or an id"))));
      loop ()
  in
  loop ();
  t.closed <- true;
  Hashtbl.iter t.pending ~f:(fun resolver ->
    Promise.resolve resolver (Error (Error.internal "the client has disconnected")));
  Hashtbl.clear t.pending
;;
