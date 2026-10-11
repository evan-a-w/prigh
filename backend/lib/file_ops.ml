open! Core
open! Import

let max_upload = 2 * 1024 * 1024 * 1024
let max_preview = 256 * 1024
let max_image_preview = 20 * 1000 * 1000
let uploads_dir = ".prigh/uploads"

(* How many chunks may be on their way to or from a host. *)
let window = 8

let host_name agent =
  let state = Agent.state agent in
  List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host)
  |> Option.value_map ~default:state.active_host ~f:(fun h -> h.Agent.Host.name)
;;

let error_of agent ~name (result : Tool.Result.t) =
  if String.is_prefix result.text ~prefix:(sprintf "unknown host tool %S" name)
  then
    Error.createf
      "the file browser needs a newer prigh on tool host %s: update prigh \
       there and restart its TUI or tool-host"
      (host_name agent)
  else Error.of_string result.text
;;

let reply agent ~name (result : Tool.Result.t) =
  if result.is_error
  then Error (error_of agent ~name result)
  else Json.parse result.text
;;

let call agent name arguments =
  reply agent ~name (Agent.host_call agent ~name (`Object arguments))
;;

let number n = `Number (Int.to_string n)

let int_member json key =
  match Json.member key json with
  | Some (`Number n) -> Int.of_string_opt n
  | _ -> None
;;

let preview agent ~path ~image_url =
  let open Or_error.Let_syntax in
  let%bind read =
    call
      agent
      Host_files.read_op
      [ "path", `String path; "offset", number 0; "length", number max_preview ]
  in
  let%bind data =
    match Json.member "data" read with
    | Some (`String data) ->
      Result.map_error (Base64.decode data) ~f:(fun (`Msg m) ->
        Error.of_string ("bad data from the host: " ^ m))
    | _ -> Or_error.error_string "bad reply from the host"
  in
  let size =
    Option.value (int_member read "size") ~default:(String.length data)
  in
  let eof =
    match Json.member "eof" read with
    | Some `True -> true
    | _ -> false
  in
  let common kind =
    [ "path", `String path
    ; "name", `String (Filename.basename path)
    ; "size", number size
    ; "mtime", Option.value (Json.member "mtime" read) ~default:`Null
    ; "kind", `String kind
    ]
  in
  return
    (`Object
        (match Image.sniff data with
         | Some mime when size > max_image_preview ->
           common "image" @ [ "mime", `String mime; "too_large", `True ]
         | Some mime ->
           common "image"
           @ [ "mime", `String mime; "url", `String (image_url ~path ~mime) ]
         | None when Tool_read.looks_binary data -> common "binary"
         | None ->
           let text =
             if eof then data else fst (Utf8.split_incomplete_suffix data)
           in
           common "text"
           @ [ "text", `String (Utf8.sanitize text)
             ; ("truncated", if eof then `False else `True)
             ]))
;;

(* Runs [name] in a fiber, its output and its result arriving on one stream
   in order. *)
let start agent ~sw ~feed ~cancel ~name arguments =
  let events = Eio.Stream.create Int.max_value in
  Fiber.fork ~sw (fun () ->
    let result =
      Agent.host_call
        agent
        ~stdin:feed
        ~cancel
        ~on_output:(fun chunk -> Eio.Stream.add events (`Output chunk))
        ~name
        (`Object arguments)
    in
    Eio.Stream.add events (`Done result));
  events
;;

let rec finish events =
  match Eio.Stream.take events with
  | `Done result -> result
  | `Output _ -> finish events
;;

let interrupted = Or_error.error_string "the transfer was interrupted"

let download agent ~path ~archive ~write =
  let name = Host_files.download_op in
  let feed = Stdin_feed.create () in
  let cancel = Cancellation.create () in
  Switch.run
  @@ fun sw ->
  let events =
    start
      agent
      ~sw
      ~feed
      ~cancel
      ~name
      [ "path", `String path
      ; ("archive", if archive then `True else `False)
      ; "window", number window
      ]
  in
  let stop () =
    Cancellation.cancel cancel;
    ignore (finish events : Tool.Result.t);
    interrupted
  in
  let rec loop () =
    match Eio.Stream.take events with
    | `Done result ->
      Or_error.map (reply agent ~name result) ~f:(fun json ->
        Option.value (int_member json "bytes") ~default:0)
    | `Output chunk ->
      (match Base64.decode chunk with
       | Error _ -> stop ()
       | Ok raw ->
         (match write raw with
          | () ->
            ignore (Stdin_feed.write feed "+" ~close:false : unit Or_error.t);
            loop ()
          | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
          | exception _ -> stop ()))
  in
  loop ()
;;

module Conflict = struct
  type t =
    | Fail
    | Overwrite
    | Rename
  [@@deriving sexp_of, equal]

  let to_string = function
    | Fail -> "fail"
    | Overwrite -> "overwrite"
    | Rename -> "rename"
  ;;
end

let upload agent ~path ~conflict ~size ?(make_dir = false) ~read () =
  let open Or_error.Let_syntax in
  let%bind () =
    if size > max_upload
    then
      Or_error.errorf
        "%s is %d bytes, more than the %d bytes an upload may have: copy it to \
         the host another way (scp, git)"
        (Filename.basename path)
        size
        max_upload
    else Ok ()
  in
  let%bind (_ : Json.t) =
    if make_dir
    then
      call
        agent
        Host_files.mkdir_op
        [ "path", `String (Filename.dirname path)
        ; "parents", `True
        ; "ignore_all", `True
        ]
    else Ok `Null
  in
  let name = Host_files.upload_op in
  let feed = Stdin_feed.create () in
  let cancel = Cancellation.create () in
  Switch.run
  @@ fun sw ->
  let events =
    start
      agent
      ~sw
      ~feed
      ~cancel
      ~name
      [ "path", `String path
      ; "conflict", `String (Conflict.to_string conflict)
      ; "size", number size
      ]
  in
  let in_flight = ref 0 in
  let acked chunk =
    in_flight := !in_flight - String.count chunk ~f:(Char.equal '+')
  in
  (* Waits for room in the window; [`Done] when the host finished early. *)
  let rec room () =
    if !in_flight < window
    then `Room
    else (
      match Eio.Stream.take events with
      | `Output chunk ->
        acked chunk;
        room ()
      | `Done result -> `Done result)
  in
  let rec loop () =
    match room () with
    | `Done result -> reply agent ~name result
    | `Room ->
      (match read Host_files.chunk_size with
       | "" ->
         ignore (Stdin_feed.write feed "" ~close:true : unit Or_error.t);
         reply agent ~name (finish events)
       | data ->
         (match
            Stdin_feed.write feed (Base64.encode_string data) ~close:false
          with
          | Ok () ->
            incr in_flight;
            loop ()
          | Error _ ->
            (* The host finished (an error): its result says why. *)
            reply agent ~name (finish events))
       | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
       | exception _ ->
         Cancellation.cancel cancel;
         ignore (finish events : Tool.Result.t);
         interrupted)
  in
  loop ()
;;
