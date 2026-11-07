open! Core
open! Import

module Transport = struct
  type t =
    { send : string -> unit
    ; receive : unit -> string option
    ; kill : unit -> unit
    }
end

type t =
  { info : Computer_info.t
  ; start : unit -> Transport.t
  ; mutable running : Transport.t option
  ; mutex : Eio.Mutex.t
  ; mutable next_id : int
  }

let program () =
  match Sys.getenv "PRIGH_COMPUTER" with
  | Some p when not (String.is_empty p) -> p
  | _ -> "prigh-computer"
;;

let extra_args () =
  match Sys.getenv "PRIGH_COMPUTER_ARGS" with
  | None -> []
  | Some args ->
    String.split args ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let int_field json name =
  match field json name with
  | Some (`Number n) ->
    Option.map (Float.of_string_opt n) ~f:Float.iround_nearest_exn
  | _ -> None
;;

let point_field json name =
  match field json name with
  | Some p ->
    (match int_field p "x", int_field p "y" with
     | Some x, Some y -> Some (x, y)
     | _ -> None)
  | None -> None
;;

let not_found prog =
  sprintf
    "%s not found: install prigh-computer (the Nix package prigh-computer, or \
     `cargo build -p prigh-computer` in prigh's desktop/) and put it on PATH, \
     or point $PRIGH_COMPUTER at it"
    prog
;;

let probe ~env =
  let prog = program () in
  match
    Process.run_collect
      ~env
      ~timeout:(Time_ns.Span.of_sec 20.)
      ~prog
      ~args:(extra_args () @ [ "--probe" ])
      ()
  with
  | exception Eio.Io (Eio.Process.E (Executable_not_found _), _) ->
    Or_error.error_string (not_found prog)
  | exception exn ->
    Or_error.errorf "cannot run %s: %s" prog (Exn.to_string exn)
  | { exit; stdout; stderr } ->
    if Process.Exit.is_success exit
    then (
      match Json.parse (String.strip stdout) with
      | Ok json -> Computer_info.of_json json
      | Error _ ->
        Or_error.errorf
          "%s --probe printed no screen information: %S"
          prog
          stdout)
    else (
      match String.strip stderr with
      | "" ->
        Or_error.errorf
          "%s --probe failed (%s)"
          prog
          (Sexp.to_string (Process.Exit.sexp_of_t exit))
      | message -> Or_error.error_string message)
;;

let create info ~start =
  { info; start; running = None; mutex = Eio.Mutex.create (); next_id = 0 }
;;

let spawn ~env ~sw () =
  let prog = program () in
  let stdin_r, stdin_w = Eio_unix.pipe sw in
  let stdout_r, stdout_w = Eio_unix.pipe sw in
  match
    Eio.Process.spawn
      ~sw
      (Eio.Stdenv.process_mgr env)
      ~stdin:stdin_r
      ~stdout:stdout_w
      (prog :: extra_args ())
  with
  | exception Eio.Io (Eio.Process.E (Executable_not_found _), _) ->
    Eio.Flow.close stdin_w;
    Eio.Flow.close stdout_r;
    failwith (not_found prog)
  | child ->
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdout_w;
    let reader = Eio.Buf_read.of_flow stdout_r ~max_size:(256 * 1024 * 1024) in
    let killed = ref false in
    let kill () =
      if not !killed
      then (
        killed := true;
        (try Eio.Process.signal child Stdlib.Sys.sigkill with
         | _ -> ());
        (try Eio.Flow.close stdin_w with
         | _ -> ());
        Fiber.fork ~sw (fun () ->
          ignore (Eio.Process.await child : Eio.Process.exit_status);
          try Eio.Flow.close stdout_r with
          | _ -> ()))
    in
    { Transport.send = (fun line -> Eio.Flow.copy_string (line ^ "\n") stdin_w)
    ; receive =
        (fun () ->
          match Eio.Buf_read.line reader with
          | line -> Some line
          | exception (End_of_file | Eio.Io _) -> None)
    ; kill
    }
;;

let enable ~env ~sw =
  Or_error.map (probe ~env) ~f:(fun info -> create info ~start:(spawn ~env ~sw))
;;

let info t = t.info
let interrupted_prefix = "[interrupted]"

let interrupted (result : Tool_result.t) =
  result.is_error && String.is_prefix result.text ~prefix:interrupted_prefix
;;

let stop t =
  Option.iter t.running ~f:(fun transport -> transport.kill ());
  t.running <- None
;;

(* One request and its reply, starting the helper if needed. *)
let request t ~cancel json =
  let id = t.next_id in
  t.next_id <- id + 1;
  let line =
    Json.to_string
      (match json with
       | `Object fields -> `Object (("id", `Number (Int.to_string id)) :: fields)
       | json -> json)
  in
  let attempt () =
    let started () =
      let transport = t.start () in
      t.running <- Some transport;
      transport
    in
    let transport =
      match t.running with
      | Some transport -> transport
      | None -> started ()
    in
    (* A helper that died since the last action never saw this one: send it
       to a new one. *)
    let transport =
      match transport.send line with
      | () -> transport
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception _ ->
        stop t;
        let transport = started () in
        transport.send line;
        transport
    in
    let rec reply () =
      match transport.receive () with
      | None -> Error "prigh-computer exited"
      | Some line ->
        (match Json.parse line with
         | Ok json when Option.equal Int.equal (int_field json "id") (Some id)
           -> Ok json
         | Ok _ | Error _ -> reply ())
    in
    reply ()
  in
  match
    Cancellation.protect cancel ~f:(fun () ->
      match attempt () with
      | result -> result
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception Failure message -> Error message
      | exception exn -> Error (Exn.to_string exn))
  with
  | None ->
    stop t;
    Error "[cancelled]"
  | Some (Ok json) -> Ok json
  | Some (Error message) ->
    stop t;
    Error message
;;

let result_of_reply action reply =
  match field reply "ok" with
  | Some `True ->
    let screenshot =
      match field reply "screenshot" with
      | Some shot ->
        (match string_field shot "mime_type", string_field shot "data" with
         | Some mime_type, Some data ->
           Some
             ( { Image.mime_type; data }
             , Option.both (int_field shot "width") (int_field shot "height") )
         | _ -> None)
      | None -> None
    in
    let first =
      Computer_action.describe action
      ^
      match point_field reply "real" with
      | Some (x, y) -> sprintf " → (%d, %d) on the screen" x y
      | None -> ""
    in
    let lines =
      List.filter_opt
        [ Some first
        ; Option.filter (string_field reply "text") ~f:(Fn.non String.is_empty)
        ; Option.map (point_field reply "cursor") ~f:(fun (x, y) ->
            sprintf "cursor at (%d, %d)" x y)
        ; Option.map screenshot ~f:(fun (_, size) ->
            match size with
            | Some (w, h) -> sprintf "screenshot %d×%d" w h
            | None -> "screenshot")
        ]
    in
    Tool_result.ok
      ~images:(Option.to_list (Option.map screenshot ~f:fst))
      (String.concat ~sep:"\n" lines)
  | _ ->
    let error =
      Option.value (string_field reply "error") ~default:"prigh-computer failed"
    in
    (match field reply "interrupted" with
     | Some `True -> Tool_result.error (interrupted_prefix ^ " " ^ error)
     | _ -> Tool_result.error error)
;;

let run t ~cancel arguments =
  match Computer_action.of_json arguments with
  | Error e -> Tool_result.error (Error.to_string_hum e)
  | Ok action ->
    Eio.Mutex.use_ro t.mutex (fun () ->
      match
        request
          t
          ~cancel
          (`Object
              [ "op", `String "act"; "action", Computer_action.to_json action ])
      with
      | Ok reply -> result_of_reply action reply
      | Error "[cancelled]" -> Tool_result.error "[cancelled]"
      | Error message ->
        Tool_result.error
          (sprintf "%s: the next action starts it again" message))
;;
