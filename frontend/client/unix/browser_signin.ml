open! Core
open! Async
module Json = Prigh_protocol.Json

module Grant = struct
  type t =
    { proxy_cookie : string
    ; token : string option
    ; user : string option
    }
  [@@deriving sexp_of, equal]
end

module Code_input = struct
  type t =
    { send : string -> unit
    ; cancel : unit -> unit
    }

  let create ~send ~cancel = { send; cancel }
  let send t code = t.send code
  let cancel t = t.cancel ()
end

let paste_wanted ~getenv =
  let set name = Option.exists (getenv name) ~f:(Fn.non String.is_empty) in
  match Option.map (getenv "PRIGH_SIGNIN_PASTE") ~f:String.strip with
  | Some "1" -> true
  | Some "0" -> false
  | Some _ | None -> set "SSH_CONNECTION" || set "SSH_TTY"
;;

module Line = struct
  type t =
    | Url of
        { url : string
        ; port : int
        ; paste : bool
        }
    | Granted of Grant.t
    | Failed of string
    | Paste_refused of string
  [@@deriving sexp_of]

  let parse line =
    match Json.parse line with
    | Error _ -> None
    | Ok json ->
      let string name =
        match Json.field json name with
        | Some (`String s) when not (String.is_empty s) -> Some s
        | _ -> None
      in
      (match
         ( string "url"
         , string "proxy_cookie"
         , string "error"
         , string "paste_error" )
       with
       | Some url, _, _, _ ->
         Option.map
           (Json.int_field json "port" |> Or_error.ok)
           ~f:(fun port ->
             Url
               { url
               ; port
               ; paste =
                   (match Json.field json "paste" with
                    | Some `True -> true
                    | _ -> false)
               })
       | None, Some proxy_cookie, _, _ ->
         Some
           (Granted
              { proxy_cookie; token = string "token"; user = string "user" })
       | None, None, Some error, _ -> Some (Failed error)
       | None, None, None, Some error -> Some (Paste_refused error)
       | None, None, None, None -> None)
  ;;
end

let run ~backend ~connect ~paste ~on_url ~on_paste_refused =
  match%bind
    Process.create
      ~prog:backend
      ~args:
        ([ "browser-signin"
         ; "-json"
         ; "-no-browser"
         ; "-program"
         ; "tui"
         ; "-connect"
         ; connect
         ]
         @ if paste then [ "-paste" ] else [])
      ()
  with
  | Error e ->
    return (Error (sprintf "cannot run %s: %s" backend (Error.to_string_hum e)))
  | Ok process ->
    let stdin = Process.stdin process in
    if not paste then Writer.close stdin |> don't_wait_for;
    let cancelled = ref false in
    let code_input =
      Code_input.create
        ~send:(fun code ->
          if not (Writer.is_closed stdin)
          then (
            Writer.write_line stdin (String.strip code);
            don't_wait_for (Writer.flushed stdin)))
        ~cancel:(fun () ->
          if not !cancelled
          then (
            cancelled := true;
            Signal_unix.send_i Signal.term (`Pid (Process.pid process))))
    in
    (* What it says on stderr would draw over the TUI. *)
    don't_wait_for (Reader.drain (Process.stderr process));
    let rec read result =
      match%bind Reader.read_line (Process.stdout process) with
      | `Eof -> return result
      | `Ok line ->
        (match Line.parse line with
         | Some (Url { url; port; paste }) ->
           on_url ~url ~port ~paste:(Option.some_if paste code_input);
           read result
         | Some (Paste_refused error) ->
           on_paste_refused error;
           read result
         | Some (Granted grant) -> read (Some (Ok grant))
         | Some (Failed error) -> read (Some (Error error))
         | None -> read result)
    in
    let%bind result = read None in
    let%map exit = Process.wait process in
    if paste then don't_wait_for (Writer.close stdin);
    (match result, exit with
     | Some result, _ -> result
     | None, _ when !cancelled -> Error "cancelled"
     | None, Ok () -> Error "prigh browser-signin said nothing"
     | None, Error e ->
       Error
         (sprintf
            "prigh browser-signin failed (%s): is %s as new as prigh-tui?"
            (Core_unix.Exit_or_signal.to_string_hum (Error e))
            backend))
;;
