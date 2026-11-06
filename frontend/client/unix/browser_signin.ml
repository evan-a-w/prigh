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

module Line = struct
  type t =
    | Url of
        { url : string
        ; port : int
        }
    | Granted of Grant.t
    | Failed of string
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
      (match string "url", string "proxy_cookie", string "error" with
       | Some url, _, _ ->
         Option.map (Json.int_field json "port" |> Or_error.ok) ~f:(fun port ->
           Url { url; port })
       | None, Some proxy_cookie, _ ->
         Some (Granted { proxy_cookie; token = string "token"; user = string "user" })
       | None, None, Some error -> Some (Failed error)
       | None, None, None -> None)
  ;;
end

let run ~backend ~connect ~on_url =
  match%bind
    Process.create
      ~prog:backend
      ~args:
        [ "browser-signin"; "-json"; "-no-browser"; "-program"; "tui"; "-connect"; connect ]
      ()
  with
  | Error e ->
    return (Error (sprintf "cannot run %s: %s" backend (Error.to_string_hum e)))
  | Ok process ->
    Writer.close (Process.stdin process) |> don't_wait_for;
    (* What it says on stderr would draw over the TUI. *)
    don't_wait_for (Reader.drain (Process.stderr process));
    let rec read result =
      match%bind Reader.read_line (Process.stdout process) with
      | `Eof -> return result
      | `Ok line ->
        (match Line.parse line with
         | Some (Url { url; port }) ->
           on_url ~url ~port;
           read result
         | Some (Granted grant) -> read (Some (Ok grant))
         | Some (Failed error) -> read (Some (Error error))
         | None -> read result)
    in
    let%bind result = read None in
    let%map exit = Process.wait process in
    (match result, exit with
     | Some result, _ -> result
     | None, Ok () -> Error "prigh browser-signin said nothing"
     | None, Error e ->
       Error
         (sprintf
            "prigh browser-signin failed (%s): is %s as new as prigh-tui?"
            (Core_unix.Exit_or_signal.to_string_hum (Error e))
            backend))
;;
