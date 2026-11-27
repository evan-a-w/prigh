open! Core
open! Import

let labels = [ "title:"; "name:"; "session title:"; "subagent name:" ]

let drop_label line =
  let lower = String.lowercase line in
  match
    List.find labels ~f:(fun label -> String.is_prefix lower ~prefix:label)
  with
  | None -> line
  | Some label -> String.strip (String.drop_prefix line (String.length label))
;;

let is_wrapper c =
  match c with
  | '"' | '\'' | '`' | '*' | '_' | '#' | '>' -> true
  | _ -> false
;;

let is_trailing c =
  match c with
  | '.' | '!' | '?' | ':' | ';' | ',' -> true
  | c -> is_wrapper c || Char.is_whitespace c
;;

let shorten line ~max_length =
  if String.length line <= max_length
  then line
  else (
    let cut = String.prefix line (max_length - 1) in
    let cut =
      match String.rindex cut ' ' with
      | Some i when i >= max_length / 2 -> String.prefix cut i
      | Some _ | None -> cut
    in
    String.rstrip cut ~drop:is_trailing ^ "…")
;;

let clean ?(max_length = 60) text =
  let line =
    String.split_lines text
    |> List.map ~f:String.strip
    |> List.find ~f:(fun l ->
      not (String.is_empty (String.strip l ~drop:is_wrapper)))
    |> Option.value ~default:""
  in
  let unwrap =
    String.strip ~drop:(fun c -> is_wrapper c || Char.is_whitespace c)
  in
  let line = unwrap line |> drop_label |> unwrap in
  let line = String.rstrip line ~drop:is_trailing in
  shorten (String.capitalize line) ~max_length
;;

let ask_with ~complete ?(cancel = Cancellation.never) ~what ~system user =
  match complete ~system ~user ~max_tokens:60 ~cancel with
  | Error (`Error e) -> Or_error.errorf "%s failed: %s" what e
  | Error `Aborted -> Or_error.errorf "%s aborted" what
  | Ok text ->
    (match clean text with
     | "" -> Or_error.errorf "%s failed: empty reply" what
     | title -> Ok title)
;;

let ask
      ~(provider : Provider.t)
      ~model
      ?(cancel = Cancellation.never)
      ~what
      ~system
      user
  =
  let complete ~system ~user ~max_tokens ~cancel =
    let request =
      { Provider.Request.model
      ; system = Some system
      ; messages = [ Message.user user ]
      ; tools = []
      ; thinking = Off
      ; max_tokens = Some max_tokens
      ; cache_key = None
      ; account = None
      }
    in
    let reply = provider.stream request ~cancel ~on_event:ignore in
    match reply.stop_reason with
    | Error e -> Error (`Error e)
    | Aborted -> Error `Aborted
    | End_turn | Tool_use | Length -> Ok (Message.Assistant.text reply)
  in
  ask_with ~complete ~cancel ~what ~system user
;;
