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
    |> List.find ~f:(fun l -> not (String.is_empty (String.strip l ~drop:is_wrapper)))
    |> Option.value ~default:""
  in
  let line = String.strip line ~drop:is_wrapper |> drop_label in
  let line = String.strip line ~drop:is_wrapper in
  let line = String.rstrip line ~drop:is_trailing in
  shorten (String.capitalize line) ~max_length
;;

let ask ~(provider : Provider.t) ~model ?(cancel = Cancellation.never) ~what ~system user =
  let request =
    { Provider.Request.model
    ; system = Some system
    ; messages = [ Message.user user ]
    ; tools = []
    ; thinking = Off
    ; max_tokens = Some 60
    ; cache_key = None
    }
  in
  let reply = provider.stream request ~cancel ~on_event:ignore in
  match reply.stop_reason with
  | Error e -> Or_error.errorf "%s failed: %s" what e
  | Aborted -> Or_error.errorf "%s aborted" what
  | End_turn | Tool_use | Length ->
    (match clean (Message.Assistant.text reply) with
     | "" -> Or_error.errorf "%s failed: empty reply" what
     | title -> Ok title)
;;
