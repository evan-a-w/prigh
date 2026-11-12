open! Core
open! Import

let escaped text =
  let buf = Buffer.create (String.length text) in
  String.iter text ~f:(fun c ->
    match c with
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | c when Char.to_int c < 0x20 || Char.to_int c = 0x7f ->
      Buffer.add_string buf (sprintf "\\x%02x" (Char.to_int c))
    | c -> Buffer.add_char buf c);
  Buffer.contents buf
;;

let quoted ?(width = 40) text =
  "\"" ^ Text_width.truncate (escaped text) ~width ^ "\""
;;

let closes_stdin args =
  Option.value (Tool_args.bool args "close") ~default:false
;;

let job_input args =
  let input = Option.value (Tool_args.string args "input") ~default:"" in
  let input =
    if String.is_empty input && closes_stdin args then None else Some input
  in
  match Tool_args.string args "id", input with
  | None, None -> None
  | Some id, None -> Some id
  | None, Some input -> Some (quoted input)
  | Some id, Some input -> Some (id ^ " " ^ quoted input)
;;

let worktree args =
  let str = Tool_args.string args in
  match str "action" with
  | None -> None
  | Some action ->
    let target =
      match action with
      | "create" ->
        Option.map (str "branch") ~f:(fun branch ->
          match str "base" with
          | Some base -> sprintf "%s from %s" branch base
          | None -> branch)
      | "remove" ->
        Option.map (str "path") ~f:(fun path ->
          if Option.value (Tool_args.bool args "force") ~default:false
          then path ^ " (force)"
          else path)
      | _ -> None
    in
    Some (String.concat ~sep:" " (action :: Option.to_list target))
;;

let short_id id = String.prefix id 8

let sessions args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    String.concat
      ~sep:" "
      ((action :: Option.to_list (Option.map (str "query") ~f:quoted))
       @ Option.to_list
           (Option.map (str "session") ~f:(fun id ->
              if String.equal action "search"
              then "in " ^ short_id id
              else short_id id))
       @ List.filter_map [ "from"; "before" ] ~f:(fun key ->
         Option.map (Tool_args.int args key) ~f:(sprintf "%s %d" key))))
;;

let ask_session args =
  match
    ( Option.map (Tool_args.string args "session") ~f:short_id
    , Option.map (Tool_args.string args "question") ~f:(quoted ~width:60) )
  with
  | None, None -> None
  | session, question ->
    Some (String.concat ~sep:" " (List.filter_opt [ session; question ]))
;;

let answer_session args =
  match
    ( Tool_args.string args "id"
    , Option.map (Tool_args.string args "answer") ~f:(quoted ~width:60) )
  with
  | None, None -> None
  | id, answer -> Some (String.concat ~sep:" " (List.filter_opt [ id; answer ]))
;;

let pull_request args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    let target =
      match action with
      | "list" -> Option.to_list (str "state")
      | "create" ->
        Option.to_list (Option.map (str "title") ~f:quoted)
        @
        if Option.value (Tool_args.bool args "draft") ~default:false
        then [ "(draft)" ]
        else []
      | _ ->
        Option.to_list
          (Option.map (Tool_args.int args "number") ~f:(sprintf "#%d"))
    in
    String.concat ~sep:" " (action :: target))
;;

let issue args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    let target =
      match action with
      | "list" ->
        Option.to_list (str "state")
        @ Option.to_list (Option.map (str "search") ~f:quoted)
      | "create" -> Option.to_list (Option.map (str "title") ~f:quoted)
      | _ ->
        Option.to_list
          (Option.map (Tool_args.int args "number") ~f:(sprintf "#%d"))
    in
    String.concat ~sep:" " (action :: target))
;;

let repo args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    let target =
      match action with
      | "runs" -> Option.to_list (str "branch")
      | "run" | "rerun" ->
        Option.to_list
          (Option.map (Tool_args.int args "run") ~f:(sprintf "run %d"))
      | "logs" ->
        Option.to_list
          (Option.map (Tool_args.int args "job") ~f:(sprintf "job %d"))
      | _ -> []
    in
    String.concat ~sep:" " (action :: target))
;;

let memory args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    let target =
      match action with
      | "save" ->
        (match str "title", str "text" with
         | Some title, _ -> [ title ]
         | None, Some text -> [ quoted text ]
         | None, None -> [])
      | "recall" -> Option.to_list (Option.map (str "query") ~f:quoted)
      | "forget" -> Option.to_list (str "id")
      | _ -> []
    in
    String.concat ~sep:" " (action :: target))
;;

let custom_instructions args =
  let str = Tool_args.string args in
  Option.map (str "action") ~f:(fun action ->
    let about =
      match str "title", str "text" with
      | Some title, _ -> [ title ]
      | None, Some text -> [ quoted text ]
      | None, None -> []
    in
    let target =
      match action with
      | "add" -> about
      | "edit" -> Option.to_list (str "id") @ about
      | "remove" -> Option.to_list (str "id")
      | _ -> []
    in
    String.concat ~sep:" " (action :: target))
;;

(* While the model still writes the arguments, the inner ones are the text
   after ["args":], an unfinished object [Tool_args] reads leniently. *)
let on_host (call : P.Tool_call.t) =
  if not (String.equal call.name "on_host")
  then None
  else (
    let args = Tool_args.of_call call in
    match Tool_args.string args "host", Tool_args.string args "tool" with
    | Some host, Some tool ->
      let inner =
        match P.Json.parse call.arguments with
        | Ok json ->
          Option.value_map
            (P.Json.field json "args")
            ~default:"{}"
            ~f:P.Json.to_string
        | Error _ ->
          (match String.substr_index call.arguments ~pattern:{|"args"|} with
           | None -> "{}"
           | Some i ->
             let rest = String.drop_prefix call.arguments (i + 6) in
             (match String.index rest '{' with
              | Some j -> String.drop_prefix rest j
              | None -> "{}"))
      in
      Some (host, { call with name = tool; arguments = inner })
    | _ -> None)
;;

let on_host_line ~host line = sprintf "on %s: %s" host line

let as_call (s : P.Content.Server_tool.t) =
  let call = { P.Tool_call.id = s.id; name = s.name; arguments = s.input } in
  ( call
  , Option.map s.output ~f:(fun text ->
      { P.Message.Tool_result.tool_call_id = s.id
      ; tool_name = s.name
      ; text
      ; is_error = s.is_error
      ; images = []
      ; at = None
      ; elided = None
      }) )
;;

let one_line text =
  String.concat ~sep:" " (List.map (String.split_lines text) ~f:String.strip)
  |> String.strip
;;

let server_tool_arg (s : P.Content.Server_tool.t) =
  let args = Tool_args.of_call (fst (as_call s)) in
  let arg =
    match s.name, Tool_args.string args "query" with
    | "web_search", Some query -> "\"" ^ one_line query ^ "\""
    | _ -> one_line s.input
  in
  Option.some_if (not (String.is_empty arg)) arg
;;

let server_tool_line (s : P.Content.Server_tool.t) =
  match server_tool_arg s with
  | Some arg -> s.name ^ " " ^ arg
  | None -> s.name
;;

module Search_result = struct
  type t =
    { title : string
    ; url : string option
    }
  [@@deriving sexp_of]

  let numbered line =
    let line = String.strip line in
    let digits = String.take_while line ~f:Char.is_digit in
    if String.is_empty digits
    then None
    else
      String.chop_prefix
        (String.drop_prefix line (String.length digits))
        ~prefix:". "
      |> Option.map ~f:String.strip
  ;;

  let of_line line =
    match String.substr_index_all line ~may_overlap:false ~pattern:" — " with
    | [] ->
      if
        String.is_prefix line ~prefix:"http://"
        || String.is_prefix line ~prefix:"https://"
      then { title = line; url = Some line }
      else { title = line; url = None }
    | indices ->
      let i = List.last_exn indices in
      let url =
        String.strip (String.drop_prefix line (i + String.length " — "))
      in
      { title = String.strip (String.prefix line i)
      ; url = Option.some_if (not (String.is_empty url)) url
      }
  ;;

  let parse text =
    List.filter_map (String.split_lines text) ~f:(fun line ->
      Option.map (numbered line) ~f:of_line)
  ;;
end

module Fetched = struct
  type t =
    { url : string
    ; details : string
    ; body : string
    }
  [@@deriving sexp_of]

  let parse text =
    let header, body =
      match String.lsplit2 text ~on:'\n' with
      | Some (header, body) -> header, body
      | None -> text, ""
    in
    let open Option.Let_syntax in
    let%bind url, rest = String.lsplit2 (String.strip header) ~on:' ' in
    let%bind details = String.chop_prefix rest ~prefix:"(" in
    let%bind details = String.chop_suffix details ~suffix:")" in
    let%map () =
      Option.some_if
        (String.is_prefix url ~prefix:"http://"
         || String.is_prefix url ~prefix:"https://")
        ()
    in
    { url; details; body = String.lstrip ~drop:(Char.equal '\n') body }
  ;;
end
