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
