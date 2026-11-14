open! Core
open! Import

module Action = struct
  type t =
    | List
    | Reconnect
    | Change of
        { request : P.Request.Method.t
        ; done_ : string
        }
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage =
  "usage: /mcp [reconnect], /mcp add NAME URL|COMMAND [ARGS...], /mcp add \
   {\"NAME\": {...}} (mcpServers JSON), /mcp remove|enable|disable NAME"
;;

let split_words s =
  let words = Queue.create () in
  let word = Buffer.create 16 in
  let in_word = ref false in
  let finish () =
    if !in_word then Queue.enqueue words (Buffer.contents word);
    Buffer.clear word;
    in_word := false
  in
  let n = String.length s in
  let rec plain i =
    if i >= n
    then Ok ()
    else (
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' ->
        finish ();
        plain (i + 1)
      | '\'' ->
        in_word := true;
        single (i + 1)
      | '"' ->
        in_word := true;
        double (i + 1)
      | '\\' when i + 1 < n ->
        in_word := true;
        Buffer.add_char word s.[i + 1];
        plain (i + 2)
      | c ->
        in_word := true;
        Buffer.add_char word c;
        plain (i + 1))
  and single i =
    if i >= n
    then Or_error.error_string "a single quote (') is not closed"
    else if Char.equal s.[i] '\''
    then plain (i + 1)
    else (
      Buffer.add_char word s.[i];
      single (i + 1))
  and double i =
    if i >= n
    then Or_error.error_string "a double quote (\") is not closed"
    else (
      match s.[i] with
      | '"' -> plain (i + 1)
      | '\\' when i + 1 < n && String.mem "\"\\$`" s.[i + 1] ->
        Buffer.add_char word s.[i + 1];
        double (i + 2)
      | c ->
        Buffer.add_char word c;
        double (i + 1))
  in
  Or_error.map (plain 0) ~f:(fun () ->
    finish ();
    Queue.to_list words)
;;

let is_url s =
  List.exists [ "https://"; "http://" ] ~f:(fun prefix ->
    String.is_prefix s ~prefix)
;;

let add rest =
  if String.is_prefix rest ~prefix:"{"
  then
    Action.Change
      { request = Add_mcp_servers_text { text = rest }
      ; done_ = "added the MCP servers to your settings"
      }
  else (
    match split_words rest with
    | Error e -> Error (sprintf "%s; %s" (Error.to_string_hum e) usage)
    | Ok ([] | [ _ ]) ->
      Error
        "give a name, then a URL or a command: /mcp add NAME URL|COMMAND \
         [ARGS...]"
    | Ok [ name; url ] when is_url url ->
      Change
        { request = Add_mcp_servers [ name, Http { url; headers = [] } ]
        ; done_ = sprintf "added MCP server %s (%s) to your settings" name url
        }
    | Ok (name :: command :: args) ->
      Change
        { request = Add_mcp_servers [ name, Stdio { command; args; env = [] } ]
        ; done_ =
            sprintf
              "added MCP server %s (%s) to your settings"
              name
              (P.Mcp_settings.Transport.summary
                 (Stdio { command; args; env = [] }))
        })
;;

let parse rest =
  let rest = String.strip rest in
  let verb, after =
    match String.lsplit2 rest ~on:' ' with
    | Some (verb, after) -> verb, String.strip after
    | None -> rest, ""
  in
  let one_name ~f =
    match
      String.split after ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    with
    | [ name ] -> f name
    | _ -> Action.Error (sprintf "usage: /mcp %s NAME" verb)
  in
  match verb with
  | "" -> Action.List
  | "reconnect" when String.is_empty after -> Reconnect
  | "add" -> add after
  | "remove" | "rm" ->
    one_name ~f:(fun name ->
      Change
        { request = Remove_mcp_server { name }
        ; done_ = sprintf "removed MCP server %s from your settings" name
        })
  | ("enable" | "disable") as verb ->
    let enabled = String.equal verb "enable" in
    one_name ~f:(fun name ->
      Change
        { request = Set_mcp_server_enabled { name; enabled }
        ; done_ = sprintf "%sd MCP server %s" verb name
        })
  | _ -> Error usage
;;

let source_label ~home source =
  if String.equal source P.Mcp_settings.source
  then "Settings"
  else (
    match home with
    | Some home when String.is_prefix source ~prefix:(home ^ "/") ->
      "~" ^ String.drop_prefix source (String.length home)
    | _ -> source)
;;

let edit_hint source =
  if String.equal source P.Mcp_settings.source
  then "edit it in Settings → MCP servers or with /mcp add|remove"
  else sprintf "check its entry in %s" source
;;
