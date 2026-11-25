open! Core
open! Import

module Install = struct
  type t =
    | Installed of
        { version : string option
        ; path : string
        }
    | Not_installed of { login_shell_path : string option }
    | Unknown of string
  [@@deriving sexp_of, equal]
end

module Auth = struct
  type t =
    | Logged_in of { account : string option }
    | Logged_out
    | Not_needed
    | Unknown
  [@@deriving sexp_of, equal]
end

let acp_agent ?(acp = Acp_agent.find_known) name = acp name

let quirks ?acp : Adapter_id.t -> Acp_agent.Quirks.t = function
  | Acp name ->
    Option.value_map
      (acp_agent ?acp name)
      ~default:Acp_agent.Quirks.none
      ~f:(fun a -> a.quirks)
  | Codex | Claude | Prigh -> Acp_agent.Quirks.none
;;

let program ?acp : Adapter_id.t -> string option = function
  | Codex -> Some "codex"
  | Claude -> Some "claude"
  | Acp name ->
    (match acp_agent ?acp name with
     | Some agent -> List.hd agent.command
     | None -> Some name)
  | Prigh -> None
;;

let login_command (quirks : Acp_agent.Quirks.t) : Adapter_id.t -> string option
  = function
  | Codex -> Some "codex login"
  | Claude -> Some "claude auth login"
  | Acp _ -> quirks.login
  | Prigh -> None
;;

module Status = struct
  type t =
    { adapter : Adapter_id.t
    ; host : string
    ; host_name : string
    ; install : Install.t
    ; auth : Auth.t
    ; checked_at : float
    ; quirks : Acp_agent.Quirks.t
    }
  [@@deriving sexp_of, equal]

  let too_old t =
    match t.install, t.quirks.min_version with
    | Installed { version = Some version; _ }, Some minimum
      when Acp_agent.older version ~than:minimum -> Some (version, minimum)
    | _ -> None
  ;;

  let hint t =
    match t.install, t.auth, login_command t.quirks t.adapter with
    | Installed _, Unknown, Some command ->
      Some
        (sprintf
           "prigh can't tell whether %s is logged in on %s: if it asks, run \
            `%s` there"
           (Adapter_id.label t.adapter)
           t.host_name
           command)
    | _ -> None
  ;;

  let problem t =
    let label = Adapter_id.label t.adapter in
    match t.install, t.auth with
    | Not_installed { login_shell_path = None }, _ ->
      Some
        (sprintf
           "%s is not installed on %s, please install it"
           label
           t.host_name)
    | Not_installed { login_shell_path = Some path }, _ ->
      Some
        (sprintf
           "%s is installed on %s at %s, but not on the PATH of prigh's tool \
            host there: add %s to that PATH and restart the tool host"
           label
           t.host_name
           path
           (Filename.dirname path))
    | Unknown why, _ ->
      Some
        (sprintf
           "can't tell whether %s is installed on %s: %s"
           label
           t.host_name
           why)
    | Installed _, Logged_out ->
      Some
        (match login_command t.quirks t.adapter with
         | Some command ->
           sprintf
             "%s on %s is not logged in: run `%s` there"
             label
             t.host_name
             command
         | None ->
           sprintf
             "%s on %s is not logged in: log in with its own command there"
             label
             t.host_name)
    | Installed _, (Logged_in _ | Not_needed | Unknown) ->
      Option.map (too_old t) ~f:(fun (version, minimum) ->
        sprintf
          "%s %s on %s is too old to drive: update it to %s or later there"
          label
          version
          t.host_name
          minimum)
  ;;

  let to_json t =
    let str s = `String s in
    let opt name = Option.value_map ~default:[] ~f:(fun v -> [ name, str v ]) in
    let install =
      match t.install with
      | Installed { version; path } ->
        [ "installed", `True ] @ opt "version" version @ [ "path", str path ]
      | Not_installed { login_shell_path } ->
        [ "installed", `False ] @ opt "login_shell_path" login_shell_path
      | Unknown why -> [ "installed", `Null; "unknown", str why ]
    in
    let auth, account =
      match t.auth with
      | Logged_in { account } -> "logged_in", account
      | Logged_out -> "logged_out", None
      | Not_needed -> "not_needed", None
      | Unknown -> "unknown", None
    in
    `Object
      ([ "adapter", str (Adapter_id.to_string t.adapter)
       ; "label", str (Adapter_id.label t.adapter)
       ; "host", str t.host
       ; "host_name", str t.host_name
       ]
       @ install
       @ [ "auth", str auth ]
       @ opt "account" account
       @ opt "problem" (problem t)
       @ opt "hint" (hint t)
       @ [ ( "checked_at"
           , `Number (Int.to_string (Float.iround_down_exn t.checked_at)) )
         ])
  ;;
end

let status_args : Adapter_id.t -> string list = function
  | Codex -> [ "login"; "status" ]
  | Claude -> [ "auth"; "status" ]
  | Acp _ | Prigh -> []
;;

let request ?acp adapters : Host_probe.Request.t =
  { programs =
      List.filter_map adapters ~f:(fun adapter ->
        Option.map (program ?acp adapter) ~f:(fun name ->
          { Host_probe.Program.name
          ; version = [ "--version" ]
          ; status = status_args adapter
          }))
  ; paths = []
  ; env = []
  }
;;

let exit_code : Process.Exit.t -> int option = function
  | Exited n -> Some n
  | Signaled _ | Timed_out | Cancelled -> None
;;

(* The first word that looks like a version: "codex-cli 0.166.0",
   "2.1.330 (Claude Code)", "v1.2.3". *)
let parse_version text =
  String.split_on_chars (String.strip text) ~on:[ ' '; '\n'; '\t' ]
  |> List.find_map ~f:(fun word ->
    let word = String.chop_prefix_if_exists word ~prefix:"v" in
    if
      (not (String.is_empty word))
      && Char.is_digit word.[0]
      && String.contains word '.'
    then Some word
    else None)
;;

(* [codex login status] says "Logged in using ChatGPT" or "Logged in using an
   API key - sk-..." (exit 0), or "Not logged in" (exit 1). *)
let codex_auth (out : Host_probe.Command_output.t) : Auth.t =
  let text = String.strip (out.stdout ^ "\n" ^ out.stderr) in
  let lower = String.lowercase text in
  match exit_code out.exit with
  | Some 0 ->
    let account =
      if String.is_substring lower ~substring:"chatgpt"
      then Some "ChatGPT"
      else if String.is_substring lower ~substring:"api key"
      then Some "API key"
      else None
    in
    Logged_in { account }
  | Some _ when String.is_substring lower ~substring:"not logged in" ->
    Logged_out
  | Some 1 -> Logged_out
  | _ -> Unknown
;;

let claude_auth (out : Host_probe.Command_output.t) : Auth.t =
  match exit_code out.exit with
  | None -> Unknown
  | Some exit_code ->
    (match Claude_health.parse_auth_status ~stdout:out.stdout ~exit_code with
     | Logged_in { method_; email } ->
       Logged_in { account = Option.first_some email method_ }
     | Logged_out -> Logged_out
     | Unknown _ -> Unknown)
;;

let auth adapter (found : Host_probe.Found.t) : Auth.t =
  match (adapter : Adapter_id.t), found.status with
  | Prigh, _ -> Not_needed
  | _, None -> Unknown
  | Codex, Some out -> codex_auth out
  | Claude, Some out -> claude_auth out
  | Acp _, Some _ -> Unknown
;;

let of_found
      ?acp
      adapter
      ~host
      ~host_name
      ~checked_at
      (found : Host_probe.Found.t)
  : Status.t
  =
  match found.path with
  | None ->
    { adapter
    ; host
    ; host_name
    ; install = Not_installed { login_shell_path = found.login_shell_path }
    ; auth = Unknown
    ; checked_at
    ; quirks = quirks ?acp adapter
    }
  | Some path ->
    let version =
      Option.bind found.version ~f:(fun (out : Host_probe.Command_output.t) ->
        parse_version (out.stdout ^ " " ^ out.stderr))
    in
    { adapter
    ; host
    ; host_name
    ; install = Installed { version; path }
    ; auth = auth adapter found
    ; checked_at
    ; quirks = quirks ?acp adapter
    }
;;

let prigh_status ~host ~host_name ~checked_at : Status.t =
  { adapter = Prigh
  ; host
  ; host_name
  ; install = Installed { version = Some Version.to_string; path = "prigh" }
  ; auth = Not_needed
  ; checked_at
  ; quirks = Acp_agent.Quirks.none
  }
;;

let unknown ?acp adapters ~host ~host_name ~checked_at why =
  List.map adapters ~f:(fun (adapter : Adapter_id.t) ->
    match adapter with
    | Prigh -> prigh_status ~host ~host_name ~checked_at
    | _ ->
      { Status.adapter
      ; host
      ; host_name
      ; install = Unknown why
      ; auth = Unknown
      ; checked_at
      ; quirks = quirks ?acp adapter
      })
;;

let of_result
      ?acp
      adapters
      ~host
      ~host_name
      ~checked_at
      (result : Tool.Result.t)
  =
  let old_host =
    String.is_prefix
      result.text
      ~prefix:(sprintf "unknown host tool %S" Host_probe.op)
  in
  if old_host
  then
    unknown
      ?acp
      adapters
      ~host
      ~host_name
      ~checked_at
      "its prigh is too old to check: update prigh there and restart its TUI \
       or tool-host"
  else if result.is_error
  then
    unknown
      ?acp
      adapters
      ~host
      ~host_name
      ~checked_at
      (String.strip result.text)
  else (
    match Host_probe.Reply.of_result result with
    | Error e ->
      unknown ?acp adapters ~host ~host_name ~checked_at (Error.to_string_hum e)
    | Ok reply ->
      List.map adapters ~f:(fun adapter ->
        match program ?acp adapter with
        | None -> prigh_status ~host ~host_name ~checked_at
        | Some name ->
          (match
             List.find reply.programs ~f:(fun (f : Host_probe.Found.t) ->
               String.equal f.name name)
           with
           | Some found ->
             of_found ?acp adapter ~host ~host_name ~checked_at found
           | None ->
             { Status.adapter
             ; host
             ; host_name
             ; install = Unknown "the host did not report it"
             ; auth = Unknown
             ; checked_at
             ; quirks = quirks ?acp adapter
             })))
;;

let probe ?acp ~exec adapters ~host ~host_name ~now =
  let result =
    if List.exists adapters ~f:(fun a -> Option.is_some (program ?acp a))
    then
      exec
        ~name:Host_probe.op
        (Host_probe.Request.to_json (request ?acp adapters))
    else { Tool.Result.text = "{}"; is_error = false; images = [] }
  in
  of_result ?acp adapters ~host ~host_name ~checked_at:now result
;;

module Key = struct
  module T = struct
    type t = string * string [@@deriving sexp, compare, hash]
  end

  include T
  include Hashable.Make (T)
end

module Cache = struct
  type t =
    { ttl : float
    ; statuses : Status.t Key.Table.t
    }

  let create ~ttl = { ttl; statuses = Key.Table.create () }
  let key ~host adapter = host, Adapter_id.to_string adapter
  let find t ~host adapter = Hashtbl.find t.statuses (key ~host adapter)

  let fresh t ~now ~host adapter =
    match find t ~host adapter with
    | Some s -> Float.(now - s.checked_at < t.ttl)
    | None -> false
  ;;

  let set t statuses =
    List.iter statuses ~f:(fun (s : Status.t) ->
      Hashtbl.set t.statuses ~key:(key ~host:s.host s.adapter) ~data:s)
  ;;

  let forget_host t ~host =
    Hashtbl.filter_keys_inplace t.statuses ~f:(fun (h, _) ->
      not (String.equal h host))
  ;;

  let forget t ~host adapter = Hashtbl.remove t.statuses (key ~host adapter)
end
