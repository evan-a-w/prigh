open! Core
open! Import

module Quirks = struct
  module Prompt_usage = struct
    type t =
      | Ignored
      | Gemini
    [@@deriving sexp_of, equal]
  end

  module Steer = struct
    type t =
      | Between_turns
      | Prompt
      | Xai_interject
    [@@deriving sexp_of, equal]
  end

  type t =
    { login : string option
    ; login_file : string option
    ; min_version : string option
    ; prompt_usage : Prompt_usage.t
    ; steer : Steer.t
    ; xai_extensions : bool
    }
  [@@deriving sexp_of, equal]

  let none =
    { login = None
    ; login_file = None
    ; min_version = None
    ; prompt_usage = Ignored
    ; steer = Between_turns
    ; xai_extensions = false
    }
  ;;
end

type t =
  { name : string
  ; display_name : string
  ; command : string list
  ; quirks : Quirks.t
  }
[@@deriving sexp_of, equal]

let known =
  [ { name = "opencode"
    ; display_name = "OpenCode"
    ; command = [ "opencode"; "acp" ]
    ; quirks = { Quirks.none with login = Some "opencode auth login" }
    }
  ; { name = "gemini"
    ; display_name = "Gemini CLI"
    ; command = [ "gemini"; "--experimental-acp" ]
    ; quirks = { Quirks.none with login = Some "gemini" }
    }
  ; { name = "antigravity"
    ; display_name = "Antigravity"
    ; command = [ "agy"; "acp" ]
    ; quirks =
        { Quirks.none with
          login = Some "agy acp --login"
        ; login_file = Some "~/.gemini/antigravity-cli/antigravity-oauth-token"
        ; min_version = Some "1.5.0"
        ; prompt_usage = Gemini
        ; steer = Prompt
        }
    }
  ; { name = "grok"
    ; display_name = "Grok Build"
    ; command = [ "grok"; "agent"; "stdio" ]
    ; quirks =
        { Quirks.none with
          login = Some "grok login"
        ; steer = Xai_interject
        ; xai_extensions = true
        }
    }
  ; { name = "goose"
    ; display_name = "Goose"
    ; command = [ "goose"; "acp" ]
    ; quirks = { Quirks.none with login = Some "goose configure" }
    }
  ]
;;

let find_known name = List.find known ~f:(fun a -> String.equal a.name name)

let of_spec spec =
  match String.lsplit2 spec ~on:'=' with
  | None ->
    Or_error.errorf
      "%S is not name=command: e.g. acp:myagent=myagent --acp"
      spec
  | Some (name, command) ->
    let name = String.strip name in
    let command =
      String.split_on_chars command ~on:[ ' '; '\t' ]
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    if String.is_empty name
    then
      Or_error.errorf "%S names no agent: e.g. acp:myagent=myagent --acp" spec
    else if List.is_empty command
    then
      Or_error.errorf
        "acp:%s has an empty command: e.g. acp:%s=%s --acp"
        name
        name
        name
    else (
      match find_known name with
      | Some known -> Ok { known with command }
      | None -> Ok { name; display_name = name; command; quirks = Quirks.none })
;;

let numbers version =
  String.split (String.chop_prefix_if_exists version ~prefix:"v") ~on:'.'
  |> List.map ~f:(fun part -> Int.of_string_opt (String.strip part))
  |> Option.all
;;

let older version ~than =
  match numbers version, numbers than with
  | Some v, Some m -> List.compare Int.compare v m < 0
  | _ -> false
;;

let start_hint t ~stderr =
  let lower = String.lowercase stderr in
  let unknown_command =
    List.exists
      [ "unexpected argument"
      ; "unknown command"
      ; "unknown argument"
      ; "unrecognized"
      ; "invalid choice"
      ]
      ~f:(fun substring -> String.is_substring lower ~substring)
  in
  match t.quirks.min_version with
  | Some minimum when unknown_command ->
    Some
      (sprintf
         "Update %s to %s or later: older versions do not have `%s`."
         t.display_name
         minimum
         (String.concat ~sep:" " t.command))
  | _ -> None
;;
