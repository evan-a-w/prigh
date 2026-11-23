open! Core
open! Import

type t =
  { name : string
  ; display_name : string
  ; command : string list
  }
[@@deriving sexp_of, equal]

let known =
  [ { name = "opencode"
    ; display_name = "OpenCode"
    ; command = [ "opencode"; "acp" ]
    }
  ; { name = "gemini"
    ; display_name = "Gemini CLI"
    ; command = [ "gemini"; "--experimental-acp" ]
    }
  ; { name = "goose"; display_name = "Goose"; command = [ "goose"; "acp" ] }
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
      let display_name =
        Option.value_map (find_known name) ~default:name ~f:(fun a ->
          a.display_name)
      in
      Ok { name; display_name; command })
;;
