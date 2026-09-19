open! Core
open! Import

let default_auth_file () =
  let config_home =
    match Sys.getenv "XDG_CONFIG_HOME" with
    | Some dir when not (String.is_empty dir) -> dir
    | _ ->
      Filename.concat (Option.value (Sys.getenv "HOME") ~default:".") ".config"
  in
  Filename.concat config_home "prigh/auth.json"
;;

let key_from_file file =
  match Sys_unix.file_exists_exn file with
  | false -> Ok None
  | true ->
    let open Or_error.Let_syntax in
    let%bind json = Json.parse (In_channel.read_all file) in
    (match Json.member "deepseek" json with
     | Some (`String key) -> Ok (Some key)
     | Some _ ->
       Or_error.error_s
         [%message "auth file: \"deepseek\" must be a string" (file : string)]
     | None -> Ok None)
;;

let deepseek_api_key ?(env = Sys.getenv) ?auth_file () =
  match env "DEEPSEEK_API_KEY" with
  | Some key when not (String.is_empty key) -> Ok key
  | _ ->
    let file = Option.value_or_thunk auth_file ~default:default_auth_file in
    (match key_from_file file with
     | Error _ as e -> e
     | Ok (Some key) -> Ok key
     | Ok None ->
       Or_error.error_s
         [%message
           "no DeepSeek API key: set DEEPSEEK_API_KEY or add {\"deepseek\": \
            \"<key>\"} to"
             (file : string)])
;;
