open! Core

let message () =
  let path =
    match Sys.getenv "PATH" with
    | Some p when not (String.is_empty p) -> p
    | _ -> "(empty)"
  in
  sprintf
    "git was not found on the tool host's PATH (%s): install git in one of \
     those directories, or restart prigh with a PATH that has git on it"
    path
;;
