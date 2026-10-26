open! Core
open! Import

let code_of_hash hash =
  let hash = String.chop_prefix_if_exists hash ~prefix:"#" in
  List.find_map (String.split hash ~on:'&') ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some ("signin", code) when not (String.is_empty code) -> Some code
    | _ -> None)
;;

let path = "/signin-handoff"

module Credentials = struct
  type t =
    { user : string option
    ; token : string option
    ; prefer_host : string option
    }
  [@@deriving sexp_of]

  let of_json text =
    match Json.parse text with
    | Ok (`Object _ as json) ->
      let string name =
        match Json.field json name with
        | Some (`String s) when not (String.is_empty s) -> Some s
        | _ -> None
      in
      Some
        { user = string "user"
        ; token = string "token"
        ; prefer_host = string "prefer_host"
        }
    | Ok _ | Error _ -> None
  ;;
end

let prefer_host_key = "prigh.prefer_host"
