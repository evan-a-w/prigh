open! Core
open! Import

let binding = "prighOpenExternal"

module Click = struct
  type t =
    { button : int
    ; ctrl : bool
    ; meta : bool
    ; shift : bool
    }
  [@@deriving sexp_of]
end

type t =
  { url : string
  ; new_window : bool
  }
[@@deriving sexp_of]

(* [scheme://host[:port]], lowercased, without the scheme's default port. *)
let origin url =
  match String.substr_index url ~pattern:"://" with
  | None -> None
  | Some i ->
    let scheme = String.lowercase (String.prefix url i) in
    let rest = String.drop_prefix url (i + 3) in
    let authority =
      String.take_while rest ~f:(fun c -> not (String.mem "/?#" c))
    in
    let host =
      String.lowercase
        (match String.rsplit2 authority ~on:'@' with
         | Some (_, host) -> host
         | None -> authority)
    in
    let default_port =
      match scheme with
      | "http" -> Some ":80"
      | "https" -> Some ":443"
      | _ -> None
    in
    let host =
      Option.value_map default_port ~default:host ~f:(fun suffix ->
        String.chop_suffix_if_exists host ~suffix)
    in
    Option.some_if (not (String.is_empty host)) (scheme ^ "://" ^ host)
;;

let new_window_target target =
  match String.lowercase target with
  | "" | "_self" | "_top" | "_parent" -> false
  | _ -> true
;;

let decide
      ~own_origin
      ~href
      ~target
      ~download
      ({ button; ctrl; meta; shift } : Click.t)
  =
  let lower = String.lowercase href in
  let elsewhere =
    if String.is_prefix lower ~prefix:"mailto:"
    then true
    else if
      String.is_prefix lower ~prefix:"http://"
      || String.is_prefix lower ~prefix:"https://"
    then (
      match origin href with
      | Some o -> not (String.equal o (String.lowercase own_origin))
      | None -> false)
    else false
  in
  if download || (button <> 0 && button <> 1) || not elsewhere
  then None
  else
    Some
      { url = href
      ; new_window =
          new_window_target target || ctrl || meta || shift || button = 1
      }
;;

let payload { url; new_window } =
  Json.to_string
    (Json.obj
       [ "url", Json.str url
       ; ("newWindow", if new_window then `True else `False)
       ])
;;
