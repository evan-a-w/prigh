open! Core
open! Import

type t =
  { id : string
  ; new_window : bool
  }
[@@deriving sexp_of]

let decide
      ~own_origin
      ~own_path
      ~href
      ~session
      ({ button; ctrl; meta; shift } : External_link.Click.t)
  =
  let id =
    match session with
    | Some _ -> session
    | None ->
      Option.bind (String.chop_prefix href ~prefix:(own_origin ^ own_path)) ~f:(fun rest ->
        Option.bind (String.chop_prefix rest ~prefix:"?") ~f:(fun _ ->
          Session_link.of_href rest))
  in
  if button > 1
  then None
  else
    Option.map id ~f:(fun id ->
      { id; new_window = ctrl || meta || shift || button = 1 })
;;
