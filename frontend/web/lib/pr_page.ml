open! Core
open! Import

module Checkout = struct
  type t =
    | Idle
    | Busy
    | Done of Pr_checkout.t
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Status = struct
  type t =
    | Loading
    | Failed of string
    | Loaded of Pr_detail.t
  [@@deriving sexp_of, equal]
end

type t =
  { number : int
  ; status : Status.t
  ; checkout : Checkout.t
  ; back : Pull_requests.t option
  }
[@@deriving sexp_of, equal]

let create ?back number = { number; status = Loading; checkout = Idle; back }

let pull_request t =
  match t.status with
  | Loaded d -> Some d.pr
  | Loading | Failed _ ->
    Option.bind t.back ~f:(fun l -> Pull_requests.find l t.number)
;;

let forge_label t =
  let kind =
    match t.status, t.back with
    | Loaded d, _ -> Some d.repo.kind
    | _, Some { status = Loaded repo; _ } -> Some repo.kind
    | _ -> None
  in
  match kind with
  | Some "github" -> "GitHub"
  | Some "forgejo" -> "Forgejo"
  | Some _ | None -> "the forge"
;;
