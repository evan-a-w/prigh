open! Core
open! Import

type t =
  { statuses : (Adapter_status.t list, string) Result.t option
  ; loading : bool
  ; logins : Adapter_login.t list
  ; importing : string option
  }
[@@deriving sexp_of, equal]

let empty = { statuses = None; loading = false; logins = []; importing = None }
let loading t = { t with loading = true }

let loaded t = function
  | Ok (statuses, logins) ->
    { t with statuses = Some (Ok statuses); loading = false; logins }
  | Error e -> { t with statuses = Some (Error e); loading = false }
;;

let logins_changed t logins = { t with logins }
let importing t adapter = { t with importing = adapter }
let changed t statuses = { t with statuses = Some (Ok statuses) }

let by_host (statuses : Adapter_status.t list) =
  List.fold statuses ~init:[] ~f:(fun acc (s : Adapter_status.t) ->
    match acc with
    | (host, name, here) :: rest when String.equal host s.host ->
      (host, name, s :: here) :: rest
    | _ -> (s.host, s.host_name, [ s ]) :: acc)
  |> List.rev_map ~f:(fun (_, name, here) -> name, List.rev here)
;;
