open! Core
open! Import

type t =
  { statuses : (Adapter_status.t list, string) Result.t option
  ; loading : bool
  }
[@@deriving sexp_of, equal]

let empty = { statuses = None; loading = false }
let loading t = { t with loading = true }
let loaded _ statuses = { statuses = Some statuses; loading = false }
let changed t statuses = { t with statuses = Some (Ok statuses) }

let by_host (statuses : Adapter_status.t list) =
  List.fold statuses ~init:[] ~f:(fun acc (s : Adapter_status.t) ->
    match acc with
    | (host, name, here) :: rest when String.equal host s.host ->
      (host, name, s :: here) :: rest
    | _ -> (s.host, s.host_name, [ s ]) :: acc)
  |> List.rev_map ~f:(fun (_, name, here) -> name, List.rev here)
;;
