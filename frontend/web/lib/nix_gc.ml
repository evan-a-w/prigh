open! Core
open! Import

module Status = struct
  type t =
    | Unknown
    | Loading
    | Loaded of Nix_store_status.t
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; collecting : Nix_gc_progress.t option
  ; outcome : (Nix_gc_report.t, string) Result.t option
  }
[@@deriving sexp_of, equal]

let empty = { status = Unknown; collecting = None; outcome = None }

let loading t =
  match t.status with
  | Loaded _ -> t (* refreshed in place *)
  | Unknown | Loading | Failed _ -> { t with status = Loading }
;;

let status t = function
  | Ok s -> { t with status = Loaded s }
  | Error e -> { t with status = Failed e }
;;

let start t =
  { t with
    collecting =
      Some { paths_deleted = 0; generations_deleted = 0; message = None }
  ; outcome = None
  }
;;

let progress t p =
  match t.collecting with
  | Some _ -> { t with collecting = Some p }
  | None -> t
;;

let finished t result =
  { t with collecting = None; outcome = Some result }
;;
