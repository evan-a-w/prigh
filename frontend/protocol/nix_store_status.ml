open! Core

let optional j name ~f =
  match Json.field j name with
  | None | Some `Null -> Ok None
  | Some v -> Or_error.map (f v) ~f:Option.some
;;

module Running = struct
  type t =
    { by : string
    ; since : Time_ns.t
    ; generations : Nix_generations.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind by = Json.string_field j "by" in
    let%bind since =
      match%bind Json.time_ms_opt_field j "since_ms" with
      | Some since -> Ok since
      | None -> Or_error.error_string "missing field \"since_ms\""
    in
    let%map generations =
      Nix_generations.of_json (Json.field j "generations")
    in
    { by; since; generations }
  ;;
end

module Schedule = struct
  type t =
    { interval : Time_ns.Span.t
    ; keep_days : int option
    ; next_at : Time_ns.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind interval_seconds = Json.int_field j "interval_seconds" in
    let%bind keep_days =
      optional j "keep_days" ~f:(fun _ -> Json.int_field j "keep_days")
    in
    let%map next_at = Json.time_ms_opt_field j "next_at_ms" in
    { interval = Time_ns.Span.of_int_sec interval_seconds; keep_days; next_at }
  ;;
end

type t =
  { store_bytes : int
  ; store_paths : int
  ; fs_total_bytes : int
  ; fs_free_bytes : int
  ; last_gc : Nix_gc_report.t option
  ; running : Running.t option
  ; schedule : Schedule.t option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind store_bytes = Json.int_field j "store_bytes" in
  let%bind store_paths = Json.int_field j "store_paths" in
  let%bind fs_total_bytes = Json.int_field j "fs_total_bytes" in
  let%bind fs_free_bytes = Json.int_field j "fs_free_bytes" in
  let%bind last_gc = optional j "last_gc" ~f:Nix_gc_report.of_json in
  let%bind running = optional j "running" ~f:Running.of_json in
  let%map schedule = optional j "schedule" ~f:Schedule.of_json in
  { store_bytes
  ; store_paths
  ; fs_total_bytes
  ; fs_free_bytes
  ; last_gc
  ; running
  ; schedule
  }
;;
