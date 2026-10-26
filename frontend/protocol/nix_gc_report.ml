open! Core

type t =
  { at : Time_ns.t
  ; by : string
  ; generations : Nix_generations.t option
  ; generations_deleted : int
  ; paths_deleted : int
  ; freed_bytes : int
  ; duration : Time_ns.Span.t
  }
[@@deriving sexp_of, equal]

let time_field j name =
  let open Or_error.Let_syntax in
  match%bind Json.time_ms_opt_field j name with
  | Some time -> Ok time
  | None -> Or_error.errorf "missing field %S" name
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind at = time_field j "at_ms" in
  let%bind by = Json.string_field j "by" in
  let%bind generations = Nix_generations.of_json (Json.field j "generations") in
  let%bind generations_deleted = Json.int_field j "generations_deleted" in
  let%bind paths_deleted = Json.int_field j "paths_deleted" in
  let%bind freed_bytes = Json.int_field j "freed_bytes" in
  let%map duration_ms = Json.int_field j "duration_ms" in
  { at
  ; by
  ; generations
  ; generations_deleted
  ; paths_deleted
  ; freed_bytes
  ; duration = Time_ns.Span.of_int_ms duration_ms
  }
;;
