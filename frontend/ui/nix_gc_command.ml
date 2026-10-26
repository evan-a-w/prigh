open! Core
open! Import

module Action = struct
  type t =
    | Status
    | Collect of P.Nix_generations.t option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "/nix-gc [run [DAYS|all]]"

let usage_error what =
  Action.Error
    (sprintf
       "%s: %s (no argument: show the store; run: collect garbage; run DAYS: \
        first delete profile generations older than DAYS days; run all: all \
        but the current ones)"
       what
       usage)
;;

let parse rest =
  match
    String.split rest ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] -> Action.Status
  | [ "run" ] -> Collect None
  | [ "run"; "all" ] -> Collect (Some All)
  | [ "run"; days ] ->
    (match
       Int.of_string_opt (String.chop_suffix_if_exists days ~suffix:"d")
     with
     | None -> usage_error (sprintf "%S is not a number of days" days)
     | Some days ->
       (match P.Nix_generations.of_days days with
        | Ok generations -> Collect (Some generations)
        | Error e -> usage_error (Error.to_string_hum e)))
  | _ -> usage_error "usage"
;;

let describe_generations : P.Nix_generations.t option -> string option =
  function
  | None -> None
  | Some All -> Some "deleting every old profile generation"
  | Some (Older_than_days 1) ->
    Some "deleting profile generations older than 1 day"
  | Some (Older_than_days days) ->
    Some (sprintf "deleting profile generations older than %d days" days)
;;

let warning generations =
  Option.map (describe_generations generations) ~f:(fun what ->
    sprintf
      "Collect Nix garbage, first %s of every user? They can no longer roll \
       back to them (current ones are kept)."
      what)
;;

let starting generations =
  match describe_generations generations with
  | None -> "collecting Nix garbage…"
  | Some what -> sprintf "collecting Nix garbage, %s…" what
;;

let bytes n =
  let rec go value = function
    | _ :: (_ :: _ as rest) when Float.(value >= 1024.) ->
      go (value /. 1024.) rest
    | unit :: _ -> sprintf "%.1f %s" value unit
    | [] -> assert false
  in
  if Int64.(n < 1024L)
  then sprintf "%Ld B" n
  else go (Int64.to_float n /. 1024.) [ "KiB"; "MiB"; "GiB"; "TiB" ]
;;

let span s =
  let seconds = Float.iround_nearest_exn (Time_ns.Span.to_sec s) in
  let seconds = Int.max 0 seconds in
  if seconds < 60
  then sprintf "%d s" seconds
  else if seconds < 3600
  then sprintf "%d min" (seconds / 60)
  else if seconds < 86400
  then sprintf "%d h" (seconds / 3600)
  else (
    let days = seconds / 86400 in
    sprintf "%d day%s" days (if days = 1 then "" else "s"))
;;

let describe_by = function
  | "schedule" -> "the automatic collection"
  | "console" -> "prigh-docker nix-gc"
  | by -> by
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let deleted ~paths ~generations =
  String.concat
    ~sep:", "
    (plural paths "path"
     :: (if generations > 0 then [ plural generations "generation" ] else []))
  ^ " deleted"
;;

let report (r : P.Nix_gc_report.t) =
  sprintf
    "freed %s: %s in %s"
    (bytes r.freed_bytes)
    (deleted ~paths:r.paths_deleted ~generations:r.generations_deleted)
    (span r.duration)
;;

let deleted_so_far (p : P.Nix_gc_progress.t) =
  deleted ~paths:p.paths_deleted ~generations:p.generations_deleted
;;

let progress p = "nix-gc: " ^ deleted_so_far p

let time at ~utc_offset =
  let date, ofday =
    Time_ns.to_date_ofday (Time_ns.add at utc_offset) ~zone:Time_ns.Zone.utc
  in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf "%s %02d:%02d" (Date.to_string date) parts.hr parts.min
;;

let size (s : P.Nix_store_status.t) =
  sprintf "store: %s in %d paths" (bytes s.store_bytes) s.store_paths
;;

let space (s : P.Nix_store_status.t) =
  sprintf
    "/nix: %s free of %s (%d%%)"
    (bytes s.fs_free_bytes)
    (bytes s.fs_total_bytes)
    (if Int64.(s.fs_total_bytes > 0L)
     then
       Float.iround_down_exn
         (100.
          *. Int64.to_float s.fs_free_bytes
          /. Int64.to_float s.fs_total_bytes)
     else 0)
;;

let details ~utc_offset (s : P.Nix_store_status.t) =
  let time at = time at ~utc_offset in
  [ (match s.last_gc with
     | None -> "last collection: none recorded"
     | Some r ->
       sprintf
         "last collection: %s by %s, %s"
         (time r.at)
         (describe_by r.by)
         (report r))
  ]
  @ Option.to_list
      (Option.map s.running ~f:(fun r ->
         sprintf
           "running: started by %s at %s%s"
           (describe_by r.by)
           (time r.since)
           (Option.value_map
              (describe_generations r.generations)
              ~default:""
              ~f:(( ^ ) ", "))))
  @ Option.to_list
      (Option.map s.schedule ~f:(fun (sched : P.Nix_store_status.Schedule.t) ->
         match sched.next_at with
         | None -> "automatic: off (PRIGH_NIX_GC_INTERVAL=0)"
         | Some next ->
           sprintf
             "automatic: every %s, %s; next at %s"
             (span sched.interval)
             (match sched.keep_days with
              | None -> "keeping profile generations"
              | Some 0 -> "deleting every old profile generation"
              | Some days ->
                sprintf
                  "deleting profile generations older than %s"
                  (plural days "day"))
             (time next)))
;;

let status_lines ~utc_offset s =
  [ size s; space s ]
  @ details ~utc_offset s
  @ [ "/nix-gc run collects garbage; run DAYS (or all) first deletes older \
       profile generations"
    ]
;;
