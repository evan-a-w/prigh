open! Core
open! Import

let int n = `Number (Int.to_string n)
let ms seconds = int (Float.iround_nearest_exn (seconds *. 1000.))
let opt f = Option.value_map ~default:`Null ~f

let get fields key =
  match List.Assoc.find fields ~equal:String.equal key with
  | Some v -> Ok v
  | None -> Or_error.errorf "no %s" key
;;

let float fields key =
  Or_error.bind (get fields key) ~f:(fun v ->
    match Float.of_string_opt v with
    | Some f -> Ok f
    | None -> Or_error.errorf "bad %s %S" key v)
;;

module Running = struct
  type t =
    { by : string
    ; since : float
    ; generations : Nix_generations.t option
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "by", `String t.by
      ; "since_ms", ms t.since
      ; "generations", Nix_generations.to_json t.generations
      ]
  ;;

  let of_fields text =
    let fields = Nix_gc_report.fields text in
    let open Or_error.Let_syntax in
    let%bind by = get fields "by" in
    let%bind since = float fields "since" in
    let%map generations = get fields "generations" >>= Nix_generations.of_arg in
    { by; since; generations }
  ;;
end

module Schedule = struct
  type t =
    { interval_seconds : int
    ; keep_days : int option
    ; next_at : float option
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "interval_seconds", int t.interval_seconds
      ; "keep_days", opt int t.keep_days
      ; "next_at_ms", opt ms t.next_at
      ]
  ;;

  let of_fields text =
    let fields = Nix_gc_report.fields text in
    let open Or_error.Let_syntax in
    let nat key =
      let%bind v = get fields key in
      match Int.of_string_opt v with
      | Some n when n >= 0 -> Ok n
      | _ -> Or_error.errorf "bad %s %S" key v
    in
    let%bind interval_seconds = nat "interval" in
    let%bind keep_days =
      match%bind get fields "keep_days" with
      | "off" -> Ok None
      | _ -> nat "keep_days" >>| Option.some
    in
    let%map next_at =
      match List.Assoc.find fields ~equal:String.equal "next_at" with
      | None | Some "" -> Ok None
      | Some _ -> float fields "next_at" >>| Option.some
    in
    { interval_seconds; keep_days; next_at }
  ;;
end

module Store = struct
  type t =
    { bytes : int
    ; paths : int
    ; fs_total_bytes : int
    ; fs_free_bytes : int
    }
  [@@deriving sexp_of]
end

type t =
  { store : Store.t
  ; last_gc : Nix_gc_report.t option
  ; running : Running.t option
  ; schedule : Schedule.t option
  }
[@@deriving sexp_of]

let to_json t =
  `Object
    [ "store_bytes", int t.store.bytes
    ; "store_paths", int t.store.paths
    ; "fs_total_bytes", int t.store.fs_total_bytes
    ; "fs_free_bytes", int t.store.fs_free_bytes
    ; "last_gc", opt Nix_gc_report.to_json t.last_gc
    ; "running", opt Running.to_json t.running
    ; "schedule", opt Schedule.to_json t.schedule
    ]
;;
