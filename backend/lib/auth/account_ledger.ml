open! Core
open! Import

module Totals = struct
  type t =
    { requests : int
    ; input : int
    ; output : int
    ; cache_read : int
    ; cost_usd : float
    }
  [@@deriving sexp_of, equal]

  let zero =
    { requests = 0; input = 0; output = 0; cache_read = 0; cost_usd = 0. }
  ;;

  let add a b =
    { requests = a.requests + b.requests
    ; input = a.input + b.input
    ; output = a.output + b.output
    ; cache_read = a.cache_read + b.cache_read
    ; cost_usd = a.cost_usd +. b.cost_usd
    }
  ;;

  let to_json t =
    let int n = `Number (Int.to_string n) in
    `Object
      [ "requests", int t.requests
      ; "input", int t.input
      ; "output", int t.output
      ; "cache_read", int t.cache_read
      ; ( "cost_usd"
        , `Number (Float.to_string_hum ~decimals:6 ~strip_zero:true t.cost_usd)
        )
      ]
  ;;

  let of_json = function
    | `Object fields ->
      let num name =
        match List.Assoc.find fields ~equal:String.equal name with
        | Some (`Number n) -> Float.of_string_opt n |> Option.value ~default:0.
        | _ -> 0.
      in
      let int name = Float.iround_nearest_exn (num name) in
      { requests = int "requests"
      ; input = int "input"
      ; output = int "output"
      ; cache_read = int "cache_read"
      ; cost_usd = num "cost_usd"
      }
    | _ -> zero
  ;;
end

module Periods = struct
  type t =
    { today : Totals.t
    ; last_7_days : Totals.t
    ; last_30_days : Totals.t
    }
  [@@deriving sexp_of, equal]

  let zero =
    { today = Totals.zero
    ; last_7_days = Totals.zero
    ; last_30_days = Totals.zero
    }
  ;;

  let add a b =
    { today = Totals.add a.today b.today
    ; last_7_days = Totals.add a.last_7_days b.last_7_days
    ; last_30_days = Totals.add a.last_30_days b.last_30_days
    }
  ;;

  let to_json t =
    `Object
      [ "today", Totals.to_json t.today
      ; "last_7_days", Totals.to_json t.last_7_days
      ; "last_30_days", Totals.to_json t.last_30_days
      ]
  ;;
end

module Row = struct
  type t =
    { user : string
    ; usage : Periods.t
    }
  [@@deriving sexp_of]
end

(* login -> user -> day ("2026-11-24") -> totals *)
type data = Totals.t String.Map.t String.Map.t String.Map.t

(* Another process (or another server of this one) may write the file too:
   what was read stays good while the file is unchanged. *)
type t =
  { path : string
  ; mutable data : (float * data) option (** the file's mtime, and it *)
  }

let create ~home =
  { path = Filename.concat home ".prigh/account-usage.json"; data = None }
;;

let mtime t =
  match Core_unix.stat t.path with
  | exception _ -> None
  | st -> Some st.st_mtime
;;

let keep_days = 90
let day now = Date.to_string (Time_ns.to_date now ~zone:Time_float.Zone.utc)

let map_of_json json ~f =
  match json with
  | `Object fields ->
    String.Map.of_alist_reduce
      (List.map fields ~f:(fun (k, v) -> k, f v))
      ~f:(fun a _ -> a)
  | _ -> String.Map.empty
;;

let load t =
  let mtime = mtime t in
  match t.data, mtime with
  | Some (at, data), Some m when Float.equal at m -> data
  | _ ->
    let data =
      match In_channel.read_all t.path with
      | exception _ -> String.Map.empty
      | contents ->
        (match Json.parse contents with
         | Ok json ->
           map_of_json json ~f:(map_of_json ~f:(map_of_json ~f:Totals.of_json))
         | Error _ -> String.Map.empty)
    in
    t.data <- Option.map mtime ~f:(fun m -> m, data);
    data
;;

let save t (data : data) =
  let json =
    `Object
      (Map.to_alist data
       |> List.map ~f:(fun (login, users) ->
         ( login
         , `Object
             (Map.to_alist users
              |> List.map ~f:(fun (user, days) ->
                ( user
                , `Object
                    (Map.to_alist days
                     |> List.map ~f:(fun (d, totals) ->
                       d, Totals.to_json totals)) ))) )))
  in
  try
    Core_unix.mkdir_p ~perm:0o700 (Filename.dirname t.path);
    let tmp = t.path ^ ".tmp" in
    Out_channel.with_file tmp ~perm:0o600 ~f:(fun oc ->
      Out_channel.output_string oc (Json.to_string json));
    Core_unix.rename ~src:tmp ~dst:t.path;
    t.data <- Option.map (mtime t) ~f:(fun m -> m, data)
  with
  | _ -> t.data <- None
;;

let record ?(now = Time_ns.now ()) t ~login ~user (usage : Usage.t) ~cost_usd =
  let today = day now in
  let oldest =
    day (Time_ns.sub now (Time_ns.Span.of_day (Float.of_int keep_days)))
  in
  let add =
    { Totals.requests = 1
    ; input = usage.input
    ; output = usage.output
    ; cache_read = usage.cache_read
    ; cost_usd
    }
  in
  let data =
    Map.update (load t) login ~f:(fun users ->
      Map.update
        (Option.value users ~default:String.Map.empty)
        user
        ~f:(fun days ->
          Map.update
            (Option.value days ~default:String.Map.empty)
            today
            ~f:(fun totals ->
              Totals.add (Option.value totals ~default:Totals.zero) add)))
    |> Map.map
         ~f:(Map.map ~f:(Map.filter_keys ~f:(fun d -> String.( >= ) d oldest)))
  in
  save t data
;;

let rows ?(now = Time_ns.now ()) ?user t ~login =
  let today = day now in
  let since days =
    day (Time_ns.sub now (Time_ns.Span.of_day (Float.of_int (days - 1))))
  in
  let week = since 7
  and month = since 30 in
  Map.find (load t) login
  |> Option.value ~default:String.Map.empty
  |> Map.to_alist
  |> List.filter ~f:(fun (u, _) -> Option.for_all user ~f:(String.equal u))
  |> List.map ~f:(fun (user, days) ->
    let sum keep =
      Map.fold days ~init:Totals.zero ~f:(fun ~key ~data acc ->
        if keep key then Totals.add acc data else acc)
    in
    { Row.user
    ; usage =
        { today = sum (String.equal today)
        ; last_7_days = sum (fun d -> String.( >= ) d week)
        ; last_30_days = sum (fun d -> String.( >= ) d month)
        }
    })
;;

let usage ?now ?user t ~login =
  List.fold (rows ?now ?user t ~login) ~init:Periods.zero ~f:(fun acc r ->
    Periods.add acc r.usage)
;;

let forget t ~login =
  let data = load t in
  if Map.mem data login then save t (Map.remove data login)
;;

let single_user = "default"

let login provider id =
  sprintf "%s/%s" (Provider_id.to_string provider) (Account_id.to_string id)
;;
