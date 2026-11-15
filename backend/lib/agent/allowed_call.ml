open! Core
open! Import

module Call = struct
  module T = struct
    type t =
      { tool : string
      ; arguments : string
      ; host : string
      ; cwd : string
      }
    [@@deriving sexp, jsonaf, equal, compare, hash]
  end

  include T
  include Hashable.Make (T)

  let rec canonical : Json.t -> Json.t = function
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (k, v) -> k, canonical v)
         |> List.stable_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
        )
    | `Array items -> `Array (List.map items ~f:canonical)
    | (`String _ | `Number _ | `True | `False | `Null) as atom -> atom
  ;;

  let create ~tool ~arguments ~host ~cwd =
    { tool; arguments = Json.to_string (canonical arguments); host; cwd }
  ;;
end

type t =
  { call : Call.t
  ; by : string
  ; reason : string option [@jsonaf.option] [@sexp.option]
  }
[@@deriving sexp, jsonaf, equal]

let you = "you"

let note t =
  match t.reason with
  | None -> "this exact call earlier"
  | Some reason -> sprintf "this exact call earlier (%s)" reason
;;

let describe t ~host_name =
  sprintf
    "%s %s on %s in %s (by %s)"
    t.call.tool
    t.call.arguments
    (host_name t.call.host)
    t.call.cwd
    t.by
;;
