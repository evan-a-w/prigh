open! Core

module Status = struct
  type t =
    | Pending
    | In_progress
    | Completed
  [@@deriving sexp_of, equal]

  let of_string = function
    | "pending" -> Ok Pending
    | "in_progress" -> Ok In_progress
    | "completed" -> Ok Completed
    | s -> Or_error.errorf "unknown plan step status %S" s
  ;;
end

module Step = struct
  type t =
    { step : string
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind step = Json.string_field j "step" in
    let%map status = Json.string_field j "status" >>= Status.of_string in
    { step; status }
  ;;
end

type t =
  { explanation : string option
  ; steps : Step.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind explanation = Json.string_opt_field j "explanation" in
  let%map steps = Json.list_field j "steps" ~f:Step.of_json in
  { explanation; steps }
;;

let of_arguments j =
  let open Or_error.Let_syntax in
  let%bind explanation = Json.string_opt_field j "explanation" in
  let%map steps = Json.list_field j "plan" ~f:Step.of_json in
  match steps with
  | [] -> None
  | steps -> Some { explanation; steps }
;;

let completed t =
  List.count t.steps ~f:(fun s -> Status.equal s.status Completed)
;;

let current t =
  match List.find t.steps ~f:(fun s -> Status.equal s.status In_progress) with
  | Some _ as step -> step
  | None -> List.find t.steps ~f:(fun s -> Status.equal s.status Pending)
;;
