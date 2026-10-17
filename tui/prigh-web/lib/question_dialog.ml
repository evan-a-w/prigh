open! Core
open! Import
module Form = Question_form

module Action = struct
  type t =
    | Up
    | Down
    | Prev
    | Next
    | Row of int
    | Choose of int
    | Toggle
    | Insert of string
    | Backspace
    | Set_own of string
    | Accept
    | Decline
  [@@deriving sexp_of, equal]
end

type t =
  { call_id : string
  ; form : Form.t
  }
[@@deriving sexp_of, equal]

let create (ask : Pending.Ask.t) =
  { call_id = ask.call_id
  ; form = Form.create ?deadline:ask.deadline ask.questions
  }
;;

let update t (action : Action.t) =
  let show form = `Show { t with form } in
  match action with
  | Up -> show (Form.up t.form)
  | Down -> show (Form.down t.form)
  | Prev -> show (Form.prev t.form)
  | Next -> show (Form.next t.form)
  | Row row -> show (Form.go_to t.form ~row)
  | Choose row ->
    let form = Form.go_to t.form ~row in
    show (if Form.on_own_answer form then form else Form.toggle form)
  | Toggle -> show (Form.toggle t.form)
  | Insert text -> show (Form.insert t.form text)
  | Backspace -> show (Form.backspace t.form)
  | Set_own text -> show (Form.set_own t.form text)
  | Accept ->
    (match Form.accept t.form with
     | `Next form -> show form
     | `Done answers -> `Answer answers)
  | Decline -> `Decline
;;

let answer_params t answers =
  [ "call_id", Json.str t.call_id
  ; ( "answers"
    , `Array (List.map answers ~f:(fun a -> `Array (List.map a ~f:Json.str))) )
  ]
;;

let decline_params t =
  [ "call_id", Json.str t.call_id; "decline", Json.bool true ]
;;

let focus t = if Form.on_own_answer t.form then "question-own" else "question"
