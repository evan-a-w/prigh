open! Core
open! Import

module Item = struct
  type t =
    | Data of string
    | Close
  [@@deriving sexp_of]
end

module State = struct
  type t =
    | Open
    | Closed
    | Refused of string
    | Finished
end

type t =
  { items : Item.t Eio.Stream.t
  ; mutable state : State.t
  }

let create () = { items = Eio.Stream.create Int.max_value; state = Open }

let write t data ~close =
  match t.state with
  | Closed -> Or_error.error_string "its stdin is already closed"
  | Finished -> Or_error.error_string "it has already exited"
  | Refused reason -> Or_error.error_string reason
  | Open ->
    if not (String.is_empty data) then Eio.Stream.add t.items (Data data);
    if close
    then (
      t.state <- Closed;
      Eio.Stream.add t.items Close);
    Ok ()
;;

let take t = Eio.Stream.take t.items

let refuse t reason =
  match t.state with
  | Open | Closed -> t.state <- Refused reason
  | Refused _ | Finished -> ()
;;

let finish t = t.state <- Finished
