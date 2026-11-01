open! Core

type t =
  | Island
  | Ring
  | Whisper
  | Border
  | Hub
  | Sidebar
[@@deriving sexp_of, equal, enumerate]

let default = Island

let name = function
  | Island -> "island"
  | Ring -> "ring"
  | Whisper -> "whisper"
  | Border -> "border"
  | Hub -> "hub"
  | Sidebar -> "sidebar"
;;

let of_name s =
  let s = String.lowercase (String.strip s) in
  List.find all ~f:(fun t -> String.equal (name t) s)
;;

let title = function
  | Island -> "Island"
  | Ring -> "Send ring"
  | Whisper -> "Status line"
  | Border -> "Border meter"
  | Hub -> "Model chip"
  | Sidebar -> "Sidebar"
;;

let describe = function
  | Island -> "a pill floating over the top of the chat"
  | Ring -> "a ring of context round send; thinking and mode by the model"
  | Whisper -> "one quiet line under the message box"
  | Border -> "the message box's edge fills with context"
  | Hub -> "the model chip shows the context; its menu has the rest"
  | Sidebar -> "a card at the sidebar's foot (a line under the box without it)"
;;
