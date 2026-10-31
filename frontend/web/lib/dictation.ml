open! Core

module Engine = struct
  type t =
    | Server
    | Browser
  [@@deriving sexp_of, equal]
end

type t =
  | Idle
  | Starting of Engine.t
  | Recording of
      { engine : Engine.t
      ; elapsed_ms : int
      ; level : float
      }
  | Transcribing
[@@deriving sexp_of, equal]

let active = function
  | Idle -> false
  | Starting _ | Recording _ | Transcribing -> true
;;

let elapsed ms =
  let s = Int.max 0 ms / 1000 in
  sprintf "%d:%02d" (s / 60) (s % 60)
;;

let max_ms = 10 * 60 * 1000
let no_provider e = String.is_substring e ~substring:"No transcription provider"

let insert ~draft ~cursor text =
  let text = String.strip text in
  if String.is_empty text
  then draft, cursor
  else (
    let cursor = Int.clamp_exn cursor ~min:0 ~max:(String.length draft) in
    let before = String.prefix draft cursor in
    let after = String.drop_prefix draft cursor in
    let gap_before =
      match String.get before (String.length before - 1) with
      | c when not (Char.is_whitespace c) -> " "
      | _ -> ""
      | exception _ -> ""
    in
    let gap_after =
      match String.get after 0 with
      | c when not (Char.is_whitespace c || String.mem ".,;:!?)]}" c) -> " "
      | _ -> ""
      | exception _ -> ""
    in
    let inserted = gap_before ^ text ^ gap_after in
    ( before ^ inserted ^ after
    , cursor + String.length gap_before + String.length text ))
;;
