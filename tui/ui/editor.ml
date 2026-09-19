open! Core

module Position = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving sexp_of, equal]

  let compare a b =
    match Int.compare a.line b.line with
    | 0 -> Int.compare a.col b.col
    | n -> n
  ;;

  let contains ~start ~stop p = compare start p <= 0 && compare stop p > 0
end

module Chip = struct
  type t =
    { start : Position.t
    ; stop : Position.t
    }
  [@@deriving sexp_of]

  let lines t = t.stop.line - t.start.line + 1
  let contains t p = Position.contains ~start:t.start ~stop:t.stop p

  (* Inclusive of the stop boundary, so an edit right after a paste dissolves
     the chip instead of leaving the inserted text hidden inside it. *)
  let touches t p =
    Position.compare t.start p <= 0 && Position.compare t.stop p >= 0
  ;;
end

module Snapshot = struct
  type t =
    { lines : string list
    ; cursor : Position.t
    }
  [@@deriving sexp_of]
end

module Yank = struct
  type t =
    { before : Snapshot.t
    ; index : int
    }
  [@@deriving sexp_of]
end

type t =
  { lines : string list
  ; cursor : Position.t
  ; history : string list (* most recent first *)
  ; history_index : int option
  ; draft : string
  ; kill_ring : string list (* most recent first *)
  ; undo_stack : Snapshot.t list (* most recent first *)
  ; undo_coalesce : bool
  ; chips : Chip.t list
  ; last_yank : Yank.t option
  }
[@@deriving sexp_of]

let max_kill = 20
let max_undo = 100
let chip_min_lines = 3

let empty =
  { lines = [ "" ]
  ; cursor = { line = 0; col = 0 }
  ; history = []
  ; history_index = None
  ; draft = ""
  ; kill_ring = []
  ; undo_stack = []
  ; undo_coalesce = false
  ; chips = []
  ; last_yank = None
  }
;;

let text t = String.concat ~sep:"\n" t.lines
let lines t = t.lines
let position t = t.cursor
let kill_ring t = t.kill_ring
let chips t = t.chips
let is_empty t = String.is_empty (text t)

(* Cursor columns count scalar values so that multi-byte input moves as one. *)
let chars s = List.map (Text_width.uchars s) ~f:fst
let of_chars cs = String.concat cs
let length s = List.length (chars s)

let split_at s col =
  let cs = chars s in
  let before = List.take cs col
  and after = List.drop cs col in
  of_chars before, of_chars after
;;

let slice s a b = of_chars (List.take (List.drop (chars s) a) (b - a))

let uchar_of_piece piece =
  let d = Stdlib.String.get_utf_8_uchar piece 0 in
  if Stdlib.Uchar.utf_decode_is_valid d
  then Some (Stdlib.Uchar.utf_decode_uchar d)
  else None
;;

let is_space piece =
  match uchar_of_piece piece with
  | Some u -> Uucp.White.is_white_space u
  | None -> false
;;

let is_word_char piece =
  match uchar_of_piece piece with
  | None -> false
  | Some u ->
    Uchar.equal u (Uchar.of_char '_')
    || Uucp.Alpha.is_alphabetic u
    ||
      (match Uucp.Num.numeric_type u with
      | `None -> false
      | _ -> true)
;;

let current t = List.nth_exn t.lines t.cursor.line

let set_line t i line =
  { t with lines = List.mapi t.lines ~f:(fun j l -> if i = j then line else l) }
;;

let snapshot t = { Snapshot.lines = t.lines; cursor = t.cursor }

let push_undo t ~coalesce =
  if coalesce && t.undo_coalesce
  then { t with last_yank = None }
  else
    { t with
      undo_stack = List.take (snapshot t :: t.undo_stack) max_undo
    ; undo_coalesce = coalesce
    ; last_yank = None
    }
;;

let dissolve_chip t pos =
  { t with chips = List.filter t.chips ~f:(fun c -> not (Chip.touches c pos)) }
;;

let push_kill t killed =
  { t with kill_ring = List.take (killed :: t.kill_ring) max_kill }
;;

let set_text t text =
  let lines =
    String.split
      (String.substr_replace_all text ~pattern:"\r" ~with_:"")
      ~on:'\n'
  in
  let last = List.length lines - 1 in
  { t with
    lines
  ; cursor = { line = last; col = length (List.last_exn lines) }
  ; chips = []
  ; last_yank = None
  }
;;

let clear t = { (set_text t "") with history_index = None }

let set_history t lines =
  { t with history = List.rev lines; history_index = None }
;;

(* [insert_raw] performs the edit without touching the undo stack; callers are
   responsible for snapshotting. *)
let insert_raw t s =
  let s = String.substr_replace_all s ~pattern:"\r\n" ~with_:"\n" in
  let s = String.substr_replace_all s ~pattern:"\r" ~with_:"\n" in
  let before, after = split_at (current t) t.cursor.col in
  match String.split s ~on:'\n' with
  | [ single ] ->
    let t = set_line t t.cursor.line (before ^ single ^ after) in
    { t with cursor = { t.cursor with col = t.cursor.col + length single } }
  | parts ->
    let n = List.length parts in
    let first = before ^ List.hd_exn parts in
    let last = List.last_exn parts in
    let middle = List.sub parts ~pos:1 ~len:(n - 2) in
    let head = List.take t.lines t.cursor.line in
    let tail = List.drop t.lines (t.cursor.line + 1) in
    { t with
      lines = head @ [ first ] @ middle @ [ last ^ after ] @ tail
    ; cursor = { line = t.cursor.line + n - 1; col = length last }
    }
;;

let insert t s =
  let coalesce =
    let cs = Text_width.uchars s in
    match cs with
    | [ (piece, _) ] -> not (is_space piece)
    | _ -> false
  in
  let t = dissolve_chip t t.cursor in
  let t = push_undo t ~coalesce in
  insert_raw t s
;;

let newline t = insert t "\n"

let join_with_previous t =
  let i = t.cursor.line in
  let prev = List.nth_exn t.lines (i - 1) in
  let cur = current t in
  let lines = List.filteri t.lines ~f:(fun j _ -> j <> i) in
  let t = { t with lines } in
  let t = set_line t (i - 1) (prev ^ cur) in
  { t with cursor = { line = i - 1; col = length prev } }
;;

let backspace t =
  let t = dissolve_chip t t.cursor in
  let t = push_undo t ~coalesce:false in
  if t.cursor.col > 0
  then (
    let before, after = split_at (current t) t.cursor.col in
    let before = of_chars (List.drop_last_exn (chars before)) in
    let t = set_line t t.cursor.line (before ^ after) in
    { t with cursor = { t.cursor with col = t.cursor.col - 1 } })
  else if t.cursor.line > 0
  then join_with_previous t
  else t
;;

let delete t =
  let t = dissolve_chip t t.cursor in
  let t = push_undo t ~coalesce:false in
  let line = current t in
  if t.cursor.col < length line
  then (
    let before, after = split_at line t.cursor.col in
    let after = of_chars (List.tl_exn (chars after)) in
    set_line t t.cursor.line (before ^ after))
  else if t.cursor.line < List.length t.lines - 1
  then (
    let next = { t with cursor = { line = t.cursor.line + 1; col = 0 } } in
    let joined = join_with_previous next in
    { joined with cursor = t.cursor })
  else t
;;

let left t =
  if t.cursor.col > 0
  then { t with cursor = { t.cursor with col = t.cursor.col - 1 } }
  else if t.cursor.line > 0
  then (
    let line = t.cursor.line - 1 in
    { t with cursor = { line; col = length (List.nth_exn t.lines line) } })
  else t
;;

let right t =
  if t.cursor.col < length (current t)
  then { t with cursor = { t.cursor with col = t.cursor.col + 1 } }
  else if t.cursor.line < List.length t.lines - 1
  then { t with cursor = { line = t.cursor.line + 1; col = 0 } }
  else t
;;

let move_line t line =
  let col = Int.min t.cursor.col (length (List.nth_exn t.lines line)) in
  { t with cursor = { line; col } }
;;

let up t =
  if t.cursor.line = 0 then None else Some (move_line t (t.cursor.line - 1))
;;

let down t =
  if t.cursor.line >= List.length t.lines - 1
  then None
  else Some (move_line t (t.cursor.line + 1))
;;

let home t = { t with cursor = { t.cursor with col = 0 } }
let end_ t = { t with cursor = { t.cursor with col = length (current t) } }
let goto t cursor = { t with cursor }

let word_left_col line col =
  let a = Array.of_list (chars line) in
  let i = ref (Int.min col (Array.length a)) in
  while !i > 0 && is_space a.(!i - 1) do
    decr i
  done;
  if !i > 0
  then (
    let word = is_word_char a.(!i - 1) in
    let continue j =
      j > 0
      && (not (is_space a.(j - 1)))
      && Bool.equal (is_word_char a.(j - 1)) word
    in
    while continue !i do
      decr i
    done);
  !i
;;

let word_right_col line col =
  let a = Array.of_list (chars line) in
  let n = Array.length a in
  let i = ref (Int.min col n) in
  while !i < n && is_space a.(!i) do
    incr i
  done;
  if !i < n
  then (
    let word = is_word_char a.(!i) in
    let continue j =
      j < n && (not (is_space a.(j))) && Bool.equal (is_word_char a.(j)) word
    in
    while continue !i do
      incr i
    done);
  !i
;;

let word_left t =
  if t.cursor.col = 0
  then
    if t.cursor.line = 0
    then t
    else end_ { t with cursor = { line = t.cursor.line - 1; col = 0 } }
  else
    { t with
      cursor = { t.cursor with col = word_left_col (current t) t.cursor.col }
    }
;;

let word_right t =
  let line = current t in
  if t.cursor.col >= length line
  then
    if t.cursor.line < List.length t.lines - 1
    then { t with cursor = { line = t.cursor.line + 1; col = 0 } }
    else t
  else
    { t with cursor = { t.cursor with col = word_right_col line t.cursor.col } }
;;

let word_start t =
  let a = Array.of_list (chars (current t)) in
  let i = ref (Int.min t.cursor.col (Array.length a)) in
  while !i > 0 && not (is_space a.(!i - 1)) do
    decr i
  done;
  { t.cursor with col = !i }
;;

let kill_to_end t =
  let before, after = split_at (current t) t.cursor.col in
  let t = dissolve_chip t t.cursor in
  let t = push_undo t ~coalesce:false in
  let t = push_kill t after in
  set_line t t.cursor.line before
;;

let kill_to_start t =
  let before, after = split_at (current t) t.cursor.col in
  let t = dissolve_chip t t.cursor in
  let t = push_undo t ~coalesce:false in
  let t = push_kill t before in
  let t = set_line t t.cursor.line after in
  { t with cursor = { t.cursor with col = 0 } }
;;

let word_delete t ~forward =
  let line = current t in
  let start, stop =
    if forward
    then t.cursor.col, word_right_col line t.cursor.col
    else word_left_col line t.cursor.col, t.cursor.col
  in
  if start = stop
  then t
  else (
    let killed = slice line start stop in
    let line = slice line 0 start ^ slice line stop (length line) in
    let t = dissolve_chip t t.cursor in
    let t = push_undo t ~coalesce:false in
    let t = push_kill t killed in
    let t = set_line t t.cursor.line line in
    { t with cursor = { t.cursor with col = start } })
;;

let kill_word t = word_delete t ~forward:false
let delete_word_forward t = word_delete t ~forward:true

let undo t =
  match t.undo_stack with
  | [] -> t
  | s :: rest ->
    { t with
      lines = s.lines
    ; cursor = s.cursor
    ; undo_stack = rest
    ; undo_coalesce = false
    ; last_yank = None
    ; chips = []
    }
;;

let yank t =
  match t.kill_ring with
  | [] -> t
  | text :: _ ->
    let before = snapshot t in
    let t = push_undo t ~coalesce:false in
    let t = insert_raw t text in
    { t with last_yank = Some { before; index = 0 } }
;;

let yank_pop t =
  match t.last_yank, t.kill_ring with
  | Some { before; index }, _ :: _ ->
    let next = (index + 1) mod List.length t.kill_ring in
    let base =
      { t with lines = before.lines; cursor = before.cursor; last_yank = None }
    in
    let base = insert_raw base (List.nth_exn t.kill_ring next) in
    { base with last_yank = Some { before; index = next } }
  | _ -> t
;;

let insert_paste t s =
  let s = String.substr_replace_all s ~pattern:"\r\n" ~with_:"\n" in
  let s = String.substr_replace_all s ~pattern:"\r" ~with_:"\n" in
  if String.is_empty s
  then t
  else (
    let start = t.cursor in
    let t = dissolve_chip t t.cursor in
    let t = push_undo t ~coalesce:false in
    let t = insert_raw t s in
    let stop = t.cursor in
    if List.length (String.split s ~on:'\n') > chip_min_lines
    then { t with chips = { Chip.start; stop } :: t.chips }
    else t)
;;

let submit ?(secret = false) t =
  let text = text t in
  let history =
    if secret || String.is_empty (String.strip text)
    then t.history
    else (
      match t.history with
      | last :: _ when String.equal last text -> t.history
      | _ -> text :: t.history)
  in
  text, clear { t with history }
;;

let history_prev t =
  match t.history with
  | [] -> None
  | _ ->
    let index, draft =
      match t.history_index with
      | None -> 0, text t
      | Some i -> i + 1, t.draft
    in
    if index >= List.length t.history
    then None
    else
      Some
        { (set_text t (List.nth_exn t.history index)) with
          history_index = Some index
        ; draft
        }
;;

let history_next t =
  match t.history_index with
  | None -> None
  | Some 0 -> Some { (set_text t t.draft) with history_index = None }
  | Some i ->
    Some
      { (set_text t (List.nth_exn t.history (i - 1))) with
        history_index = Some (i - 1)
      }
;;
