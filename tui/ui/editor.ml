open! Core

module Position = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  { lines : string list
  ; cursor : Position.t
  ; history : string list (* most recent first *)
  ; history_index : int option
  ; draft : string
  }
[@@deriving sexp_of]

let empty =
  { lines = [ "" ]
  ; cursor = { line = 0; col = 0 }
  ; history = []
  ; history_index = None
  ; draft = ""
  }
;;

let text t = String.concat ~sep:"\n" t.lines
let lines t = t.lines
let position t = t.cursor
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

let current t = List.nth_exn t.lines t.cursor.line

let set_line t i line =
  { t with lines = List.mapi t.lines ~f:(fun j l -> if i = j then line else l) }
;;

let set_text t text =
  let lines =
    String.split
      (String.substr_replace_all text ~pattern:"\r" ~with_:"")
      ~on:'\n'
  in
  let last = List.length lines - 1 in
  { t with lines; cursor = { line = last; col = length (List.last_exn lines) } }
;;

let clear t = { (set_text t "") with history_index = None }

let insert t s =
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

let kill_to_end t =
  let before, _ = split_at (current t) t.cursor.col in
  set_line t t.cursor.line before
;;

let kill_line t = home (set_line t t.cursor.line "")

let kill_word t =
  let cs = chars (current t) in
  let before = List.take cs t.cursor.col
  and after = List.drop cs t.cursor.col in
  let rec drop_spaces = function
    | " " :: rest -> drop_spaces rest
    | l -> l
  in
  let rec drop_word = function
    | c :: rest when not (String.equal c " ") -> drop_word rest
    | l -> l
  in
  let kept = List.rev before |> drop_spaces |> drop_word |> List.rev in
  let t = set_line t t.cursor.line (of_chars kept ^ of_chars after) in
  { t with cursor = { t.cursor with col = List.length kept } }
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
