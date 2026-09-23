open! Core
module Key = Prigh_ui.Key

module Event = struct
  type t =
    { key : string
    ; code : string
    ; ctrl : bool
    ; alt : bool
    ; shift : bool
    ; meta : bool
    }
  [@@deriving sexp_of]
end

let named (key : string) : Key.Code.t option =
  match key with
  | "Enter" -> Some Enter
  | "Tab" -> Some Tab
  | "Escape" -> Some Escape
  | "Backspace" -> Some Backspace
  | "Delete" -> Some Delete
  | "Insert" -> Some Insert
  | "Home" -> Some Home
  | "End" -> Some End
  | "ArrowUp" -> Some Up
  | "ArrowDown" -> Some Down
  | "ArrowLeft" -> Some Left
  | "ArrowRight" -> Some Right
  | "PageUp" -> Some Page_up
  | "PageDown" -> Some Page_down
  | _ ->
    (match String.chop_prefix key ~prefix:"F" with
     | Some n ->
       (match Int.of_string_opt n with
        | Some n when n >= 1 && n <= 24 -> Some (Function n)
        | _ -> None)
     | None -> None)
;;

(* With Alt (Option on a Mac) or Ctrl held, [key] may be a dead key or a symbol;
   the physical key tells the letter. *)
let letter_of_code code =
  match String.chop_prefix code ~prefix:"Key" with
  | Some l when String.length l = 1 -> Some (String.lowercase l)
  | _ ->
    (match String.chop_prefix code ~prefix:"Digit" with
     | Some d when String.length d = 1 -> Some d
     | _ -> None)
;;

let single_scalar s =
  match Prigh_ui.Text_width.uchars s with
  | [ _ ] -> true
  | _ -> false
;;

(* Left to the browser: paste in all its spellings and reload. *)
let browser_owned (e : Event.t) =
  (e.ctrl && (not e.alt) && String.Caseless.equal e.key "v")
  || (e.shift && String.equal e.key "Insert")
  || (e.ctrl && String.equal e.code "KeyV")
  || (e.meta && not e.ctrl)
;;

let key (e : Event.t) : Key.t option =
  if browser_owned e
  then None
  else (
    let code : Key.Code.t option =
      match named e.key with
      | Some code -> Some code
      | None ->
        (match e.key with
         | " " -> Some (Char " ")
         | _ when (e.ctrl || e.alt) && Option.is_some (letter_of_code e.code) ->
           Some (Char (Option.value_exn (letter_of_code e.code)))
         | key when single_scalar key ->
           Some (Char (if e.ctrl then String.lowercase key else key))
         | _ -> None)
    in
    Option.map code ~f:(fun code ->
      { Key.code; ctrl = e.ctrl; alt = e.alt; shift = e.shift }))
;;
