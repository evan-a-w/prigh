open! Core
open! Import

let config =
  { Note_store.Config.file = ".prigh/memory.json"
  ; id_prefix = "m"
  ; enabled_key = "memory.enabled"
  ; max_text = 2000
  ; max_title = 80
  ; noun = "note"
  ; a_noun = "a note"
  ; too_long = "keep only what must be remembered"
  }
;;

type t = Note_store.t

let path ~home = Filename.concat home config.file
let create ~home = Note_store.create config ~home
let subscribe = Note_store.subscribe
let enabled_key = config.enabled_key
let enabled = Note_store.enabled
let set_enabled = Note_store.set_enabled

module Note = Note_store.Note

module Saved = struct
  type t =
    | Added of Note.t
    | Replaced of Note.t
  [@@deriving sexp_of]
end

let max_text = config.max_text
let max_title = config.max_title
let max_notes = 200
let notes = Note_store.notes
let update = Note_store.update

let save t ?title ~now text =
  let%bind.Or_error title, text = Note_store.checked t ?title text in
  update t ~f:(fun file ->
    match title with
    | Some title
      when List.exists file.notes ~f:(fun note -> Note.has_title note title) ->
      let replaced = ref None in
      let notes =
        List.map file.notes ~f:(fun note ->
          if Note.has_title note title
          then (
            let note = { note with title = Some title; text; saved = now } in
            replaced := Some note;
            note)
          else note)
      in
      Ok ({ file with notes }, Saved.Replaced (Option.value_exn !replaced))
    | _ ->
      if List.length file.notes >= max_notes
      then
        Or_error.errorf "memory is full (%d notes): forget some first" max_notes
      else (
        let file, note = Note_store.append t file ~title ~now text in
        Ok (file, Saved.Added note)))
;;

let words s =
  String.split_on_chars (String.lowercase s) ~on:[ ' '; '\t'; '\n'; '\r' ]
  |> List.map ~f:(String.strip ~drop:(fun c -> not (Char.is_alphanum c)))
  |> List.filter ~f:(Fn.non String.is_empty)
;;

(* Whether [a] becomes [b] with at most one insertion, deletion or
   substitution. *)
let one_edit a b =
  let la = String.length a
  and lb = String.length b in
  if abs (la - lb) > 1
  then false
  else (
    let rec go i j edits =
      if edits > 1
      then false
      else if i = la || j = lb
      then edits + (la - i) + (lb - j) <= 1
      else if Char.equal a.[i] b.[j]
      then go (i + 1) (j + 1) edits
      else if la > lb
      then go (i + 1) j (edits + 1)
      else if lb > la
      then go i (j + 1) (edits + 1)
      else go (i + 1) (j + 1) (edits + 1)
    in
    go 0 0 0)
;;

let search notes query =
  let query = String.strip query in
  match
    List.find notes ~f:(fun (note : Note.t) ->
      String.Caseless.equal note.id query)
  with
  | Some note -> [ note ]
  | None ->
    let wanted = words query in
    let score (note : Note.t) =
      let haystack =
        String.lowercase (Option.value note.title ~default:"" ^ "\n" ^ note.text)
      in
      let note_words = words haystack in
      List.count wanted ~f:(fun word ->
        String.is_substring haystack ~substring:word
        || (String.length word >= 4
            && List.exists note_words ~f:(fun w ->
              String.length w >= 4 && one_edit word w)))
    in
    List.filter_map notes ~f:(fun note ->
      let score = score note in
      Option.some_if (score > 0) (score, note))
    |> List.stable_sort ~compare:(fun (s1, (n1 : Note.t)) (s2, n2) ->
      match Int.compare s2 s1 with
      | 0 -> Time_float.compare n2.saved n1.saved
      | c -> c)
    |> List.map ~f:snd
;;

let forget t key =
  let key = String.strip key in
  update t ~f:(fun file ->
    let matches (note : Note.t) =
      String.Caseless.equal note.id key || Note.has_title note key
    in
    match List.find file.notes ~f:matches with
    | Some note ->
      Ok
        ( { file with
            notes =
              List.filter file.notes ~f:(fun n ->
                not (String.equal n.id note.id))
          }
        , note )
    | None ->
      Or_error.errorf
        "no note %S%s"
        key
        (match file.notes with
         | [] -> ": memory is empty"
         | notes ->
           sprintf
             "; the notes are %s (recall shows them)"
             (String.concat
                ~sep:", "
                (List.map (List.take notes 20) ~f:(fun (n : Note.t) ->
                   match n.title with
                   | Some title -> sprintf "%s (%s)" n.id title
                   | None -> n.id))
              ^ if List.length notes > 20 then ", ..." else "")))
;;

let edit t ~id ?title ~now text =
  let%bind.Or_error title, text = Note_store.checked t ?title text in
  update t ~f:(fun file ->
    match
      List.find file.notes ~f:(fun (note : Note.t) -> String.equal note.id id)
    with
    | None -> Or_error.errorf "no note %S: it was forgotten" id
    | Some _ ->
      (match
         Option.bind title ~f:(fun title ->
           List.find file.notes ~f:(fun (note : Note.t) ->
             (not (String.equal note.id id)) && Note.has_title note title))
       with
       | Some other ->
         Or_error.errorf
           "note %s is titled %S already: choose another title"
           other.id
           (Option.value other.title ~default:"")
       | None ->
         let edited = { Note.id; title; text; saved = now } in
         Ok
           ( { file with
               notes =
                 List.map file.notes ~f:(fun (note : Note.t) ->
                   if String.equal note.id id then edited else note)
             }
           , edited )))
;;
