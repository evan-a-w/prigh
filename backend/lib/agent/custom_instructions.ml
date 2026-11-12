open! Core
open! Import

let config =
  { Note_store.Config.file = ".prigh/custom_instructions.json"
  ; id_prefix = "ci"
  ; enabled_key = "custom_instructions.enabled"
  ; max_text = 1000
  ; max_title = 80
  ; noun = "instruction"
  ; a_noun = "an instruction"
  ; too_long =
      "they are in the context of every session, so keep each to the point \
       (and what is only sometimes needed in memory)"
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

let max_text = config.max_text
let max_title = config.max_title
let max_notes = 50
let notes = Note_store.notes

let to_model ({ id; title; text; saved = _ } : Note.t) =
  { Custom_instruction.id; title; text }
;;

let for_model t =
  match notes t with
  | Ok notes -> List.map notes ~f:to_model
  | Error _ -> []
;;

let listed (notes : Note.t list) =
  match notes with
  | [] -> ": there are none"
  | notes ->
    sprintf
      "; they are %s"
      (String.concat ~sep:", " (List.map notes ~f:(fun n -> n.id)))
;;

let add t ?title ~now text =
  let%bind.Or_error title, text = Note_store.checked t ?title text in
  Note_store.update t ~f:(fun file ->
    if List.length file.notes >= max_notes
    then
      Or_error.errorf
        "there are %d custom instructions, the most there can be: remove or \
         merge some first"
        max_notes
    else Ok (Note_store.append t file ~title ~now text))
;;

let edit t ~id ?title ?text ~now () =
  let id = String.strip id in
  Note_store.update t ~f:(fun file ->
    match
      List.find file.notes ~f:(fun (note : Note.t) ->
        String.Caseless.equal note.id id)
    with
    | None ->
      Or_error.errorf "no custom instruction %S%s" id (listed file.notes)
    | Some note ->
      let title =
        match title with
        | None -> note.title
        | Some title -> Some title
      in
      let%map.Or_error title, text =
        Note_store.checked t ?title (Option.value text ~default:note.text)
      in
      let edited = { note with title; text; saved = now } in
      ( { file with
          notes =
            List.map file.notes ~f:(fun (n : Note.t) ->
              if String.equal n.id note.id then edited else n)
        }
      , edited ))
;;

let remove t key =
  let key = String.strip key in
  Note_store.update t ~f:(fun file ->
    match
      List.find file.notes ~f:(fun (note : Note.t) ->
        String.Caseless.equal note.id key || Note.has_title note key)
    with
    | Some note ->
      Ok
        ( { file with
            notes =
              List.filter file.notes ~f:(fun (n : Note.t) ->
                not (String.equal n.id note.id))
          }
        , note )
    | None ->
      Or_error.errorf "no custom instruction %S%s" key (listed file.notes))
;;
