open! Core
open! Import

type t =
  { path : string
  ; mutex : Eio.Mutex.t
  }

let path ~home = Filename.concat home ".prigh/memory.json"
let create ~path = { path; mutex = Eio.Mutex.create () }

module Note = struct
  module Utc = struct
    type t = Time_float.t

    let sexp_of_t t = Sexp.Atom (Time_float.to_string_utc t)
  end

  type t =
    { id : string
    ; title : string option
    ; text : string
    ; saved : Utc.t
    }
  [@@deriving sexp_of]

  let to_json t : Json.t =
    `Object
      ([ "id", `String t.id ]
       @ Option.value_map t.title ~default:[] ~f:(fun title ->
         [ "title", `String title ])
       @ [ "text", `String t.text
         ; "saved", `String (Time_float.to_string_utc t.saved)
         ])
  ;;

  let of_json (json : Json.t) =
    let str key = Option.bind (Json.member key json) ~f:Json.string in
    match str "id", str "text" with
    | Some id, Some text ->
      let saved =
        Option.bind (str "saved") ~f:(fun s ->
          Option.try_with (fun () -> Time_float.of_string_with_utc_offset s))
        |> Option.value ~default:Time_float.epoch
      in
      Some { id; title = str "title"; text; saved }
    | _ -> None
  ;;

  let has_title t title =
    Option.exists t.title ~f:(String.Caseless.equal (String.strip title))
  ;;
end

module Saved = struct
  type t =
    | Added of Note.t
    | Replaced of Note.t
  [@@deriving sexp_of]
end

module File = struct
  type t =
    { next : int
    ; notes : Note.t list
    }

  let empty = { next = 1; notes = [] }
end

let max_text = 2000
let max_title = 80
let max_notes = 200

let read t =
  match In_channel.read_all t.path with
  | exception Sys_error _ -> Ok File.empty
  | data when String.is_empty (String.strip data) -> Ok File.empty
  | data ->
    let invalid why =
      Or_error.errorf "%s %s: fix it, or move it aside" t.path why
    in
    (match Json.parse data with
     | Error e ->
       invalid (sprintf "is not valid JSON (%s)" (Error.to_string_hum e))
     | Ok json ->
       (match Json.member "notes" json with
        | Some (`Array notes) ->
          let notes = List.filter_map notes ~f:Note.of_json in
          let next =
            Option.bind (Json.member "next" json) ~f:Json.int
            |> Option.value ~default:1
          in
          (* Never below an id in the file, so that ids are not reused. *)
          let next =
            List.fold notes ~init:next ~f:(fun next (note : Note.t) ->
              match
                Option.bind
                  (String.chop_prefix note.id ~prefix:"m")
                  ~f:(fun n -> Int.of_string_opt n)
              with
              | Some n -> Int.max next (n + 1)
              | None -> next)
          in
          Ok { File.next; notes }
        | _ -> invalid "must be an object with a \"notes\" array"))
;;

let write t (file : File.t) =
  let json : Json.t =
    `Object
      [ "next", `Number (Int.to_string file.next)
      ; "notes", `Array (List.map file.notes ~f:Note.to_json)
      ]
  in
  let tmp = t.path ^ ".tmp" in
  Out_channel.write_all tmp ~data:(Json.to_string_hum json ^ "\n");
  Core_unix.rename ~src:tmp ~dst:t.path
;;

(* Fibers of one process are serialised with the mutex, processes with the
   lock directory. *)
let update t ~f =
  Eio.Mutex.use_rw ~protect:false t.mutex
  @@ fun () ->
  Or_error.join
    (Or_error.try_with (fun () ->
       Core_unix.mkdir_p (Filename.dirname t.path);
       Lock_dir.with_lock
         ~file:t.path
         ~f:(fun () ->
           let open Or_error.Let_syntax in
           let%bind file = read t in
           let%map file, result = f file in
           write t file;
           result)
         ()))
;;

let notes t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Or_error.map (read t) ~f:(fun (file : File.t) -> file.notes))
;;

let save t ?title ~now text =
  let text = String.strip text in
  let title =
    Option.bind title ~f:(fun title ->
      let title = String.strip title in
      Option.some_if (not (String.is_empty title)) title)
  in
  if String.is_empty text
  then Or_error.error_string "the note's text is empty"
  else if String.length text > max_text
  then
    Or_error.errorf
      "a note is limited to %d bytes (this one has %d): keep only what must be \
       remembered"
      max_text
      (String.length text)
  else if Option.exists title ~f:(fun title -> String.length title > max_title)
  then
    Or_error.errorf "a title is limited to %d bytes: use a few words" max_title
  else
    update t ~f:(fun file ->
      match title with
      | Some title
        when List.exists file.notes ~f:(fun note -> Note.has_title note title)
        ->
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
          Or_error.errorf
            "memory is full (%d notes): forget some first"
            max_notes
        else (
          let note =
            { Note.id = sprintf "m%d" file.next; title; text; saved = now }
          in
          Ok
            ( { File.next = file.next + 1; notes = file.notes @ [ note ] }
            , Saved.Added note )))
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
