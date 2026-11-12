open! Core
open! Import

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

module Config = struct
  type t =
    { file : string
    ; id_prefix : string
    ; enabled_key : string
    ; max_text : int
    ; max_title : int
    ; noun : string
    ; a_noun : string
    ; too_long : string
    }
end

module File = struct
  type t =
    { next : int
    ; notes : Note.t list
    }

  let empty = { next = 1; notes = [] }
end

type t =
  { config : Config.t
  ; path : string
  ; prefs : User_prefs.t
  ; mutex : Eio.Mutex.t
  ; mutable subscribers : (unit -> unit) list
  }

let create (config : Config.t) ~home =
  { config
  ; path = Filename.concat home config.file
  ; prefs = User_prefs.create ~path:(User_prefs.path ~home)
  ; mutex = Eio.Mutex.create ()
  ; subscribers = []
  }
;;

let path t = t.path
let config t = t.config
let subscribe t ~f = t.subscribers <- f :: t.subscribers
let changed t = List.iter (List.rev t.subscribers) ~f:(fun f -> f ())

let enabled t =
  match User_prefs.get t.prefs ~keys:[ t.config.enabled_key ] with
  | Ok [ (_, `False) ] -> false
  | Ok _ | Error _ -> true
;;

let set_enabled t enabled =
  let%map.Or_error changes =
    User_prefs.set
      t.prefs
      [ (t.config.enabled_key, if enabled then None else Some `False) ]
  in
  if not (List.is_empty changes) then changed t
;;

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
                  (String.chop_prefix note.id ~prefix:t.config.id_prefix)
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
  let result =
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
  in
  if Or_error.is_ok result then changed t;
  result
;;

let notes t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Or_error.map (read t) ~f:(fun (file : File.t) -> file.notes))
;;

let checked t ?title text =
  let c = t.config in
  let text = String.strip text in
  let title =
    Option.bind title ~f:(fun title ->
      let title = String.strip title in
      Option.some_if (not (String.is_empty title)) title)
  in
  if String.is_empty text
  then Or_error.errorf "the %s's text is empty" c.noun
  else if String.length text > c.max_text
  then
    Or_error.errorf
      "%s is limited to %d bytes (this one has %d): %s"
      c.a_noun
      c.max_text
      (String.length text)
      c.too_long
  else if
    Option.exists title ~f:(fun title -> String.length title > c.max_title)
  then
    Or_error.errorf
      "a title is limited to %d bytes: use a few words"
      c.max_title
  else Ok (title, text)
;;

let append t (file : File.t) ~title ~now text =
  let note =
    { Note.id = sprintf "%s%d" t.config.id_prefix file.next
    ; title
    ; text
    ; saved = now
    }
  in
  { File.next = file.next + 1; notes = file.notes @ [ note ] }, note
;;
