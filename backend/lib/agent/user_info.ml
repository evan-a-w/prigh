open! Core
open! Import

module Units = struct
  type t =
    | Metric
    | Imperial
    | Us_customary
  [@@deriving sexp_of, equal, enumerate]

  let to_wire = function
    | Metric -> "metric"
    | Imperial -> "imperial"
    | Us_customary -> "us_customary"
  ;;

  let describe = function
    | Metric -> "metric (°C, km, kg, L)"
    | Imperial -> "imperial (miles, stones and pounds, pints; °C)"
    | Us_customary -> "US customary (°F, miles, lb, gallons)"
  ;;

  let of_wire s =
    match String.lowercase (String.strip s) with
    | "metric" | "si" -> Some Metric
    | "imperial" | "uk" -> Some Imperial
    | "us_customary" | "us customary" | "us" | "customary" -> Some Us_customary
    | _ -> None
  ;;
end

module Date_format = struct
  type t =
    | Dmy
    | Mdy
    | Iso
  [@@deriving sexp_of, equal, enumerate]

  let to_wire = function
    | Dmy -> "dmy"
    | Mdy -> "mdy"
    | Iso -> "iso"
  ;;

  let describe = function
    | Dmy -> "day/month/year (25/12/2026)"
    | Mdy -> "month/day/year (12/25/2026)"
    | Iso -> "ISO 8601 (2026-12-25)"
  ;;

  let of_wire s =
    match String.lowercase (String.strip s) with
    | "dmy" | "d/m/y" | "dd/mm/yyyy" -> Some Dmy
    | "mdy" | "m/d/y" | "mm/dd/yyyy" -> Some Mdy
    | "iso" | "ymd" | "y-m-d" | "iso 8601" | "yyyy-mm-dd" -> Some Iso
    | _ -> None
  ;;
end

let max_short = 100
let max_background = 1000

(* As much as the free text of earlier versions held, so that it fits. *)
let max_anything_else = 4000
let max_total = 5000

let max_field = function
  | "background" -> max_background
  | "anything_else" -> max_anything_else
  | _ -> max_short
;;

module Form = struct
  type t =
    { name : string option
    ; country : string option
    ; time_zone : string option
    ; language : string option
    ; units : Units.t option
    ; date_format : Date_format.t option
    ; occupation : string option
    ; background : string option
    ; anything_else : string option
    }
  [@@deriving sexp_of, equal, fields ~getters]

  let empty =
    { name = None
    ; country = None
    ; time_zone = None
    ; language = None
    ; units = None
    ; date_format = None
    ; occupation = None
    ; background = None
    ; anything_else = None
    }
  ;;

  module Field = struct
    type form = t

    type t =
      | Text of
          { get : form -> string option
          ; set : form -> string option -> form
          }
      | Choice of
          { get : form -> string option (** the wire name *)
          ; set : form -> string -> form option
          }

    (* Wire name, label in the block, field: in the form's order. *)
    let all =
      let text get set = Text { get; set } in
      [ "name", "Name", text name (fun t name -> { t with name })
      ; ( "country"
        , "Country/region"
        , text country (fun t country -> { t with country }) )
      ; ( "time_zone"
        , "Time zone"
        , text time_zone (fun t time_zone -> { t with time_zone }) )
      ; ( "language"
        , "Language and spelling"
        , text language (fun t language -> { t with language }) )
      ; ( "units"
        , "Units"
        , Choice
            { get = (fun t -> Option.map t.units ~f:Units.to_wire)
            ; set =
                (fun t s ->
                  Option.map (Units.of_wire s) ~f:(fun u ->
                    { t with units = Some u }))
            } )
      ; ( "date_format"
        , "Date format"
        , Choice
            { get = (fun t -> Option.map t.date_format ~f:Date_format.to_wire)
            ; set =
                (fun t s ->
                  Option.map (Date_format.of_wire s) ~f:(fun d ->
                    { t with date_format = Some d }))
            } )
      ; ( "occupation"
        , "Occupation/role"
        , text occupation (fun t occupation -> { t with occupation }) )
      ; ( "background"
        , "Background and expertise"
        , text background (fun t background -> { t with background }) )
      ; ( "anything_else"
        , "Anything else"
        , text anything_else (fun t anything_else -> { t with anything_else }) )
      ]
    ;;
  end

  let fields = List.map Field.all ~f:fst3

  let nonempty s =
    let s = String.strip s in
    Option.some_if (not (String.is_empty s)) s
  ;;

  let to_json t : Json.t =
    `Object
      (List.filter_map Field.all ~f:(fun (key, _, field) ->
         let value =
           match field with
           | Text { get; _ } | Choice { get; _ } -> get t
         in
         Option.map value ~f:(fun v -> key, `String v)))
  ;;

  let unknown_choice key value =
    let choices =
      match key with
      | "units" -> List.map Units.all ~f:Units.to_wire
      | _ -> List.map Date_format.all ~f:Date_format.to_wire
    in
    Or_error.errorf
      "%s %S is not one of %s"
      key
      value
      (String.concat ~sep:", " choices)
  ;;

  let set_field t key value =
    match List.find Field.all ~f:(fun (k, _, _) -> String.equal k key) with
    | None ->
      Or_error.errorf
        "no field %S: the fields are %s"
        key
        (String.concat ~sep:", " fields)
    | Some (_, _, Text { set; _ }) -> Ok (set t (nonempty value))
    | Some (_, _, Choice { set; _ }) ->
      (match nonempty value with
       | None ->
         Ok
           (match key with
            | "units" -> { t with units = None }
            | _ -> { t with date_format = None })
       | Some value ->
         (match set t value with
          | Some t -> Ok t
          | None -> unknown_choice key value))
  ;;

  let of_json (json : Json.t) =
    List.fold_result Field.all ~init:empty ~f:(fun t (key, _, _) ->
      match Json.member key json with
      | None | Some `Null -> Ok t
      | Some (`String value) -> set_field t key value
      | Some _ -> Or_error.errorf "%s must be a string" key)
  ;;

  let value_line label value =
    match String.split_lines value with
    | [ line ] -> sprintf "- %s: %s" label line
    | lines ->
      String.concat
        ~sep:"\n"
        (sprintf "- %s:" label
         :: List.map lines ~f:(fun l ->
           if String.is_empty (String.strip l) then "" else "  " ^ l))
  ;;

  let render t =
    let lines =
      List.filter_map Field.all ~f:(fun (key, label, field) ->
        let value =
          match field with
          | Text { get; _ } -> get t
          | Choice { get = _; _ } ->
            (match key with
             | "units" -> Option.map t.units ~f:Units.describe
             | _ -> Option.map t.date_format ~f:Date_format.describe)
        in
        Option.map value ~f:(value_line label))
    in
    Option.some_if (not (List.is_empty lines)) (String.concat ~sep:"\n" lines)
  ;;

  let texts t =
    List.filter_map Field.all ~f:(fun (key, _, field) ->
      match field with
      | Text { get; _ } -> Option.map (get t) ~f:(fun v -> key, v)
      | Choice _ -> None)
  ;;
end

type t =
  { path : string
  ; legacy_path : string
  ; mutex : Eio.Mutex.t
  ; mutable subscribers : (unit -> unit) list
  }

let path ~home = Filename.concat home ".prigh/about_you.json"
let legacy_path ~home = Filename.concat home ".prigh/user_info.md"
let version = 1

let create ~home =
  { path = path ~home
  ; legacy_path = legacy_path ~home
  ; mutex = Eio.Mutex.create ()
  ; subscribers = []
  }
;;

let subscribe t ~f = t.subscribers <- f :: t.subscribers

let write t (form : Form.t) =
  if Form.equal form Form.empty
  then (
    match Core_unix.unlink t.path with
    | () -> ()
    | exception Core_unix.Unix_error (ENOENT, _, _) -> ())
  else (
    let fields =
      match Form.to_json form with
      | `Object fields -> fields
      | _ -> []
    in
    let json : Json.t =
      `Object (("version", `Number (Int.to_string version)) :: fields)
    in
    Core_unix.mkdir_p (Filename.dirname t.path);
    let tmp = t.path ^ ".tmp" in
    Out_channel.write_all tmp ~data:(Json.to_string_hum json ^ "\n");
    Core_unix.rename ~src:tmp ~dst:t.path)
;;

let read t =
  match In_channel.read_all t.path with
  | exception Sys_error _ -> None
  | data when String.is_empty (String.strip data) -> Some (Ok Form.empty)
  | data ->
    let invalid why =
      Or_error.errorf "%s %s: fix it, or move it aside" t.path why
    in
    Some
      (match Json.parse data with
       | Error e ->
         invalid (sprintf "is not valid JSON (%s)" (Error.to_string_hum e))
       | Ok json ->
         (match Option.bind (Json.member "version" json) ~f:Json.int with
          | Some v when v > version ->
            invalid
              (sprintf
                 "is version %d, from a newer prigh (this one reads %d)"
                 v
                 version)
          | _ ->
            (match Form.of_json json with
             | Ok form -> Ok form
             | Error e -> invalid (Error.to_string_hum e))))
;;

(* The free text of earlier versions, cut at the limit it had. *)
let legacy_text path =
  match In_channel.read_all path with
  | exception Sys_error _ -> None
  | text ->
    let text = String.strip (Utf8.sanitize text) in
    let text =
      if String.length text <= max_anything_else
      then text
      else
        String.strip
          (fst
             (Utf8.split_incomplete_suffix
                (String.prefix text max_anything_else)))
    in
    Some text
;;

let migrate t =
  Eio.Mutex.use_rw ~protect:false t.mutex
  @@ fun () ->
  Or_error.join
    (Or_error.try_with (fun () ->
       Core_unix.mkdir_p (Filename.dirname t.path);
       Lock_dir.with_lock
         ~file:t.path
         ~f:(fun () ->
           match read t with
           | Some form -> form
           | None ->
             (match legacy_text t.legacy_path with
              | None -> Ok Form.empty
              | Some text ->
                let form =
                  { Form.empty with anything_else = Form.nonempty text }
                in
                write t form;
                Core_unix.rename
                  ~src:t.legacy_path
                  ~dst:(t.legacy_path ^ ".migrated");
                Ok form))
         ()))
;;

let get t =
  match read t with
  | Some form -> form
  | None ->
    if Sys_unix.file_exists_exn t.legacy_path then migrate t else Ok Form.empty
;;

let text t = Option.bind (Result.ok (get t)) ~f:Form.render

let check (form : Form.t) =
  let texts = Form.texts form in
  match
    List.find texts ~f:(fun (key, v) -> String.length v > max_field key)
  with
  | Some (key, v) ->
    Or_error.errorf
      "%s is limited to %d bytes (this one has %d): shorten it, and keep what \
       is only sometimes needed in memory notes"
      key
      (max_field key)
      (String.length v)
  | None ->
    let total =
      List.sum (module Int) texts ~f:(fun (_, v) -> String.length v)
    in
    if total > max_total
    then
      Or_error.errorf
        "about you is limited to %d bytes in all (this has %d): shorten it, \
         and keep what is only sometimes needed in memory notes"
        max_total
        total
    else Ok ()
;;

let set t form =
  (* Stripped, as read back. *)
  let%bind.Or_error form = Form.of_json (Form.to_json form) in
  let%bind.Or_error () = check form in
  (* The free text of earlier versions is kept as a form or renamed, not
     left for a later [get] to bring back. *)
  ignore (get t : Form.t Or_error.t);
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex
    @@ fun () -> Or_error.try_with (fun () -> write t form)
  in
  if Or_error.is_ok result
  then List.iter (List.rev t.subscribers) ~f:(fun f -> f ());
  result
;;
