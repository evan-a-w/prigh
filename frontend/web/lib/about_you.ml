open! Core
open! Import

module Status = struct
  type t =
    | Unknown
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Field = struct
  type t =
    | Name
    | Country
    | Time_zone
    | Language
    | Units
    | Date_format
    | Occupation
    | Background
    | Anything_else
  [@@deriving sexp_of, equal, enumerate]

  let wire = function
    | Name -> "name"
    | Country -> "country"
    | Time_zone -> "time_zone"
    | Language -> "language"
    | Units -> "units"
    | Date_format -> "date_format"
    | Occupation -> "occupation"
    | Background -> "background"
    | Anything_else -> "anything_else"
  ;;

  let label = function
    | Name -> "Name"
    | Country -> "Country or region"
    | Time_zone -> "Time zone"
    | Language -> "Language and spelling"
    | Units -> "Units"
    | Date_format -> "Date format"
    | Occupation -> "Occupation or role"
    | Background -> "Background and expertise"
    | Anything_else -> "Anything else"
  ;;

  let placeholder = function
    | Name -> "What to call you"
    | Country -> "Australia"
    | Time_zone -> "Australia/Sydney"
    | Language -> "English, Australian spelling"
    | Units | Date_format -> ""
    | Occupation -> "Backend engineer"
    | Background -> "Undergrad pure maths and CS; years of OCaml"
    | Anything_else -> "Keep answers short, and ask before adding a dependency."
  ;;

  let choices = function
    | Units ->
      Some
        (List.map User_info.Units.all ~f:(fun u ->
           User_info.Units.(to_wire u, short u, label u)))
    | Date_format ->
      Some
        (List.map User_info.Date_format.all ~f:(fun d ->
           User_info.Date_format.(to_wire d, short d, label d)))
    | Name
    | Country
    | Time_zone
    | Language
    | Occupation
    | Background
    | Anything_else -> None
  ;;

  let multiline = function
    | Background | Anything_else -> true
    | Name | Country | Time_zone | Language | Units | Date_format | Occupation
      -> false
  ;;

  let get (f : User_info.Form.t) = function
    | Name -> f.name
    | Country -> f.country
    | Time_zone -> f.time_zone
    | Language -> f.language
    | Units -> Option.map f.units ~f:User_info.Units.to_wire
    | Date_format -> Option.map f.date_format ~f:User_info.Date_format.to_wire
    | Occupation -> f.occupation
    | Background -> f.background
    | Anything_else -> f.anything_else
  ;;

  (* Kept as typed (stripped when saved), so that typing a space works. *)
  let set (f : User_info.Form.t) t value =
    let v = Option.some_if (not (String.is_empty value)) value in
    match t with
    | Name -> { f with name = v }
    | Country -> { f with country = v }
    | Time_zone -> { f with time_zone = v }
    | Language -> { f with language = v }
    | Units -> { f with units = Option.bind v ~f:User_info.Units.of_wire }
    | Date_format ->
      { f with date_format = Option.bind v ~f:User_info.Date_format.of_wire }
    | Occupation -> { f with occupation = v }
    | Background -> { f with background = v }
    | Anything_else -> { f with anything_else = v }
  ;;
end

type t =
  { status : Status.t
  ; info : User_info.t
  ; draft : User_info.Form.t option
  ; saving : bool
  ; saved : bool
  ; elsewhere : bool
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { status = Unknown
  ; info =
      { form = User_info.Form.empty
      ; text = ""
      ; max_short = 0
      ; max_background = 0
      ; max_anything_else = 0
      ; max_total = 0
      }
  ; draft = None
  ; saving = false
  ; saved = false
  ; elsewhere = false
  ; error = None
  }
;;

let loading t =
  match t.status with
  | Loaded -> t
  | Unknown | Loading | Failed _ -> { t with status = Loading }
;;

let stripped (form : User_info.Form.t) =
  List.fold Field.all ~init:form ~f:(fun form field ->
    match Field.get form field with
    | None -> form
    | Some v -> Field.set form field (String.strip v))
;;

let same draft (info : User_info.t) =
  User_info.Form.equal (stripped draft) info.form
;;

let changed t (info : User_info.t) =
  let t = { t with status = Loaded; info } in
  match t.draft with
  | Some draft when not (same draft info) -> { t with elsewhere = not t.saving }
  | Some _ when t.saving -> t
  | Some _ | None -> { t with draft = None; elsewhere = false }
;;

let loaded t = function
  | Ok info -> changed t info
  | Error e -> { t with status = Failed e }
;;

let form t = Option.value t.draft ~default:t.info.form
let value t field = Option.value (Field.get (form t) field) ~default:""

let max t : Field.t -> int = function
  | Background -> t.info.max_background
  | Anything_else -> t.info.max_anything_else
  | Units | Date_format -> 0
  | Name | Country | Time_zone | Language | Occupation -> t.info.max_short
;;

let dirty t = Option.exists t.draft ~f:(fun draft -> not (same draft t.info))

let set t field value =
  { t with
    draft = Some (Field.set (form t) field value)
  ; saved = false
  ; error = None
  }
;;

let revert t = { t with draft = None; elsewhere = false; error = None }

let too_long t form =
  let sizes =
    List.filter_map Field.all ~f:(fun field ->
      match Field.choices field, Field.get form field with
      | None, Some v -> Some (field, String.length v)
      | Some _, _ | _, None -> None)
  in
  match
    List.find sizes ~f:(fun (field, size) ->
      max t field > 0 && size > max t field)
  with
  | Some (field, size) ->
    Some
      (sprintf
         "%s is %d bytes and the limit is %d: shorten it, and keep what is \
          only sometimes needed in memory notes."
         (Field.label field)
         size
         (max t field))
  | None ->
    let total = List.sum (module Int) sizes ~f:snd in
    if t.info.max_total > 0 && total > t.info.max_total
    then
      Some
        (sprintf
           "All together this is %d bytes and the limit is %d: shorten it, and \
            keep what is only sometimes needed in memory notes."
           total
           t.info.max_total)
    else None
;;

let save t =
  match t.draft with
  | None -> t, None
  | Some _ when t.saving -> t, None
  | Some draft ->
    let form = stripped draft in
    (match too_long t form with
     | Some error -> { t with error = Some error }, None
     | None ->
       ( { t with saving = true; error = None }
       , Some (Request.Method.Set_user_info { form }) ))
;;

let was_saved (t : t) = function
  | Ok (info : User_info.t) ->
    { status = Loaded
    ; info
    ; draft = None
    ; saving = false
    ; saved = true
    ; elsewhere = false
    ; error = None
    }
  | Error e -> { t with saving = false; error = Some e }
;;
