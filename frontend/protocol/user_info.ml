open! Core

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

  let of_wire s = List.find all ~f:(fun t -> String.equal (to_wire t) s)

  let short = function
    | Metric -> "Metric"
    | Imperial -> "Imperial"
    | Us_customary -> "US"
  ;;

  let label = function
    | Metric -> "Metric (°C, km, kg)"
    | Imperial -> "Imperial (miles, stone; °C)"
    | Us_customary -> "US customary (°F, miles, lb)"
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

  let of_wire s = List.find all ~f:(fun t -> String.equal (to_wire t) s)

  let short = function
    | Dmy -> "D/M/Y"
    | Mdy -> "M/D/Y"
    | Iso -> "ISO"
  ;;

  let label = function
    | Dmy -> "D/M/Y (25/12/2026)"
    | Mdy -> "M/D/Y (12/25/2026)"
    | Iso -> "ISO (2026-12-25)"
  ;;
end

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
  [@@deriving sexp_of, equal]

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

  let fields =
    [ "name", "what to call you"
    ; "country", "country or region"
    ; "time_zone", "IANA time zone, e.g. Australia/Sydney"
    ; "language", "language and spelling, e.g. English, Australian spelling"
    ; "units", "metric, imperial or us_customary"
    ; "date_format", "dmy, mdy or iso"
    ; "occupation", "occupation or role"
    ; "background", "background and expertise"
    ; "anything_else", "anything else the model should know"
    ]
  ;;

  let to_json t =
    let field key value = Option.map value ~f:(fun v -> key, Json.str v) in
    Json.obj
      (List.filter_opt
         [ field "name" t.name
         ; field "country" t.country
         ; field "time_zone" t.time_zone
         ; field "language" t.language
         ; field "units" (Option.map t.units ~f:Units.to_wire)
         ; field "date_format" (Option.map t.date_format ~f:Date_format.to_wire)
         ; field "occupation" t.occupation
         ; field "background" t.background
         ; field "anything_else" t.anything_else
         ])
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    let text key =
      let%map v = Json.string_opt_field j key in
      Option.bind v ~f:(fun v ->
        let v = String.strip v in
        Option.some_if (not (String.is_empty v)) v)
    in
    let choice key of_wire =
      match%bind text key with
      | None -> Ok None
      | Some v ->
        (match of_wire v with
         | Some c -> Ok (Some c)
         | None -> Or_error.errorf "unknown %s %S" key v)
    in
    let%bind name = text "name" in
    let%bind country = text "country" in
    let%bind time_zone = text "time_zone" in
    let%bind language = text "language" in
    let%bind units = choice "units" Units.of_wire in
    let%bind date_format = choice "date_format" Date_format.of_wire in
    let%bind occupation = text "occupation" in
    let%bind background = text "background" in
    let%map anything_else = text "anything_else" in
    { name
    ; country
    ; time_zone
    ; language
    ; units
    ; date_format
    ; occupation
    ; background
    ; anything_else
    }
  ;;
end

type t =
  { form : Form.t
  ; text : string
  ; max_short : int
  ; max_background : int
  ; max_anything_else : int
  ; max_total : int
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind form = Json.object_field j "form" >>= Form.of_json in
  let%bind text = Json.string_field j "text" in
  let%bind limits = Json.object_field j "limits" in
  let%bind max_short = Json.int_field limits "name" in
  let%bind max_background = Json.int_field limits "background" in
  let%bind max_anything_else = Json.int_field limits "anything_else" in
  let%map max_total = Json.int_field limits "total" in
  { form; text; max_short; max_background; max_anything_else; max_total }
;;

let set_params form = [ "form", Form.to_json form ]

let set_field_params ~field value =
  [ "field", Json.str field; "value", Json.str value ]
;;
